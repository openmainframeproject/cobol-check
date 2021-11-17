/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck.workers;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.interpreter.InterpreterController;
import com.neopragma.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import com.neopragma.cobolcheck.features.writer.WriterController;
import com.neopragma.cobolcheck.features.prepareMerge.PrepareMergeController;
import com.neopragma.cobolcheck.services.*;
import com.neopragma.cobolcheck.services.log.Log;

import java.io.*;
import java.util.*;

/**
 * This class merges a Test Suite (a text file) with the source of the Cobol program to be tested,
 * producing a Cobol program with the unit test cases embedded in it.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Generator {

    private InterpreterController interpreter;
    private WriterController writerController;
    private TestSuiteParserController testSuiteParserController;

    private boolean waitingForMockedComponentToEnd = false;

    List<String> matchingTestDirectories;

    public Generator() {

    }

    /**
     * Finds any test-directory that matches the given program name. For each of these directories, all test
     * files are concatenated into one and the source program are merged with all the test-suites and cobol-
     * check's boilerplate copybooks into one test-program.
     * @param programName The name of the source program.
     * @param testFileNames The names of every test for the source program, separated by a space.
     */
    public void prepareAndRunMerge(String programName, String testFileNames) {
        matchingTestDirectories = PrepareMergeController.getMatchingTestDirectoriesForProgram(programName);
        for (String matchingDirectory : matchingTestDirectories) {

            Reader sourceReader = PrepareMergeController.getSourceReader(programName);
            interpreter = new InterpreterController(new BufferedReader(sourceReader));

            testSuiteParserController = new TestSuiteParserController(testFileNames);
            testSuiteParserController.concatenateTestSuites(matchingDirectory);

            Writer testSourceWriter = PrepareMergeController.getTestSourceWriter(programName);
            writerController = new WriterController(testSourceWriter);

            String testSourceOutPath = PrepareMergeController.getTestSourceOutPath();
            Log.debug("Initializer.runTestSuites() testSourceOutPath: <" + testSourceOutPath + ">");

            mergeTestSuite();

            closeReadersAndWriters(programName);
        }
    }

    /**
     * Handles the merge of the source program, the test-suites and cobol-check's boilerplate copybooks.
     * This is done by reading the source file line by line, and writing the appropriate lines to the
     * output file. Each line is interpreted, in order to know if, it should be commented out, ignored
     * or if the test-suite or boilerplate code should be inserted.
     */
    private void mergeTestSuite() {
        String sourceLine;
        try {
            while ((sourceLine = interpreter.interpretNextLine()) != null) {
                processingBeforeEchoingSourceLineToOutput();
                sourceLine = tryInsertEndEvaluateAtMockedCompomentEnd(sourceLine);

                writeToSource(sourceLine);

                processingAfterEchoingSourceLineToOutput();
            }
            testSuiteParserController.logUnusedMocks();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
        catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
    }

    /**
     * processingBeforeEchoingSourceLineToOutput
     * Perform appropriate processing of the current input line from the program under test prior to echoing that
     * line to the test program (that is, the copy of the program under test that has test code injected into it).
     *
     * @throws IOException - pass any IOExceptions up to the caller
     */
    private void processingBeforeEchoingSourceLineToOutput() throws IOException {

        if (interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)) {
            if (!testSuiteParserController.hasWorkingStorageTestCodeHasBeenInserted()) {
                writerController.writeLine(testSuiteParserController.getWorkingStorageHeader());

                writerController.writeLines(testSuiteParserController.getWorkingStorageTestCode(
                        interpreter.getFileSectionStatements()));
            }
        }
    }

    private String tryInsertEndEvaluateAtMockedCompomentEnd(String sourceLine) throws IOException {
        if (waitingForMockedComponentToEnd){
            if (interpreter.doesCurrentLineEndCurrentComponent()){
                if (interpreter.shouldEndEvaluateBeInsertedBeforeLine(sourceLine)){
                    waitingForMockedComponentToEnd = false;
                    writerController.writeLine(testSuiteParserController.getEndEvaluateLine());
                }
                else {
                    return sourceLine.replace(".", "");
                }
            }
        }
        return sourceLine;
    }

    /**
     * Writes the given line to the output file. If a multiline statement has been read, this will
     * be written instead (this statement will include the line). Comments out lines and statements
     * when needed.
     *
     * @param sourceLine - The line to write
     */
    private void writeToSource(String sourceLine) throws IOException {
        if (interpreter.shouldCurrentLineBeParsed()) {

            if (interpreter.hasStatementBeenRead()){
                if (interpreter.shouldCurrentStatementBeCommentedOut()){
                    writerController.writeCommentedLines(interpreter.getCurrentStatement());
                }
                else {
                    writerController.writeLines(interpreter.getCurrentStatement());
                }
            }

            else {
                if (interpreter.shouldCurrentLineBeCommentedOut()){
                    writerController.writeCommentedLine(sourceLine);
                }
                else {
                    writerController.writeLine(sourceLine);
                }
            }
        }
    }

    /**
     * Perform appropriate processing after echoing (or skipping) the current source line from the program under test.
     *
     * @throws IOException - pass any IOExceptions to the caller
     */
    private void processingAfterEchoingSourceLineToOutput() throws IOException {

        if (interpreter.currentLineContains(Constants.WORKING_STORAGE_SECTION)) {
            testSuiteParserController.parseTestSuites(interpreter.getNumericFields());
            writerController.writeLines(testSuiteParserController.getWorkingStorageTestCode(
                    interpreter.getFileSectionStatements()));
        }

        if (interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)) {
            writerController.writeLines(testSuiteParserController.getProcedureDivisionTestCode());
        }

        if (interpreter.isCurrentComponentMockable()){
            String identifier = interpreter.getPossibleMockIdentifier();
            String type = interpreter.getPossibleMockType();
            if (testSuiteParserController.mockExistsFor(identifier, type)){
                writerController.writeLines(testSuiteParserController.generateMockPerformCalls(identifier, type));
                waitingForMockedComponentToEnd = true;
            }
        }
    }

    private void closeReadersAndWriters(String programName) {
        interpreter.closeReader();
        testSuiteParserController.closeTestSuiteReader();
        writerController.closeWriter(programName);
    }

}
