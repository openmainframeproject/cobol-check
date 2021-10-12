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
import com.neopragma.cobolcheck.exceptions.TestSuiteCouldNotBeReadException;
import com.neopragma.cobolcheck.features.concatenator.ConcatenatorController;
import com.neopragma.cobolcheck.features.concatenator.TestSuiteConcatenator;
import com.neopragma.cobolcheck.features.interpreter.InterpreterController;
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
    private ConcatenatorController concatenatorController;

    List<String> matchingTestDirectories;

    public Generator() {

    }

    /**
     * For each program name specified in command-line option --programs, walk the directory tree under
     * test.suite.directory (from config) to find subdirectories that match the program name, and then pick up
     * all the testsuite files there (or the ones that match the specifications in command-line option --tests)
     * and concatenate them into a single testsuite source. For each program, invoke the Generator to merge the test
     * code into the program under test to produce a test program. Finally, launch an OS process to compile the test
     * program and execute it.
     */
    public void prepareAndRunMerge(String programName, String testFileNames) {
        matchingTestDirectories = PrepareMergeController.getMatchingTestDirectoriesForProgram(programName);
        for (String matchingDirectory : matchingTestDirectories) {

            Reader sourceReader = PrepareMergeController.getSourceReader(programName);
            interpreter = new InterpreterController(new BufferedReader(sourceReader));

            concatenatorController = new ConcatenatorController(testFileNames);
            Reader testSuiteReader = concatenatorController.concatenateTestSuites(matchingDirectory);

            Writer testSourceWriter = PrepareMergeController.getTestSourceWriter(programName);
            writerController = new WriterController(testSourceWriter, testSuiteReader);

            String testSourceOutPath = PrepareMergeController.getTestSourceOutPath();
            Log.debug("Initializer.runTestSuites() testSourceOutPath: <" + testSourceOutPath + ">");

            mergeTestSuite();

            closeReadersAndWriters(programName);

        }
    }

    /**
     * Merge test code with the program under test to produce a Cobol source program
     * that can be compiled and executed to run the test suite.
     *
     * @return (Writer) Same Writer object as passed in, populated with Cobol source lines
     */
    private void mergeTestSuite() {
        String sourceLine;
        boolean emptyInputStream = true;
        try {
            while ((sourceLine = interpreter.interpretNextLine()) != null) {
                emptyInputStream = false;

                processingBeforeEchoingSourceLineToOutput();

                writeToSource(sourceLine);

                processingAfterEchoingSourceLineToOutput();
            }
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
        catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
        if (emptyInputStream) {
            throw new PossibleInternalLogicErrorException(Messages.get("ERR007"));
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
            if (!writerController.hasWorkingStorageTestCodeHasBeenInserted()) {
                writerController.insertWorkingStorageHeader();
                writerController.insertWorkingStorageTestCode(interpreter.getFileSectionStatements());
            }
        }
    }

    private void writeToSource(String sourceLine) throws IOException {
        if (interpreter.shouldCurrentLineBeParsed()) {

            if (interpreter.hasStatementBeenRead()){
                if (interpreter.shouldCurrentStatementBeCommentedOut()){
                    writerController.writeCommentedStatement(interpreter.getCurrentStatement());
                }
                else {
                    writerController.writeStatement(interpreter.getCurrentStatement());
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
            writerController.insertWorkingStorageTestCode(interpreter.getFileSectionStatements());
        }

        if (interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)) {
            writerController.insertProcedureDivisionTestCode(interpreter.getNumericFields());
        }
    }

    private void closeReadersAndWriters(String programName) {
        interpreter.closeReader();
        writerController.closeTestSuiteReader();
        writerController.closeWriter(programName);
    }

}
