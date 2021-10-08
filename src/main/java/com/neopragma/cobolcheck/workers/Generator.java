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
import com.neopragma.cobolcheck.features.parser.KeywordExtractor;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;
import com.neopragma.cobolcheck.features.parser.TestSuiteParser;
import com.neopragma.cobolcheck.features.prepareMerge.PrepareMergeController;
import com.neopragma.cobolcheck.services.*;
import com.neopragma.cobolcheck.services.log.Log;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static com.neopragma.cobolcheck.services.filehelpers.PathHelper.endWithFileSeparator;
import static com.neopragma.cobolcheck.services.filehelpers.PathHelper.getMatchingDirectories;

/**
 * This class merges a Test Suite (a text file) with the source of the Cobol program to be tested,
 * producing a Cobol program with the unit test cases embedded in it.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Generator {


    private final TestSuiteParser testSuiteParser;
    private NumericFields numericFields;



    private InterpreterController interpreter;

    // Optionally replace identifier prefixes in cobol-check copybook lines and generated source lines,
    // in case of conflict with prefixes used in programs to be tested.
    // This is set in config.properties, cobolcheck.prefix entry.
    private String testCodePrefix;

    // The boilerplate copybooks for cobol-check test code inserted into Working-Storage and Procedure.
    // The names are a throwback to the proof-of-concept project, cobol-unit-test. Might change in future.
    private static final String workingStorageCopybookFilename = "CCHECKWS.CPY";
    private static final String procedureDivisionCopybookFilename = "CCHECKPD.CPY";
    // Comes from config.properties, cobolcheck.copybook.directory entry.
    private static String copybookDirectoryName = Constants.EMPTY_STRING;
    // Used to read source lines from cobol-check copybooks (as opposed to reading the program under test)
    private Reader secondarySourceReader;

    // Used to handle programs that don't have a Working-Storage Section
    private boolean workingStorageTestCodeHasBeenInserted = false;
    private final String workingStorageHeader = StringHelper.fixedLength("       WORKING-STORAGE SECTION.");

    // used while processing SELECT statements in the program under test
    String fileIdentifier = Constants.EMPTY_STRING;
    boolean expectFileIdentifier;

    // Flags to keep track of context while reading input source files.
    // We want to make a single pass of all inputs, so we need to know what we are looking for at any given point.
    private boolean readingDataDivision;//
    private boolean readingFileControl;//
    private boolean readingFileSection;//
    private boolean skipThisLine;
    private boolean expectFileStatusFieldName;
    private boolean processingFD;//
    private boolean processing01ItemUnderFD;//
    private boolean processingCopyStatement;//
    private boolean processingProcedureDivision;//
    private boolean processingBatchFileIOStatement;
    private boolean commentThisLine;
    private boolean previousLineContainedOnlyAPeriod;//

    public Generator() {
        testSuiteParser = new TestSuiteParser(new KeywordExtractor());
        numericFields = new NumericFields();
        testCodePrefix = Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX);
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
        // all test suites are located under this directory
        String testSuiteDirectory = Config.getTestSuiteDirectoryPathString();
        testSuiteDirectory = endWithFileSeparator(testSuiteDirectory);

        // Find test subdirectories that match program names
        List<String> matchingDirectories;
        Path path = Paths.get(programName);
        programName = path.getFileName().toString();
        try{
            matchingDirectories = getMatchingDirectories(programName, testSuiteDirectory);
        } catch (IOException ioException) {
            throw new PossibleInternalLogicErrorException(Messages.get("ERR019", programName));
        }


        for (String matchingDirectory : matchingDirectories) {
            Reader sourceReader = PrepareMergeController.getSourceReader(programName);
            Reader testSuiteReader = PrepareMergeController.getTestSuiteReader(matchingDirectory, testFileNames);
            Writer testSourceWriter = PrepareMergeController.getTestSourceWriter(programName);
            String testSourceOutPath = PrepareMergeController.getTestSourceOutPath();

            Log.debug("Initializer.runTestSuites() testSourceOutPath: <" + testSourceOutPath + ">");

            mergeTestSuitesIntoTheTestProgram(sourceReader, testSuiteReader, testSourceWriter, programName);
        }
    }

    /**
     * Merges source input and test suites into a single file
     *
     * @param sourceReader - For reading the cobol source.
     * @param testSuiteReader - For reading the test suites
     * @param testSourceWriter - For writing the merged output test file.
     * @param programName - The name of the cobol source program.
     */
    void mergeTestSuitesIntoTheTestProgram(Reader sourceReader, Reader testSuiteReader,
                                           Writer testSourceWriter, String programName) {
        mergeTestSuite(testSuiteReader, sourceReader, testSourceWriter);

        try {
            testSourceWriter.close();
        } catch (IOException closeTestSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR017", programName));
        }
    }

    /**
     * Merge test code with the program under test to produce a Cobol source program
     * that can be compiled and executed to run the test suite.
     *
     * @param testSuite (Reader) Test cases
     * @param cobolSourceIn (Reader) Source of Cobol program under test
     * @param testSourceOut (Writer) Cobol source with test cases merged into program under test
     * @return (Writer) Same Writer object as passed in, populated with Cobol source lines
     */
    public Writer mergeTestSuite(Reader testSuite, Reader cobolSourceIn, Writer testSourceOut) {

        if (testSuite == null) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        BufferedReader testSuiteReader
                = new BufferedReader(testSuite);
        if (cobolSourceIn == null) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        interpreter = new InterpreterController(new BufferedReader(cobolSourceIn));
        String sourceLine;
        boolean emptyInputStream = true;
        try {
            while ((sourceLine = interpreter.interpretNextLine()) != null) {
                emptyInputStream = false;

                processingBeforeEchoingSourceLineToOutput( sourceLine, testSourceOut);

                if (interpreter.shouldCurrentLineBeParsed()) {
                    if (interpreter.hasStatementBeenRead()){
                        for(String line : interpreter.getCurrentStatement()){
                            if (interpreter.shouldCurrentLineBeCommentedOut()){
                                line = "      *" + line.substring(6 + 1);
                            }
                            testSourceOut.write(line);
                        }

                    }
                    else {
                        testSourceOut.write(sourceLine);
                    }

                }

                processingAfterEchoingSourceLineToOutput(testSuiteReader, testSourceOut);
            }
            interpreter.closeReader();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
            catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
        if (emptyInputStream) {
            throw new PossibleInternalLogicErrorException(Messages.get("ERR007"));
        }
        return testSourceOut;
    }

    /**
     * processingBeforeEchoingSourceLineToOutput
     * Perform appropriate processing of the current input line from the program under test prior to echoing that
     * line to the test program (that is, the copy of the program under test that has test code injected into it).
     *
     * @param sourceLine - the current source line
     * @param testSourceOut - the writer attached to the test program being generated
     * @throws IOException - pass any IOExceptions up to the caller
     */
    private void processingBeforeEchoingSourceLineToOutput(
            String sourceLine,
            Writer testSourceOut) throws IOException {

        if (interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)) {
            if (!workingStorageTestCodeHasBeenInserted) {
                testSourceOut.write(workingStorageHeader);
                insertWorkingStorageTestCode(testSourceOut);
            }
        }
    }

    /**
     * Perform appropriate processing after echoing (or skipping) the current source line from the program under test.
     *
     * @param testSuiteReader - reader attached to the user-written test suite
     * @param testSourceOut - writer attached to the test program being generated
     * @throws IOException - pass any IOExceptions to the caller
     */
    private void processingAfterEchoingSourceLineToOutput(
            BufferedReader testSuiteReader,
            Writer testSourceOut) throws IOException {

        if (interpreter.currentLineContains(Constants.WORKING_STORAGE_SECTION)) {
            insertWorkingStorageTestCode(testSourceOut);
        }

        if (interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)) {
            insertProcedureDivisionTestCode(testSuiteReader, testSourceOut);
        }
    }

    /**
     * processFileSectionSource
     * Called for each source line read from the program under test while processing the File Section of the
     * Data Division. We capture source statements that define record layouts so that we can copy them into
     * the Working-Storage Section later. These statements may use optional Cobol keywords and they may be
     * coded on multiple source lines. We also need to expand any copybooks referenced in this part of the
     * source, in case data items in the copybooks are referenced by user-written test cases.
     *
     * @param tokens - tokens extracted from source line.
     * @param sourceLine - original source line.
     */


    /**
     * Insert test code into the Working-Storage Section of the test program being generated.
     *
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void insertWorkingStorageTestCode(Writer testSourceOut) throws IOException {
        // If this program had File Section source that we need to move to Working-Storage, inject the lines here.
        List<String> fileSectionStatements = interpreter.getFileSectionStatements();
        if (fileSectionStatements != null) {
            for (String line : fileSectionStatements) {
                testSourceOut.write(StringHelper.fixedLength(line));
            }
        }

        // Inject boilerplate test code from cobol-check Working-Storage copybook
        insertSecondarySourceIntoTestSource(workingStorageCopybookFilename, testSourceOut);
        workingStorageTestCodeHasBeenInserted = true;
    }

    /**
     * Insert test code into the Procedure Division of the test program being generated.
     *
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void insertProcedureDivisionTestCode(
            BufferedReader testSuiteReader,
            Writer testSourceOut) throws IOException {
        // Inject test initialization statement
        testSuiteParser.insertTestInitializationLineIntoTestSource(testSourceOut);

        // Parse the concatenated test suite and insert generated Cobol test statements
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);

        // Inject boilerplate test code from cobol-check Procedure Division copybook
        insertSecondarySourceIntoTestSource(procedureDivisionCopybookFilename, testSourceOut);

    }

    /**
     * Inject source statements from a secondary source (not the program under test) into the test program
     * being generated. Secondary sources are the cobol-check boilerplate copybooks, one for Working-Storage
     * and one for Procedure Division.
     *
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void insertSecondarySourceIntoTestSource(String copybookFilename, Writer testSourceOut) throws IOException {
        InputStream is = this.getClass().getResourceAsStream(Constants.COBOLCHECK_COPYBOOK_DIRECTORY + copybookFilename);
        BufferedReader secondarySourceBufferedReader = new BufferedReader(new InputStreamReader(is));
        String secondarySourceLine = Constants.EMPTY_STRING;
        while ((secondarySourceLine = secondarySourceBufferedReader.readLine()) != null) {
            secondarySourceLine = secondarySourceLine
                    .replaceAll(Constants.TEST_CODE_PREFIX_PLACEHOLDER, testCodePrefix);
            testSourceOut.write(StringHelper.fixedLength(secondarySourceLine));
        }
        secondarySourceBufferedReader.close();
    }

    /**
     * Enclose a value in quotation marks.
     *
     * @param value - original string
     * @return - quoted string
     */
    String quoted(String value) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(Constants.QUOTE);
        buffer.append(value);
        buffer.append(Constants.QUOTE);
        return buffer.toString();
    }


}
