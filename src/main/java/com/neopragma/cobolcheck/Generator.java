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
package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.exceptions.TestSuiteCouldNotBeReadException;

import java.io.*;
import java.util.*;

/**
 * This class merges a Test Suite (a text file) with the source of the Cobol program to be tested,
 * producing a Cobol program with the unit test cases embedded in it.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Generator implements Constants, StringHelper {
    //TODO simplify this:the injected Config object contains an instance of Messages; no need for another one
    private final Messages messages;
    private final Config config;
    private final TokenExtractor tokenExtractor;
    private final KeywordExtractor keywordExtractor;

    private final State state = new State();

    // All lines from original Environment Division / Input-Output Section / File Control
    private List<String> fileControlStatements;

    // All lines from original Data Division / File Section
    private List<String> fileSectionStatements;

    // Internal file identifiers and status field names
    private Map<String, String> fileIdentifiersAndStatuses;

    // Tokens collected from COPY statements that may span multiple lines
    private List<String> copyTokens;

    // Special values to look for in the source of the program under test
    private static final String IDENTIFICATION_DIVISION = "IDENTIFICATION DIVISION";
    private static final String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION";
    private static final String CONFIGURATION_SECTION = "CONFIGURATION SECTION";
    private static final String INPUT_OUTPUT_SECTION = "INPUT-OUTPUT SECTION";
    private static final String FILE_CONTROL = "FILE-CONTROL";
    private static final String DATA_DIVISION = "DATA DIVISION";
    private static final String PROCEDURE_DIVISION = "PROCEDURE DIVISION";
    private static final String FILE_SECTION = "FILE SECTION";
    private static final String LOCAL_STORAGE_SECTION = "LOCAL-STORAGE SECTION";
    private static final String LINKAGE_SECTION = "LINKAGE SECTION";
    private static final String WORKING_STORAGE_SECTION = "WORKING-STORAGE SECTION";
    private static final String SELECT_TOKEN = "SELECT";
    private static final String FILE_STATUS_TOKEN = "FILE STATUS";
    private static final String IS_TOKEN = "IS";
    private static final String FD_TOKEN = "FD";
    private static final String LEVEL_01_TOKEN = "01";
    private static final String COPY_TOKEN = "COPY";

    // Used for handling source lines from copybooks that may not have the standard 80-byte length
    private static final int minimumMeaningfulSourceLineLength = 7;
    private static final int commentIndicatorOffset = 6;
    private static final char commentIndicator = '*';

    // The boilerplate copybooks for cobol-check test code inserted into Working-Storage and Procedure.
    // The names are a throwback to the proof-of-concept project, cobol-unit-test. Might change in future.
    private static final String workingStorageCopybookFilename = "ZUTZCWS.CPY";
    private static final String procedureDivisionCopybookFilename = "ZUTZCPD.CPY";
    // Comes from config.properties, cobolcheck.copybook.directory entry.
    private static String copybookDirectoryName = EMPTY_STRING;
    // Used to read source lines from cobol-check copybooks (as opposed to reading the program under test)
    private Reader secondarySourceReader;
    // Optionally replace identifier prefixes in cobol-check copybook lines and generated source lines,
    // in case of conflict with prefixes used in programs to be tested.
    // This is set in config.properties, cobolcheck.prefix entry.
    private String testCodePrefix;

    // Used to handle programs that don't have a Working-Storage Section
    private boolean workingStorageTestCodeHasBeenInserted = false;
    private final String workingStorageHeader = fixedLength("       WORKING-STORAGE SECTION.");

    // Used when parsing and concatenating user-written test suites
    private KeywordAction nextAction = KeywordAction.NONE;
    private String currentTestSuiteName = EMPTY_STRING;
    private String currentTestCaseName = EMPTY_STRING;

    // used while processing SELECT statements in the program under test
    String fileIdentifier = EMPTY_STRING;
    boolean expectFileIdentifier;

    // Flags to keep track of context while reading input source files.
    // We want to make a single pass of all inputs, so we need to know what we are looking for at any given point.
    private List<String> testSuiteTokens;
    private boolean emptyTestSuite;
    private boolean cobolStatementInProgress;
    private boolean expectInProgress;
    private boolean toBeInProgress;
    private boolean alphanumericLiteralCompare;
    private boolean numericLiteralCompare;
    private boolean boolean88LevelCompare;
    private boolean expectTestsuiteName;
    private boolean expectTestcaseName;
    private boolean readingFileControl;
    private boolean readingFileSection;
    private boolean skipThisLine;
    private String fieldNameForExpect;
    private String expectedValueToCompare;
    private boolean expectFileStatusFieldName;
    private boolean processingFD;
    private boolean processing01ItemUnderFD;
    private boolean processingCopyStatement;

    // Lines inserted into the test program
    private static final String COBOL_PERFORM_UT_INITIALIZE =
            "           PERFORM %sINITIALIZE";
    private static final String COBOL_DISPLAY_SPACE =
            "           DISPLAY SPACE                                                        ";
    private static final String COBOL_DISPLAY_TESTSUITE =
            "           DISPLAY \"TESTSUITE:\"                                                 ";
    private static final String COBOL_DISPLAY_NAME =
            "           DISPLAY %s";
    private static final String COBOL_STORE_TESTCASE_NAME_1 =
            "           MOVE %s";
    private static final String COBOL_STORE_TESTCASE_NAME_2 =
            "               TO %sTEST-CASE-NAME";
    private static final String COBOL_PERFORM_UT_BEFORE =
            "           PERFORM %sBEFORE";
    private static final String COBOL_INCREMENT_TEST_CASE_COUNT =
            "           ADD 1 TO %sTEST-CASE-COUNT";
    private static final String COBOL_SET_UT_NORMAL_COMPARE =
            "           SET %1$sNORMAL-COMPARE TO %2$s";
    private static final String COBOL_SET_COMPARE_NUMERIC =
            "           SET %1$sCOMPARE-NUMERIC TO %2$s";
    private static final String COBOL_SET_COMPARE_88_LEVEL =
            "           SET %1$sCOMPARE-88-LEVEL TO %2$s";
    private static final String COBOL_MOVE_FIELDNAME_TO_ACTUAL =
            "           MOVE %2$s TO %1$sACTUAL";
    private static final String COBOL_MOVE_FIELDNAME_TO_ACTUAL_NUMERIC =
            "           MOVE %2$s TO %1$sACTUAL-NUMERIC";
    private static final String COBOL_MOVE_EXPECTED_ALPHANUMERIC_LITERAL_1 =
            "           MOVE %s";
    private static final String COBOL_MOVE_EXPECTED_ALPHANUMERIC_LITERAL_2 =
            "               TO %sEXPECTED";
    private static final String COBOL_MOVE_EXPECTED_NUMERIC_LITERAL =
            "           MOVE %2$s TO %1$sEXPECTED-NUMERIC";


    private static final String COBOL_SET_ACTUAL_88_VALUE_1 =
            "           IF %1$s";
    private static final String COBOL_SET_ACTUAL_88_VALUE_2 =
            "               SET %1$sACTUAL-88-VALUE TO TRUE";
    private static final String COBOL_SET_ACTUAL_88_VALUE_3 =
            "               MOVE 'TRUE' TO %1$sACTUAL";
    private static final String COBOL_SET_ACTUAL_88_VALUE_4 =
            "           ELSE";
    private static final String COBOL_SET_ACTUAL_88_VALUE_5 =
            "               SET %1$sACTUAL-88-VALUE TO FALSE";
    private static final String COBOL_SET_ACTUAL_88_VALUE_6 =
            "               MOVE 'FALSE' TO %1$sACTUAL";
    private static final String COBOL_SET_ACTUAL_88_VALUE_7 =
            "           END-IF";

    private static final String COBOL_SET_EXPECTED_88_VALUE_1 =
            "           IF %1$sEXPECTED-88-VALUE";
    private static final String COBOL_SET_EXPECTED_88_VALUE_2 =
            "               MOVE 'TRUE' TO %1$sEXPECTED";
    private static final String COBOL_SET_EXPECTED_88_VALUE_3 =
            "           ELSE";
    private static final String COBOL_SET_EXPECTED_88_VALUE_4 =
            "               MOVE 'FALSE' TO %1$sEXPECTED";
    private static final String COBOL_SET_EXPECTED_88_VALUE_5 =
            "           END-IF";

    private static final String COBOL_SET_EXPECTED_88_VALUE =
            "           SET %1$sEXPECTED-88-VALUE TO %2$s";
    private static final String COBOL_SET_UT_COMPARE_DEFAULT =
            "           SET %1$sCOMPARE-DEFAULT TO %2$s";
    private static final String COBOL_PERFORM_UT_ASSERT_EQUAL =
            "           PERFORM %sASSERT-EQUAL";
    private static final String COBOL_PERFORM_UT_AFTER =
            "           PERFORM %sAFTER";
    private static final String ELEVEN_LEADING_SPACES = "           ";
    private StringBuffer cobolStatement;


    public Generator(
            Messages messages,
            TokenExtractor tokenExtractor,
            KeywordExtractor keywordExtractor,
            Config config) {
        this.config = config;
        this.messages = messages;
        this.tokenExtractor = tokenExtractor;
        this.keywordExtractor = keywordExtractor;
        testSuiteTokens = new ArrayList<>();
        emptyTestSuite = true;
        initializeCobolStatement();
        copybookDirectoryName = setCopybookDirectoryName(config);
        testCodePrefix = config.getString(COBOLCHECK_PREFIX_CONFIG_KEY, DEFAULT_COBOLCHECK_PREFIX);
        fileIdentifiersAndStatuses = new HashMap<>();
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
    public Writer mergeTestSuite(
            Reader testSuite,
            Reader cobolSourceIn,
            Writer testSourceOut) {

        if (testSuite == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        BufferedReader testSuiteReader
                = new BufferedReader(testSuite);
        if (cobolSourceIn == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        BufferedReader cobolSourceInReader
                = new BufferedReader(cobolSourceIn);
        String sourceLine;
        boolean emptyInputStream = true;
        try {
            while ((sourceLine = cobolSourceInReader.readLine()) != null) {
                emptyInputStream = false;
                sourceLine = fixedLength(sourceLine);
                List<String> tokens = tokenExtractor.extractTokensFrom(sourceLine);

                processingBeforeEchoingSourceLineToOutput(
                        tokens, sourceLine, cobolSourceInReader, testSourceOut);

                if (!skipThisLine) {
                    testSourceOut.write(sourceLine);
                }

                processingAfterEchoingSourceLineToOutput(
                        tokens, sourceLine, testSuiteReader, cobolSourceInReader, testSourceOut);
            }
            cobolSourceInReader.close();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
            catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
        if (emptyInputStream) {
            throw new PossibleInternalLogicErrorException(messages.get("ERR007"));
        }
        return testSourceOut;
    }

    /**
     * Change the state of the merge process depending on which section of the program under test we have reached.
     * This is how we know which kinds of source statements to look for when parsing the program source.
     *
     * @param partOfProgram
     */
    private void entering(String partOfProgram) {
        state.flags.get(partOfProgram).set();
    }

    /**
     * Perform appropriate processing of the current input line from the program under test prior to echoing that
     * line to the test program (that is, the copy of the program under test that has test code injected into it).
     *
     * @param tokens - extracted from the current source line
     * @param sourceLine - the current source line
     * @param reader - the reader attached to the source of the program under test
     * @param testSourceOut - the writer attached to the test program being generated
     * @throws IOException - pass any IOExceptions up to the caller
     */
    private void processingBeforeEchoingSourceLineToOutput(
            List<String> tokens,
            String sourceLine,
            Reader reader,
            Writer testSourceOut) throws IOException {


        System.out.println("just read sourceLine: <" + sourceLine + ">");

        skipThisLine = false;

        if (state.flags.get(FILE_CONTROL).isSet()) {
            if (expectFileStatusFieldName) {
                if (tokens.size() > 0) {
                    fileIdentifiersAndStatuses.put(fileIdentifier, tokens.get(0));
                    expectFileStatusFieldName = false;
                }
            }

            // When the current source line contains FILE STATUS, the next tokens will be [IS] FIELDNAME.
            // Those tokens may be coded on the same line or on subsequent lines in the source program.
            if (sourceLineContains(tokens, FILE_STATUS_TOKEN)) {
                if (tokens.size() > 2) {
                    if (tokens.get(1).equalsIgnoreCase(IS_TOKEN)) {
                        fileIdentifiersAndStatuses.put(fileIdentifier, tokens.get(2));
                    }
                } else {
                    if (tokens.size() > 1) {

                        if (tokens.get(1).equalsIgnoreCase(IS_TOKEN)) {
                            expectFileStatusFieldName = true;
                        } else {
                            fileIdentifiersAndStatuses.put(fileIdentifier, tokens.get(1));
                        }
                    } else {
                        expectFileStatusFieldName = true;
                    }
                }
            }
        }

        // We expect the next token from the source program to be the file identifier associated with the
        // most recent SELECT statement we encountered. It will become a key in a map of file identifiers
        // to file status field names.
        if (expectFileIdentifier) {
            if (tokens.size() > 0) {
                fileIdentifier = tokens.get(0);
                fileIdentifiersAndStatuses.put(fileIdentifier, EMPTY_STRING);
                expectFileIdentifier = false;
            }
        }

        if (sourceLineContains(tokens, ENVIRONMENT_DIVISION)) entering(ENVIRONMENT_DIVISION);

        if (sourceLineContains(tokens, CONFIGURATION_SECTION)) {
            entering(CONFIGURATION_SECTION);
            readingFileControl = false;
            skipThisLine = false;
        }

        if (sourceLineContains(tokens, INPUT_OUTPUT_SECTION)) entering(INPUT_OUTPUT_SECTION);

        if (sourceLineContains(tokens, FILE_CONTROL)) {
            entering(FILE_CONTROL);
            readingFileControl = true;
            fileControlStatements = new ArrayList<>();
        }

        if (readingFileControl) {
            processFileControlSource(tokens, sourceLine);
        }

        if (sourceLineContains(tokens, FILE_SECTION)) {
            entering(FILE_SECTION);
            readingFileSection = true;
            fileSectionStatements = new ArrayList<>();
        }

        if (readingFileSection) {
            processFileSectionSource(tokens, sourceLine);
        }

        if (sourceLineContains(tokens, DATA_DIVISION)) {
            entering(DATA_DIVISION);
            skipThisLine = false;
            readingFileControl = false;
        }

        if (sourceLineContains(tokens, PROCEDURE_DIVISION)) {
            entering(PROCEDURE_DIVISION);
            if (!workingStorageTestCodeHasBeenInserted) {
                testSourceOut.write(workingStorageHeader);
                insertWorkingStorageTestCode(testSourceOut);
            }
        }
        if (sourceLineContains(tokens, WORKING_STORAGE_SECTION)) {
            entering(WORKING_STORAGE_SECTION);
            skipThisLine = false;
            readingFileSection = false;
        }
    }

    /**
     * Perform appropriate processing after echoing (or skipping) the current source line from the program under test.
     *
     * @param tokens - extracted from the current source line
     * @param sourceLine - the original source line
     * @param testSuiteReader - reader attached to the user-written test suite
     * @param reader - reader attached to the source of the program under test
     * @param testSourceOut - writer attached to the test program being generated
     * @throws IOException - pass any IOExceptions to the caller
     */
    private void processingAfterEchoingSourceLineToOutput(
            List<String> tokens,
            String sourceLine,
            BufferedReader testSuiteReader,
            Reader reader,
            Writer testSourceOut) throws IOException {

        if (sourceLineContains(tokens, WORKING_STORAGE_SECTION)) {
            insertWorkingStorageTestCode(testSourceOut);
        }

        if (sourceLineContains(tokens, PROCEDURE_DIVISION)) {
            insertProcedureDivisionTestCode(testSuiteReader, testSourceOut);
        }
    }

    /**
     * Called for each source line read from the program under test while processing the File Section of the
     * Data Division. We capture source statements that define record layouts so that we can copy them into
     * the Working-Storage Section later. These statements may use optional Cobol keywords and they may be
     * coded on multiple source lines. We also need to expand any copybooks referenced in this part of the
     * source, in case data items in the copybooks are referenced by user-written test cases.
     *
     * @param tokens - tokens extracted from source line.
     * @param sourceLine - original source line.
     */
    void processFileSectionSource(List<String> tokens, String sourceLine) {
        if (isTooShortToBeMeaningful(sourceLine)) {
            skipThisLine = true;
            return;
        }
        if (isComment(sourceLine)) {
            skipThisLine = true;
            return;
        }
        if (sourceLineContains(tokens, FD_TOKEN)) {
            processingFD = true;
        }
        if (processingFD) {
            if (sourceLineContains(tokens, LEVEL_01_TOKEN)) {
                processing01ItemUnderFD = true;
                processingCopyStatement = false;
            } else {
                if (sourceLineContains(tokens, COPY_TOKEN)) {
                    // Collect the tokens that constitute the Copy statement
                    copyTokens = accumulateTokensFromCopyStatement(copyTokens, sourceLine);
                    if (sourceLine.trim().endsWith(".")) {
                        // COPY statement is complete on this line
                        fileSectionStatements.addAll(collectExpandedCopyStatements(copyTokens));
                        processingCopyStatement = false;
                    } else {
                        // COPY statement is coded across multiple lines
                        processingCopyStatement = true;
                    }
                    skipThisLine = true;
                    return;
                }
            }
            if (processing01ItemUnderFD) {
                if (sourceLineContains(tokens, WORKING_STORAGE_SECTION)
                        || sourceLineContains(tokens, LOCAL_STORAGE_SECTION)
                        || sourceLineContains(tokens, LINKAGE_SECTION)
                        || sourceLineContains(tokens, PROCEDURE_DIVISION)) {
                    processingFD = false;
                    processing01ItemUnderFD = false;
                } else {
                    if (sourceLineContains(tokens, FD_TOKEN)) {
                        processing01ItemUnderFD = false;
                        skipThisLine = true;
                        return;
                    } else {
                        if (processingCopyStatement) {
                            List<String> lineTokens = new ArrayList<>(List.of(sourceLine.trim().split(SPACE)));
                            copyTokens = accumulateTokensFromCopyStatement(copyTokens, sourceLine);
                            if (sourceLine.trim().endsWith(".")) {
                                // Multi-line COPY statement ends on this line
                                fileSectionStatements.addAll(collectExpandedCopyStatements(copyTokens));
                                processingCopyStatement = false;
                            }
                            skipThisLine = true;
                            return;
                        }
                    }
                    // Record layout statements coded directly and not in a copybook
                    fileSectionStatements.add(sourceLine);
                }
            }
        }
        // Don't echo these lines to the test source program
        skipThisLine = true;
    }

    /**
     * If we are currently reading the FILE CONTROL paragraph of the program under test, look for specific
     * source lines that require explicit processing by cobol-check.
     * Specifically, we need to save the file identifiers associated with SELECT statements and store the
     * corresponding field names of FILE STATUS specifications in case they are referenced in user-written
     * test cases. We also need to copy any record layout items into Working-Storage, as storage for FD areas
     * will not be allocated when we stub out the OPEN statements for files.
     *
     * @param tokens - extracted from the current source line
     * @param sourceLine - the original source line
     */
    void processFileControlSource(List<String> tokens, String sourceLine) {
        skipThisLine = true;
        if (isTooShortToBeMeaningful(sourceLine)) {
            return;
        }
        fileControlStatements.add(sourceLine);

        // If the current line contains SELECT, then the next token on the same line or the first token on the
        // next line will be the file identifier. We will store the file identifier as the key in a map of
        // file identifiers and file status field names.
        if (sourceLineContains(tokens, SELECT_TOKEN)) {
            fileIdentifier = EMPTY_STRING;
            if (tokens.size() > 1) {
                fileIdentifier = tokens.get(1);
                fileIdentifiersAndStatuses.put(fileIdentifier, EMPTY_STRING);
            } else {
                expectFileIdentifier = true;
            }
        }
    }

    List<String> accumulateTokensFromCopyStatement(List<String> copyTokens, String sourceLine) {
        if (copyTokens == null) {
            copyTokens = new ArrayList<>();
        }
        String[] lineTokens = sourceLine.trim().split(SPACE);
        for (String lineToken : lineTokens) {
            if (lineToken != null && !lineToken.equals(EMPTY_STRING)) {
                copyTokens.add(lineToken);
            }
        }
        return copyTokens;
    }

    List<String> collectExpandedCopyStatements(List<String> copyTokens) {
        for (int i = 0 ; i < copyTokens.size() ; i++) {
            if (copyTokens.get(i).equals(EMPTY_STRING)) {
                copyTokens.remove(i);
            }
        }
        if (copyTokens.isEmpty()
                || !copyTokens.get(0).equalsIgnoreCase(COPY_TOKEN)
                || copyTokens.size() < 2) {
            throw new PossibleInternalLogicErrorException(messages.get("ERR024"));
        }
        List<String> copyLines = new ArrayList<>();

        // 2nd entry is the name of the copybook. The value might end with a period.
        String copybookName = copyTokens.get(1).replace(PERIOD, EMPTY_STRING);

        // 3rd entry might be the word "REPLACING" followed by "x" "BY" "y"
        StringTuple replacingValues = new StringTuple(null, null);
        if (copyTokens.size() > 4) {
            if (copyTokens.get(2).equalsIgnoreCase(REPLACING_KEYWORD)
            || copyTokens.get(4).equalsIgnoreCase(BY_KEYWORD)) {
                replacingValues = new StringTuple(copyTokens.get(3), copyTokens.get(5));
            }
        }

        //crude implementation
        StringWriter expandedLines = new StringWriter();
        CopybookExpander copybookExpander = new CopybookExpander(config, messages);
        try {
            expandedLines = (StringWriter) copybookExpander.expand(
                    expandedLines,
                    copybookName,
                    PERIOD + config.getString(
                            APPLICATION_COPYBOOK_FILENAME_SUFFIX_KEY,
                            DEFAULT_APPLICATION_COPYBOOK_FILENAME_SUFFIX),
                    replacingValues);
            BufferedReader reader = new BufferedReader(new StringReader(expandedLines.toString()));
            String line = reader.readLine();
            while(line != null) {
                copyLines.add(line);
                line = reader.readLine();
            }
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
        return copyLines;
    }

    private boolean isComment(String sourceLine) {
        return sourceLine.charAt(commentIndicatorOffset) == commentIndicator;
    }

    private boolean isTooShortToBeMeaningful(String sourceLine) {
        return sourceLine == null || sourceLine.length() < minimumMeaningfulSourceLineLength;
    }

    /**
     * Insert record layout specifications captured from File Control at the top of Working-Storage.
     * Then insert Cobol Check Working-Storage entries.
     *
     * @param testSourceOut
     * @throws IOException
     */
    private void insertWorkingStorageTestCode(Writer testSourceOut) throws IOException {
        // If this program had File Section source that we need to move to Working-Storage, inject them here.
        if (fileSectionStatements != null) {
            for (String line : fileSectionStatements) {
                testSourceOut.write(fixedLength(line));
            }
        }

        // Inject boilerplate test code from cobol-check Working-Storage copybook
        secondarySourceReader = new FileReader(copybookFile(workingStorageCopybookFilename));
        insertSecondarySourceIntoTestSource(testSourceOut);
        workingStorageTestCodeHasBeenInserted = true;
    }

    private void insertProcedureDivisionTestCode(
            BufferedReader testSuiteReader,
            Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(COBOL_PERFORM_UT_INITIALIZE, testCodePrefix)));
        parseTestSuite(testSuiteReader, testSourceOut);
        secondarySourceReader = new FileReader(copybookFile(procedureDivisionCopybookFilename));
        insertSecondarySourceIntoTestSource(testSourceOut);
    }

    private void insertSecondarySourceIntoTestSource(Writer testSourceOut) throws IOException {
        BufferedReader secondarySourceBufferedReader = new BufferedReader(secondarySourceReader);
        String secondarySourceLine = EMPTY_STRING;
        while ((secondarySourceLine = secondarySourceBufferedReader.readLine()) != null) {
            secondarySourceLine = secondarySourceLine
                    .replaceAll(TEST_CODE_PREFIX_PLACEHOLDER, testCodePrefix);
            testSourceOut.write(fixedLength(secondarySourceLine));
        }
        secondarySourceBufferedReader.close();
    }

    void insertTestSuiteNameIntoTestSource(String testSuiteName, Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(COBOL_DISPLAY_TESTSUITE));
        writeCobolLine(String.format(COBOL_DISPLAY_NAME, testSuiteName), testSourceOut);
    }

    void insertTestCaseNameIntoTestSource(String testCaseName, Writer testSourceOut) throws IOException {
        writeCobolLine(String.format(COBOL_STORE_TESTCASE_NAME_1, testCaseName), testSourceOut);
        testSourceOut.write(fixedLength(String.format(COBOL_STORE_TESTCASE_NAME_2, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(COBOL_PERFORM_UT_BEFORE, testCodePrefix)));
    }

    void insertPerformBeforeEachIntoTestSource(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(COBOL_PERFORM_UT_BEFORE, testCodePrefix)));
    }

    void insertIncrementTestCaseCount(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(COBOL_INCREMENT_TEST_CASE_COUNT, testCodePrefix)));
    }

    void insertTestCodeForAssertion(Writer testSourceOut) throws IOException {
        if (alphanumericLiteralCompare) {
            insertTestCodeForAlphanumericEqualityCheck(testSourceOut);
        } else if (numericLiteralCompare) {
            insertTestCodeForNumericEqualityCheck(testSourceOut);
        } else if (boolean88LevelCompare) {
            insertTestCodeFor88LevelEqualityCheck(testSourceOut);
        }
    }

    void insertTestCodeForAlphanumericEqualityCheck(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_UT_NORMAL_COMPARE, testCodePrefix, TRUE)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_FIELDNAME_TO_ACTUAL, testCodePrefix, fieldNameForExpect)));
        String cobolLine = String.format(
                COBOL_MOVE_EXPECTED_ALPHANUMERIC_LITERAL_1, expectedValueToCompare);
        writeCobolLine(cobolLine, testSourceOut);
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_EXPECTED_ALPHANUMERIC_LITERAL_2, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_UT_COMPARE_DEFAULT, testCodePrefix, TRUE)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_UT_ASSERT_EQUAL, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_UT_AFTER, testCodePrefix)));
    }

    void insertTestCodeForNumericEqualityCheck(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_COMPARE_NUMERIC, testCodePrefix, TRUE)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_FIELDNAME_TO_ACTUAL_NUMERIC, testCodePrefix, fieldNameForExpect)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_EXPECTED_NUMERIC_LITERAL, testCodePrefix, expectedValueToCompare)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_UT_ASSERT_EQUAL, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_UT_AFTER, testCodePrefix)));
    }

    void insertTestCodeFor88LevelEqualityCheck(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_COMPARE_88_LEVEL, testCodePrefix, TRUE)));

        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_1, fieldNameForExpect)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_2, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_3, testCodePrefix)));
        testSourceOut.write(fixedLength(
                COBOL_SET_ACTUAL_88_VALUE_4));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_5, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_6, testCodePrefix)));
        testSourceOut.write(fixedLength(
                COBOL_SET_ACTUAL_88_VALUE_7));

        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_EXPECTED_88_VALUE, testCodePrefix, expectedValueToCompare)));

        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_EXPECTED_88_VALUE_1, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_EXPECTED_88_VALUE_2, testCodePrefix)));
        testSourceOut.write(fixedLength(
                COBOL_SET_EXPECTED_88_VALUE_3));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_EXPECTED_88_VALUE_4, testCodePrefix)));
        testSourceOut.write(fixedLength(
                COBOL_SET_EXPECTED_88_VALUE_5));

        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_UT_ASSERT_EQUAL, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_UT_AFTER, testCodePrefix)));
    }

    /**
     * Build a Cobol statement out of tokens from the test suite input.
     * Users may code standard Cobol statements to set up preconditions for a test case.
     * These tokens may occur immediately following the test case name string.
     * Users may code standard Cobol statements to define the behavior of a MOCK.
     *
     * @param testSuiteToken
     */
    void appendTokenToCobolStatement(String testSuiteToken) {
        if (cobolStatement.length() > 0) cobolStatement.append(SPACE);
        cobolStatement.append(testSuiteToken);
    }

    void insertUserWrittenCobolStatement(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(cobolStatement.toString()));
    }

    String quoted(String value) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(QUOTE);
        buffer.append(value);
        buffer.append(QUOTE);
        return buffer.toString();
    }

    private void initializeCobolStatement() {
        cobolStatement = new StringBuffer(ELEVEN_LEADING_SPACES);
    }

    /**
     * Process the test suite as a series of tokens. When we have processed all the input, getNextTokenFromTestSuite()
     * returns a null reference.
     *
     * @param testSuiteReader
     * @param testSourceOut
     */
    void parseTestSuite(BufferedReader testSuiteReader, Writer testSourceOut) throws IOException {
        String testSuiteToken = getNextTokenFromTestSuite(testSuiteReader);
        while (testSuiteToken != null) {
            if (!testSuiteToken.startsWith(QUOTE) && !testSuiteToken.startsWith(APOSTROPHE)) {
                testSuiteToken = testSuiteToken.toUpperCase(Locale.ROOT);
            }
            Keyword keyword = Keywords.getKeywordFor(testSuiteToken);

            if (Log.level() == LogLevel.DEBUG) {
                System.out.println("Generator.parseTestSuite(), " +
                        "testSuiteToken <" + testSuiteToken + ">, \tkeyword.value() <" + keyword.value() + ">");
            }

            // take actions triggered by the type of the current token
            switch (keyword.value()) {
                case TESTSUITE_KEYWORD:
                    expectTestsuiteName = true;
                    break;

                case TESTCASE_KEYWORD:
                    expectTestcaseName = true;
                    break;

                case EXPECT_KEYWORD:
                    if (cobolStatementInProgress) {
                        insertUserWrittenCobolStatement(testSourceOut);
                    }
                    cobolStatementInProgress = false;
                    initializeCobolStatement();
                    insertIncrementTestCaseCount(testSourceOut);
                    expectInProgress = true;
                    fieldNameForExpect = EMPTY_STRING;
                    break;

                case COBOL_TOKEN:
                    if (expectInProgress) {
                        fieldNameForExpect = testSuiteToken;
                        expectInProgress = false;
                    }
                    break;

                case ALPHANUMERIC_LITERAL_KEYWORD:
                    if (expectTestsuiteName) {
                        expectTestsuiteName = false;
                        currentTestSuiteName = testSuiteToken;
                        insertTestSuiteNameIntoTestSource(currentTestSuiteName, testSourceOut);
                        initializeCobolStatement();
                    }
                    if (expectTestcaseName) {
                        expectTestcaseName = false;
                        currentTestCaseName = testSuiteToken;
                        insertTestCaseNameIntoTestSource(currentTestCaseName, testSourceOut);
                        initializeCobolStatement();
                    }
                    if (toBeInProgress) {
                        if (testSuiteToken.startsWith(QUOTE) || testSuiteToken.startsWith(APOSTROPHE)) {
                            alphanumericLiteralCompare = true;
                            expectedValueToCompare = testSuiteToken;
                            insertTestCodeForAssertion(testSourceOut);
                            alphanumericLiteralCompare = false;
                        }
                        toBeInProgress = false;
                    }
                    break;

                case NUMERIC_LITERAL_KEYWORD:
                    if (toBeInProgress) {
                        numericLiteralCompare = true;
                        expectedValueToCompare = testSuiteToken;
                        insertTestCodeForAssertion(testSourceOut);
                        numericLiteralCompare = false;
                        toBeInProgress = false;
                    }
                    break;

                case BOOLEAN_VALUE:
                    if (toBeInProgress) {
                        boolean88LevelCompare = true;
                        expectedValueToCompare = testSuiteToken;
                        insertTestCodeForAssertion(testSourceOut);
                        boolean88LevelCompare = false;
                        toBeInProgress = false;
                    } else {
                        if (cobolStatementInProgress) {
                            appendTokenToCobolStatement(testSuiteToken);
                            insertUserWrittenCobolStatement(testSourceOut);
                            initializeCobolStatement();
                        }
                        cobolStatementInProgress = false;
                    }
                    break;

                case TO_BE_KEYWORD:
                    toBeInProgress = true;
                    break;
            }

            // take actions that were triggered by the previous token's action, pertaining to the current token
            switch (nextAction) {
                case TESTSUITE_NAME:
                    currentTestSuiteName = testSuiteToken;
                    nextAction = KeywordAction.NONE;
                    break;
                case NEW_TESTCASE:
                    currentTestCaseName = testSuiteToken;
                    nextAction = KeywordAction.NONE;
                    break;
            }

            // take actions that are triggered by the current token's action
            switch (keyword.keywordAction()) {
                case COBOL_STATEMENT:
                    if (CobolVerbs.isCobolVerb(testSuiteToken)) {
                        if (cobolStatementInProgress) {
                            insertUserWrittenCobolStatement(testSourceOut);
                            initializeCobolStatement();
                        }
                        cobolStatementInProgress = true;
                    }
                    appendTokenToCobolStatement(testSuiteToken);
                    break;
                case FIELDNAME:
                    if (cobolStatementInProgress) {
                        appendTokenToCobolStatement(testSuiteToken);
                    }
                    break;
            }
            nextAction = keyword.keywordAction();
            testSuiteToken = getNextTokenFromTestSuite(testSuiteReader);
        }
        if (cobolStatementInProgress) {
            insertUserWrittenCobolStatement(testSourceOut);
        }
    }

    /**
     * This method hides file I/O from the test suite parsing logic so the parsing logic will be easier to understand.
     * We don't want to load the whole test suite into memory at once, as we don't know how large it may be.
     * Here we consume tokens one by one and invoke the file read routine whenever we exhaust the list of tokens.
     * When the file read routine returns a null reference, it means we have reached end-of-file on the test suite.
     * This method uses a keyword extractor instance to get tokens from the input record. "Tokens" in this context
     * may mean phrases that contain embedded spaces, like "TO BE", and quoted string literals with the quotes intact.
     * Comment lines are bypassed, as there is no need to insert them into the test program.
     *
     * @param testSuiteReader
     * @return
     */
    private String getNextTokenFromTestSuite(BufferedReader testSuiteReader) {
        while (testSuiteTokens.isEmpty()) {
            String testSuiteLine = readNextLineFromTestSuite(testSuiteReader);
            if (testSuiteLine == null) {
                return null;
            }
            if (testSuiteLine.length() > 5 && testSuiteLine.charAt(6) != '*') {
                testSuiteTokens = keywordExtractor.extractTokensFrom(testSuiteLine);
            }
        }
        String testSuiteToken = testSuiteTokens.get(0);
        testSuiteTokens.remove(0);
        return testSuiteToken;
    }

    /**
     * This method performs the grunt work of reading records from the test suite input source.
     *
     * @param testSuiteReader
     * @return
     */
    private String readNextLineFromTestSuite(BufferedReader testSuiteReader) {
        String testSuiteLine = EMPTY_STRING;
        try {
            testSuiteLine = testSuiteReader.readLine();
            if (testSuiteLine == null) {
                if (emptyTestSuite) {
                    throw new PossibleInternalLogicErrorException(messages.get("ERR010"));
                }
                return null;
            }
            emptyTestSuite = false;
            return testSuiteLine;
        } catch (IOException ioEx) {
            throw new TestSuiteCouldNotBeReadException(ioEx);
        }
        catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
    }

    private boolean sourceLineContains(List<String> tokens, String tokenValue) {
        return tokens.size() > 0 && tokens.contains(tokenValue.toUpperCase(Locale.ROOT));
    }

    private File copybookFile(String fileName) {
        return new File(copybookDirectoryName + fileName);
    }

    private String setCopybookDirectoryName(Config config) {
        return config.getString(RESOURCES_DIRECTORY_CONFIG_KEY)
                + Constants.FILE_SEPARATOR
                + this.getClass().getPackageName().replace(".", "/")
                + Constants.FILE_SEPARATOR
                + config.getString(COBOLCHECK_COPYBOOK_DIRECTORY_CONFIG_KEY)
                + Constants.FILE_SEPARATOR;
    }

    String getCurrentTestSuiteName() {
        return currentTestSuiteName;
    }

    String getCurrentTestCaseName() {
        return currentTestCaseName;
    }

    String getCobolStatement() {
        return cobolStatement.toString();
    }

    void writeCobolLine(String line, Writer testSourceOut) throws IOException {
        String line1 = line;
        String line2 = EMPTY_STRING;
        if (line.length() > 72) {
            line1 = line.substring(0,72);
            line2 = line.substring(72);
        }
        testSourceOut.write(fixedLength(line1));
        if (line2.length() > 0) {
            line2 = fixedLength("      -    \"" + line2);
        }
        testSourceOut.write(line2);
    }

    class State {
        private final Map<String, Flag> flags;

        public State() {
            flags = new HashMap<>();

            flags.put(IDENTIFICATION_DIVISION, new Flag());

            flags.put(FILE_SECTION, new Flag());
            flags.put(LINKAGE_SECTION, new Flag());
            flags.put(LOCAL_STORAGE_SECTION, new Flag());
            flags.put(WORKING_STORAGE_SECTION, new Flag());
            mutuallyExclusiveFlagsFor(FILE_SECTION,
                    LINKAGE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);
            mutuallyExclusiveFlagsFor(LINKAGE_SECTION,
                    FILE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);
            mutuallyExclusiveFlagsFor(LOCAL_STORAGE_SECTION,
                    LINKAGE_SECTION, FILE_SECTION, WORKING_STORAGE_SECTION);
            mutuallyExclusiveFlagsFor(WORKING_STORAGE_SECTION,
                    LINKAGE_SECTION, LOCAL_STORAGE_SECTION, FILE_SECTION);

            flags.put(CONFIGURATION_SECTION, new Flag());
            flags.put(FILE_CONTROL, new Flag());
            flags.put(INPUT_OUTPUT_SECTION, new Flag());
            dependentFlagsFor(INPUT_OUTPUT_SECTION,
                    FILE_CONTROL);
            mutuallyExclusiveFlagsFor(CONFIGURATION_SECTION,
                    INPUT_OUTPUT_SECTION);
            mutuallyExclusiveFlagsFor(INPUT_OUTPUT_SECTION,
                    CONFIGURATION_SECTION);

            flags.put(ENVIRONMENT_DIVISION, new Flag());
            dependentFlagsFor(ENVIRONMENT_DIVISION,
                    CONFIGURATION_SECTION, INPUT_OUTPUT_SECTION);

            flags.put(DATA_DIVISION, new Flag());
            dependentFlagsFor(DATA_DIVISION,
                    FILE_SECTION, LINKAGE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);

            flags.put(PROCEDURE_DIVISION, new Flag());

            mutuallyExclusiveFlagsFor(IDENTIFICATION_DIVISION,
                    DATA_DIVISION, ENVIRONMENT_DIVISION, PROCEDURE_DIVISION);
            mutuallyExclusiveFlagsFor(ENVIRONMENT_DIVISION,
                    IDENTIFICATION_DIVISION, DATA_DIVISION, PROCEDURE_DIVISION);
            mutuallyExclusiveFlagsFor(DATA_DIVISION,
                    IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, PROCEDURE_DIVISION);
            mutuallyExclusiveFlagsFor(PROCEDURE_DIVISION,
                    IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, DATA_DIVISION);


        }

        private void mutuallyExclusiveFlagsFor(String token, String... mutuallyExclusiveTokens) {
            List<Flag> mutuallyExclusiveFlags = new ArrayList<>();
            for (String mutuallyExclusiveToken : mutuallyExclusiveTokens) {
                mutuallyExclusiveFlags.add(flags.get(mutuallyExclusiveToken));
            }
            flags.get(token).setMutuallyExclusiveFlags(mutuallyExclusiveFlags);
        }
        private void dependentFlagsFor(String token, String... dependentTokens) {
            List<Flag> dependentFlags = new ArrayList<>();
            for (String dependentToken : dependentTokens) {
                dependentFlags.add(flags.get(dependentToken));
            }
            flags.get(token).setDependentFlags(dependentFlags);
        }
    }

    static class Flag {
        private boolean state = false;
        private List<Flag> mutuallyExclusiveFlags;
        private List<Flag> dependentFlags;
        public Flag() {
            this.mutuallyExclusiveFlags = new ArrayList<>();
            this.dependentFlags = new ArrayList<>();
        }
        public void setMutuallyExclusiveFlags(List<Flag> mutuallyExclusiveFlags) {
            this.mutuallyExclusiveFlags = mutuallyExclusiveFlags;
        }
        public void setDependentFlags(List<Flag> dependentFlags) {
            this.dependentFlags = dependentFlags;
        }
        public boolean isSet() {
            return state;
        }
        public void set() {
            state = true;
            for (Flag flag : mutuallyExclusiveFlags) {
                flag.unset();
            }
        }
        public void unset() {
            state = false ;
            for (Flag flag : dependentFlags) {
                flag.unset();
            }
        }
    }

}
