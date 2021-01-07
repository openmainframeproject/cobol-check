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
    private final Messages messages;
    private final TokenExtractor tokenExtractor;
    private final KeywordExtractor keywordExtractor;

    private final State state = new State();

    private static final String IDENTIFICATION_DIVISION = "IDENTIFICATION DIVISION";
    private static final String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION";
    private static final String DATA_DIVISION = "DATA DIVISION";
    private static final String PROCEDURE_DIVISION = "PROCEDURE DIVISION";
    private static final String FILE_SECTION = "FILE SECTION";
    private static final String LOCAL_STORAGE_SECTION = "LOCAL-STORAGE SECTION";
    private static final String LINKAGE_SECTION = "LINKAGE SECTION";
    private static final String WORKING_STORAGE_SECTION = "WORKING-STORAGE SECTION";

    private static final String workingStorageCopybookFilename = "ZUTZCWS.CPY";
    private static final String procedureDivisionCopybookFilename = "ZUTZCPD.CPY";

    private boolean workingStorageTestCodeHasBeenInserted = false;
    private final String workingStorageHeader = fixedLength("       WORKING-STORAGE SECTION.");
    private static String copybookDirectoryName = EMPTY_STRING;

    private Reader secondarySourceReader;
    private KeywordAction nextAction = KeywordAction.NONE;
    private String currentTestSuiteName = EMPTY_STRING;
    private String currentTestCaseName = EMPTY_STRING;

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
    private String fieldNameForExpect;
    private String expectedValueToCompare;

    //TODO: Use prefix specified in config file
    private static final String testCodePrefix = "UT-";

    // Lines inserted into the test program
    private static final String COBOL_PERFORM_UT_INITIALIZE = "           PERFORM UT-INITIALIZE";
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
        this.messages = messages;
        this.tokenExtractor = tokenExtractor;
        this.keywordExtractor = keywordExtractor;
        testSuiteTokens = new ArrayList<>();
        emptyTestSuite = true;
        initializeCobolStatement();
        copybookDirectoryName = setCopybookDirectoryName(config);
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
                processingBeforeEchoingTheSourceLineToTheOutput(
                        tokens, sourceLine, cobolSourceInReader, testSourceOut);
                testSourceOut.write(sourceLine);
                processingAfterEchoingTheSourceLineToTheOutput(
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

    private void processingBeforeEchoingTheSourceLineToTheOutput(
            List<String> tokens,
            String sourceLine,
            Reader reader,
            Writer testSourceOut) throws IOException {

        if (sourceLineContains(tokens, DATA_DIVISION)) entering(DATA_DIVISION);

        if (sourceLineContains(tokens, PROCEDURE_DIVISION)) {
            entering(PROCEDURE_DIVISION);
            if (!workingStorageTestCodeHasBeenInserted) {
                testSourceOut.write(workingStorageHeader);
                insertWorkingStorageTestCode(testSourceOut);
            }
        }
        if (sourceLineContains(tokens, WORKING_STORAGE_SECTION)) entering(WORKING_STORAGE_SECTION);
    }

    private void processingAfterEchoingTheSourceLineToTheOutput(
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

    private void insertWorkingStorageTestCode(Writer testSourceOut) throws IOException {
        secondarySourceReader = new FileReader(copybookFile(workingStorageCopybookFilename));
        insertSecondarySourceIntoTestSource(testSourceOut);
        workingStorageTestCodeHasBeenInserted = true;
    }

    private void insertProcedureDivisionTestCode(
            BufferedReader testSuiteReader,
            Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(COBOL_PERFORM_UT_INITIALIZE));
        parseTestSuite(testSuiteReader, testSourceOut);
        secondarySourceReader = new FileReader(copybookFile(procedureDivisionCopybookFilename));
        insertSecondarySourceIntoTestSource(testSourceOut);
    }

    private void insertSecondarySourceIntoTestSource(Writer testSourceOut) throws IOException {
        BufferedReader secondarySourceBufferedReader = new BufferedReader(secondarySourceReader);
        String secondarySourceLine = EMPTY_STRING;
        while ((secondarySourceLine = secondarySourceBufferedReader.readLine()) != null) {
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
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_4)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_5, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_6, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_ACTUAL_88_VALUE_7, testCodePrefix)));

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
        return tokens.size() > 0 && tokens.contains(tokenValue);
    }

    private File copybookFile(String fileName) {
        return new File(copybookDirectoryName + fileName);
    }

    private void entering(String partOfProgram) {
        state.flags.get(partOfProgram).set();
    }

    private String setCopybookDirectoryName(Config config) {
        return config.getString("resources.directory")
                + Constants.FILE_SEPARATOR
                + this.getClass().getPackageName().replace(".", "/")
                + Constants.FILE_SEPARATOR
                + config.getString("cobolcheck.copybook.directory")
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

            flags.put(IDENTIFICATION_DIVISION, new Flag());

            flags.put(ENVIRONMENT_DIVISION, new Flag());

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
