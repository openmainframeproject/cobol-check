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

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.exceptions.TestSuiteCouldNotBeReadException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class TestSuiteParser implements Constants, StringHelper {
    private final KeywordExtractor keywordExtractor;
//    private Config config;
    private final Messages messages;
    private List<String> testSuiteTokens;

    // Source tokens used in fully-qualified data item names
    private List<String> qualifiedNameKeywords = List.of("IN", "OF");


    // Optionally replace identifier prefixes in cobol-check copybook lines and generated source lines,
    // in case of conflict with prefixes used in programs to be tested.
    // This is set in config.properties, cobolcheck.prefix entry.
    private final String testCodePrefix;

    // Flags to keep track of context while reading input source files.
    // We want to make a single pass of all inputs, so we need to know what we are looking for at any given point.
    private boolean emptyTestSuite;
    private boolean cobolStatementInProgress;
    private boolean expectInProgress;
    private boolean toBeInProgress;
    private boolean alphanumericCompare;
    private boolean numericLiteralCompare;
    private boolean boolean88LevelCompare;
    private boolean expectTestsuiteName;
    private boolean expectTestcaseName;
    private String fieldNameForExpect;
    private boolean possibleQualifiedName;
    private boolean expectQualifiedName;
    private String expectedValueToCompare;
    private KeywordAction nextAction = KeywordAction.NONE;
    private String currentTestSuiteName = EMPTY_STRING;
    private String currentTestCaseName = EMPTY_STRING;

    // Lines inserted into the test program
    private static final String COBOL_PERFORM_INITIALIZE =
            "           PERFORM %sINITIALIZE";
    private static final String COBOL_DISPLAY_TESTSUITE =
            "           DISPLAY \"TESTSUITE:\"                                                 ";
    private static final String COBOL_DISPLAY_NAME =
            "           DISPLAY %s";
    private static final String COBOL_STORE_TESTCASE_NAME_1 =
            "           MOVE %s";
    private static final String COBOL_STORE_TESTCASE_NAME_2 =
            "               TO %sTEST-CASE-NAME";
    private static final String COBOL_PERFORM_BEFORE =
            "           PERFORM %sBEFORE";
    private static final String COBOL_INCREMENT_TEST_CASE_COUNT =
            "           ADD 1 TO %sTEST-CASE-COUNT";
    private static final String COBOL_SET_NORMAL_COMPARE =
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
    private static final String COBOL_SET_COMPARE_DEFAULT =
            "           SET %1$sCOMPARE-DEFAULT TO %2$s";
    private static final String COBOL_PERFORM_ASSERT_EQUAL =
            "           PERFORM %sASSERT-EQUAL";
    private static final String COBOL_PERFORM_AFTER =
            "           PERFORM %sAFTER";
    private static final String ELEVEN_LEADING_SPACES = "           ";
    private StringBuffer cobolStatement;

    public TestSuiteParser(
            KeywordExtractor keywordExtractor,
            Config config) {
        this.keywordExtractor = keywordExtractor;
        this.messages = config.getMessages();
        testSuiteTokens = new ArrayList<>();
        emptyTestSuite = true;
        testCodePrefix = config.getString(COBOLCHECK_PREFIX_CONFIG_KEY, DEFAULT_COBOLCHECK_PREFIX);
        initializeCobolStatement();
    }

    /**
     * Process the test suite as a series of tokens. When we have processed all the input, getNextTokenFromTestSuite()
     * returns a null reference.
     *
     * @param testSuiteReader - reader attached to the concatenated test suite files.
     * @param testSourceOut - writer attached to the test program being generated.
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
                    if (expectQualifiedName) {
                        fieldNameForExpect += testSuiteToken;
                        expectQualifiedName = false;
                    }
                    if (possibleQualifiedName) {
                        if (qualifiedNameKeywords.contains(testSuiteToken)) {
                            fieldNameForExpect += SPACE + testSuiteToken + SPACE;
                            expectQualifiedName = true;
                            possibleQualifiedName = false;
                        }
                    }
                    if (expectInProgress) {
                        fieldNameForExpect = testSuiteToken;
                        expectInProgress = false;
                        possibleQualifiedName = true;
                    }
                    if (toBeInProgress) {
                        alphanumericCompare = true;
                        expectedValueToCompare = testSuiteToken;
                        insertTestCodeForAssertion(testSourceOut);
                        toBeInProgress = false;
                        alphanumericCompare = false;
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
                            alphanumericCompare = true;
                            expectedValueToCompare = testSuiteToken;
                            insertTestCodeForAssertion(testSourceOut);
                            alphanumericCompare = false;
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
                case TO_EQUAL_KEYWORD:
                    toBeInProgress = true;
                    break;
            }

            // take actions that were triggered by the previous token's action, pertaining to the current token
            switch (nextAction) {
                case TESTSUITE_NAME -> {
                    currentTestSuiteName = testSuiteToken;
                    nextAction = KeywordAction.NONE;
                }
                case NEW_TESTCASE -> {
                    currentTestCaseName = testSuiteToken;
                    nextAction = KeywordAction.NONE;
                }
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
     * @param testSuiteReader - reader attached to the concatenated test suite files.
     * @return - the next token from the testSuiteReader.
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
     * @param testSuiteReader - reader attached to the concatenated test suite files.
     * @return - line of source from the testSuiteReader.
     */
    private String readNextLineFromTestSuite(BufferedReader testSuiteReader) {
        String testSuiteLine;
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

    // Helper methods to insert code into the test program being generated based on interpretation of user-written
    // test case code.

    void insertTestInitializationLineIntoTestSource(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(COBOL_PERFORM_INITIALIZE, testCodePrefix)));
    }

    void insertTestSuiteNameIntoTestSource(String testSuiteName, Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(COBOL_DISPLAY_TESTSUITE));
        writeCobolLine(String.format(COBOL_DISPLAY_NAME, testSuiteName), testSourceOut);
    }

    void insertTestCaseNameIntoTestSource(String testCaseName, Writer testSourceOut) throws IOException {
        writeCobolLine(String.format(COBOL_STORE_TESTCASE_NAME_1, testCaseName), testSourceOut);
        testSourceOut.write(fixedLength(String.format(COBOL_STORE_TESTCASE_NAME_2, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(COBOL_PERFORM_BEFORE, testCodePrefix)));
    }

    void insertPerformBeforeEachIntoTestSource(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(COBOL_PERFORM_BEFORE, testCodePrefix)));
    }

    void insertIncrementTestCaseCount(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(COBOL_INCREMENT_TEST_CASE_COUNT, testCodePrefix)));
    }

    void insertTestCodeForAssertion(Writer testSourceOut) throws IOException {
        if (alphanumericCompare) {
            insertTestCodeForAlphanumericEqualityCheck(testSourceOut);
        } else if (numericLiteralCompare) {
            insertTestCodeForNumericEqualityCheck(testSourceOut);
        } else if (boolean88LevelCompare) {
            insertTestCodeFor88LevelEqualityCheck(testSourceOut);
        }
    }

    void insertTestCodeForAlphanumericEqualityCheck(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_NORMAL_COMPARE, testCodePrefix, TRUE)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_FIELDNAME_TO_ACTUAL, testCodePrefix, fieldNameForExpect)));
        String cobolLine = String.format(
                COBOL_MOVE_EXPECTED_ALPHANUMERIC_LITERAL_1, expectedValueToCompare);
        writeCobolLine(cobolLine, testSourceOut);
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_EXPECTED_ALPHANUMERIC_LITERAL_2, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_COMPARE_DEFAULT, testCodePrefix, TRUE)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_ASSERT_EQUAL, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_AFTER, testCodePrefix)));
    }

    void insertTestCodeForNumericEqualityCheck(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(String.format(
                COBOL_SET_COMPARE_NUMERIC, testCodePrefix, TRUE)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_FIELDNAME_TO_ACTUAL_NUMERIC, testCodePrefix, fieldNameForExpect)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_MOVE_EXPECTED_NUMERIC_LITERAL, testCodePrefix, expectedValueToCompare)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_ASSERT_EQUAL, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_AFTER, testCodePrefix)));
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
                COBOL_PERFORM_ASSERT_EQUAL, testCodePrefix)));
        testSourceOut.write(fixedLength(String.format(
                COBOL_PERFORM_AFTER, testCodePrefix)));
    }

    /**
     * Build a Cobol statement out of tokens from the test suite input.
     * Users may code standard Cobol statements to set up preconditions for a test case.
     * These tokens may occur immediately following the test case name string.
     * Users may code standard Cobol statements to define the behavior of a MOCK.
     *
     * @param testSuiteToken - token extracted from test suit input
     */
    void appendTokenToCobolStatement(String testSuiteToken) {
        if (cobolStatement.length() > 0) cobolStatement.append(SPACE);
        cobolStatement.append(testSuiteToken);
    }

    /**
     * Insert user-written Cobol statement from a test suite (not from the program under test) into the test program
     * being generated.
     *
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller
     */
    void insertUserWrittenCobolStatement(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(cobolStatement.toString()));
    }

    private void initializeCobolStatement() {
        cobolStatement = new StringBuffer(ELEVEN_LEADING_SPACES);
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

    /**
     * Lines of test code in a test suite are Cobol-like, but not strictly Cobol. The descriptions for TESTSUITE and
     * TESTCASE specifications may exceed the maximum length allowed for Area B in the generated test Cobol program.
     * This method splits the literal and writes the value with a continuation line, if necessary.
     *
     * Limitation: Only works for a maximum of 2 source lines.
     *
     * @param line - original line from test suite.
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeCobolLine(String line, Writer testSourceOut) throws IOException {
        //TODO: Enhance this to work with an arbitrary number of continuation lines
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


}
