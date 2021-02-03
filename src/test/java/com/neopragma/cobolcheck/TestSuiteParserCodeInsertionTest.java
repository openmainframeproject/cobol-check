package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class TestSuiteParserCodeInsertionTest {

    Writer testSourceOut;
    TestSuiteParser testSuiteParser;
    @Mock
    NumericFields numericFields;

    private static StringBuilder ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT;

    @BeforeAll
    public static void initializeExpectedResults() {
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE \"Hello\"                                                         ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE \"Hello\"                                                         ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-TEXT                                                         ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-TEXT                                                         ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-NUMERIC TO TRUE                                       ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-VALUE TO UT-ACTUAL-NUMERIC                                   ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE 18.92 TO UT-EXPECTED-NUMERIC                                    ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-NUMERIC TO TRUE                                       ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-VALUE TO UT-ACTUAL-NUMERIC                                   ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE 18.92 TO UT-EXPECTED-NUMERIC                                    ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT = new StringBuilder();
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           SET UT-COMPARE-NUMERIC TO TRUE                                       ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           MOVE 18 TO UT-EXPECTED-NUMERIC                                       ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           SET UT-RELATION-GT TO TRUE                                           ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);

        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-NUMERIC TO TRUE                                       ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-EXPECTED TO UT-EXPECTED-NUMERIC                              ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT = new StringBuilder();
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-NUMERIC TO TRUE                                       ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-EXPECTED TO UT-EXPECTED-NUMERIC                              ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
    }

    @Mock
    Config config;

    @BeforeEach
    public void commonSetup() {
        testSourceOut = new StringWriter();
        when(config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX))
                .thenReturn("UT-");
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), config);
    }

    @Test
    public void it_recognizes_the_end_of_a_user_written_cobol_statement_when_it_encounters_a_cobolcheck_keyword_that_can_follow_a_user_written_statement() throws IOException {
        String testSuite = "            MOVE \"alpha\" TO WS-FIELDNAME                                           " +
                Constants.NEWLINE +
                "           EXPECT                                                                    " +
                Constants.NEWLINE;
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME                                        " +
                Constants.NEWLINE +
                "           ADD 1 TO UT-TEST-CASE-COUNT                                          " +
                Constants.NEWLINE;
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_display_the_testsuite_name() throws IOException {
        String expectedResult =
                "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE
              + "           DISPLAY \"Test Suite Name\"                                            " + Constants.NEWLINE;
        testSuiteParser.insertTestSuiteNameIntoTestSource("\"Test Suite Name\"", testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_store_the_testcase_name() throws IOException {
        String expectedResult =
                "           MOVE \"Test Case Name\"                                                " + Constants.NEWLINE
              + "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE
              + "           PERFORM UT-BEFORE                                                    " + Constants.NEWLINE;

        testSuiteParser.insertTestCaseNameIntoTestSource("\"Test Case Name\"", testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_perform_before_each_logic() throws IOException {
        String expectedResult =
                "           PERFORM UT-BEFORE                                                    " + Constants.NEWLINE;
        testSuiteParser.insertPerformBeforeEachIntoTestSource(testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_for_an_88_level_equality_check_in_an_EXPECT() throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader("           EXPECT WS-88-LEVEL-ITEM TO BE TRUE"));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        String expectedResult = "           ADD 1 TO UT-TEST-CASE-COUNT                                          " +
                Constants.NEWLINE +
                "           SET UT-COMPARE-88-LEVEL TO TRUE                                      " +
                Constants.NEWLINE +
                "           IF WS-88-LEVEL-ITEM                                                  " +
                Constants.NEWLINE +
                "               SET UT-ACTUAL-88-VALUE TO TRUE                                   " +
                Constants.NEWLINE +
                "               MOVE 'TRUE' TO UT-ACTUAL                                         " +
                Constants.NEWLINE +
                "           ELSE                                                                 " +
                Constants.NEWLINE +
                "               SET UT-ACTUAL-88-VALUE TO FALSE                                  " +
                Constants.NEWLINE +
                "               MOVE 'FALSE' TO UT-ACTUAL                                        " +
                Constants.NEWLINE +
                "           END-IF                                                               " +
                Constants.NEWLINE +
                "           SET UT-EXPECTED-88-VALUE TO TRUE                                     " +
                Constants.NEWLINE +
                "           IF UT-EXPECTED-88-VALUE                                              " +
                Constants.NEWLINE +
                "               MOVE 'TRUE' TO UT-EXPECTED                                       " +
                Constants.NEWLINE +
                "           ELSE                                                                 " +
                Constants.NEWLINE +
                "               MOVE 'FALSE' TO UT-EXPECTED                                      " +
                Constants.NEWLINE +
                "           END-IF                                                               " +
                Constants.NEWLINE +
                "           PERFORM UT-CHECK-EXPECTATION                                         " +
                Constants.NEWLINE +
                "           PERFORM UT-AFTER                                                     " +
                Constants.NEWLINE;
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @ParameterizedTest
    @MethodSource("alphanumericLiteralEqualityCheckProvider")
    public void it_inserts_cobol_statements_for_an_alphanumeric_literal_equality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> alphanumericLiteralEqualityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-MESSAGE TO BE \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE TO EQUAL \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE = \"Hello\"")
        );
    }

    @ParameterizedTest
    @MethodSource("alphanumericLiteralInequalityCheckProvider")
    public void it_inserts_cobol_statements_for_an_alphanumeric_literal_inequality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader("           EXPECT WS-MESSAGE NOT TO BE \"Hello\""));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> alphanumericLiteralInequalityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-MESSAGE NOT TO BE \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO EQUAL \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE NOT = \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE != \"Hello\"")
        );
    }

    @ParameterizedTest
    @MethodSource("alphanumericFieldEqualityCheckProvider")
    public void it_inserts_cobol_statements_for_an_alphanumeric_field_equality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> alphanumericFieldEqualityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-MESSAGE TO BE WS-TEXT"),
                Arguments.of("           EXPECT WS-MESSAGE TO EQUAL WS-TEXT"),
                Arguments.of("           EXPECT WS-MESSAGE = WS-TEXT")
        );
    }

    @ParameterizedTest
    @MethodSource("alphanumericFieldInequalityCheckProvider")
    public void it_inserts_cobol_statements_for_an_alphanumeric_field_inequality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> alphanumericFieldInequalityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-MESSAGE NOT TO BE WS-TEXT"),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO EQUAL WS-TEXT"),
                Arguments.of("           EXPECT WS-MESSAGE NOT = WS-TEXT"),
                Arguments.of("           EXPECT WS-MESSAGE != WS-TEXT")
        );
    }

    @ParameterizedTest
    @MethodSource("numericLiteralEqualityCheckProvider")
    public void it_inserts_cobol_statements_for_a_numeric_literal_equality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> numericLiteralEqualityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-VALUE TO BE 18.92"),
                Arguments.of("           EXPECT WS-VALUE TO EQUAL 18.92"),
                Arguments.of("           EXPECT WS-VALUE = 18.92")
        );
    }

    @ParameterizedTest
    @MethodSource("numericLiteralInequalityCheckProvider")
    public void it_inserts_cobol_statements_for_a_numeric_literal_inequality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> numericLiteralInequalityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-VALUE NOT TO BE 18.92"),
                Arguments.of("           EXPECT WS-VALUE NOT TO EQUAL 18.92"),
                Arguments.of("           EXPECT WS-VALUE NOT = 18.92"),
                Arguments.of("           EXPECT WS-VALUE != 18.92")
        );
    }

    @ParameterizedTest
    @MethodSource("numericLiteralGreaterThanCheckProvider")
    public void it_inserts_cobol_statements_for_a_numeric_literal_greater_than_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> numericLiteralGreaterThanCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-ACTUAL > 18")
        );
    }

    @ParameterizedTest
    @MethodSource("numericFieldEqualityCheckProvider")
    public void it_inserts_cobol_statements_for_a_numeric_field_equality_check_in_an_EXPECT(
            String testSuiteInput, String fieldName, DataType dataType) throws IOException {
        doReturn(dataType).when(numericFields).dataTypeOf(fieldName);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> numericFieldEqualityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-ACTUAL TO BE WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL),
                Arguments.of("           EXPECT WS-ACTUAL TO EQUAL WS-EXPECTED",
                        "WS-ACTUAL", DataType.PACKED_DECIMAL),
                Arguments.of("           EXPECT WS-ACTUAL = WS-EXPECTED",
                "WS-ACTUAL", DataType.PACKED_DECIMAL)
        );
    }

    @ParameterizedTest
    @MethodSource("numericFieldInequalityCheckProvider")
    public void it_inserts_cobol_statements_for_a_numeric_field_inequality_check_in_an_EXPECT(
            String testSuiteInput,
            String fieldName,
            DataType dataType) throws IOException {
        lenient().doReturn(dataType).when(numericFields).dataTypeOf(fieldName);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> numericFieldInequalityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-ACTUAL NOT TO BE WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL),
                Arguments.of("           EXPECT WS-ACTUAL NOT TO EQUAL WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL),
                Arguments.of("           EXPECT WS-ACTUAL NOT = WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL),
                Arguments.of("           EXPECT WS-ACTUAL != WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL)
        );
    }
}
