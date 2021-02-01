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
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class TestSuiteParserCodeInsertionTest {

    Writer testSourceOut;
    TestSuiteParser testSuiteParser;


    private static StringBuilder ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT;

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

        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT = new StringBuilder();
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           MOVE \"Hello\"                                                         ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);

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

        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT = new StringBuilder();
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-NUMERIC TO TRUE                                       ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           MOVE WS-VALUE TO UT-ACTUAL-NUMERIC                                   ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           MOVE 18.92 TO UT-EXPECTED-NUMERIC                                    ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append("           PERFORM UT-AFTER                                                     ");
        NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);


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
        StringBuffer expectedResult = new StringBuffer();
        expectedResult.append("            MOVE \"alpha\" TO WS-FIELDNAME                                        ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        expectedResult.append(Constants.NEWLINE);
        StringBuffer testSuite = new StringBuffer();
        testSuite.append("            MOVE \"alpha\" TO WS-FIELDNAME                                           ");
        testSuite.append(Constants.NEWLINE);
        testSuite.append("           EXPECT                                                                    ");
        testSuite.append(Constants.NEWLINE);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(expectedResult.toString(), testSourceOut.toString());
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
        StringBuilder expectedResult = new StringBuilder();
        expectedResult.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           SET UT-COMPARE-88-LEVEL TO TRUE                                      ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           IF WS-88-LEVEL-ITEM                                                  ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               SET UT-ACTUAL-88-VALUE TO TRUE                                   ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               MOVE 'TRUE' TO UT-ACTUAL                                         ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           ELSE                                                                 ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               SET UT-ACTUAL-88-VALUE TO FALSE                                  ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               MOVE 'FALSE' TO UT-ACTUAL                                        ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           END-IF                                                               ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           SET UT-EXPECTED-88-VALUE TO TRUE                                     ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           IF UT-EXPECTED-88-VALUE                                              ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               MOVE 'TRUE' TO UT-EXPECTED                                       ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           ELSE                                                                 ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               MOVE 'FALSE' TO UT-EXPECTED                                      ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           END-IF                                                               ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           PERFORM UT-AFTER                                                     ");
        expectedResult.append(Constants.NEWLINE);
        StringBuilder testSuite = new StringBuilder();
        testSuite.append("           EXPECT WS-88-LEVEL-ITEM TO BE TRUE");
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(expectedResult.toString(), testSourceOut.toString());
    }

    @ParameterizedTest
    @MethodSource("alphanumericLiteralEqualityCheckProvider")
    public void it_inserts_cobol_statements_for_an_alphanumeric_literal_equality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        StringBuilder testSuite = new StringBuilder();
        testSuite.append(testSuiteInput);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
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
        StringBuilder testSuite = new StringBuilder();
        testSuite.append("           EXPECT WS-MESSAGE NOT TO BE \"Hello\"");
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(ALPHANUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> alphanumericLiteralInequalityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-MESSAGE NOT TO BE \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO EQUAL \"Hello\""),
                Arguments.of("           EXPECT WS-MESSAGE NOT = \"Hello\"")
        );
    }

    @ParameterizedTest
    @MethodSource("numericLiteralEqualityCheckProvider")
    public void it_inserts_cobol_statements_for_a_numeric_literal_equality_check_in_an_EXPECT(
            String testSuiteInput) throws IOException {
        StringBuilder testSuite = new StringBuilder();
        testSuite.append(testSuiteInput);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
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
        StringBuilder testSuite = new StringBuilder();
        testSuite.append(testSuiteInput);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(NUMERIC_LITERAL_INEQUALITY_EXPECTED_RESULT.toString(), testSourceOut.toString());
    }
    private static Stream<Arguments> numericLiteralInequalityCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-VALUE NOT TO BE 18.92"),
                Arguments.of("           EXPECT WS-VALUE NOT TO EQUAL 18.92"),
                Arguments.of("           EXPECT WS-VALUE NOT = 18.92"),
                Arguments.of("           EXPECT WS-VALUE != 18.92")
        );
    }

    @Test
    public void it_inserts_cobol_statements_for_an_equality_check_between_fields_in_an_EXPECT() throws IOException {
        StringBuilder expectedResult = new StringBuilder();
        expectedResult.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           MOVE WS-FIELD-1 TO UT-ACTUAL                                         ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           MOVE WS-FIELD-2                                                      ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("               TO UT-EXPECTED                                                   ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        expectedResult.append(Constants.NEWLINE);
        expectedResult.append("           PERFORM UT-AFTER                                                     ");
        expectedResult.append(Constants.NEWLINE);
        StringBuilder testSuite = new StringBuilder();
        testSuite.append("           EXPECT WS-FIELD-1 TO EQUAL WS-FIELD-2");
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(expectedResult.toString(), testSourceOut.toString());
    }
}
