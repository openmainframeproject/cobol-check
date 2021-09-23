package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.parser.KeywordExtractor;
import com.neopragma.cobolcheck.features.parser.NumericFields;
import com.neopragma.cobolcheck.features.parser.TestSuiteParser;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.cobolLogic.DataType;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
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

    private static StringBuilder BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT;
    private static StringBuilder NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT;

    @BeforeAll
    public static void initializeExpectedResults() {
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           SET UT-COMPARE-88-LEVEL TO TRUE                                      ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           IF WS-88-LEVEL-ITEM                                                  ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("               SET UT-ACTUAL-88-VALUE TO TRUE                                   ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("               MOVE 'TRUE' TO UT-ACTUAL                                         ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           ELSE                                                                 ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("               SET UT-ACTUAL-88-VALUE TO FALSE                                  ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("               MOVE 'FALSE' TO UT-ACTUAL                                        ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           END-IF                                                               ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           SET UT-EXPECTED-88-VALUE TO TRUE                                     ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           IF UT-EXPECTED-88-VALUE                                              ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("               MOVE 'TRUE' TO UT-EXPECTED                                       ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           ELSE                                                                 ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("               MOVE 'FALSE' TO UT-EXPECTED                                      ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append("           END-IF                                                               ");
        BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT);

        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE \"Hello\"                                                         ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT);

        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE \"Hello\"                                                         ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT);

        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-TEXT                                                         ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT);

        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-TEXT                                                         ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("               TO UT-EXPECTED                                                   ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT);

        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-NUMERIC-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-VALUE TO UT-ACTUAL-NUMERIC                                   ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           MOVE 18.92 TO UT-EXPECTED-NUMERIC                                    ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT);

        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-NUMERIC-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-VALUE TO UT-ACTUAL-NUMERIC                                   ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE 18.92 TO UT-EXPECTED-NUMERIC                                    ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT);

        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT = appendCommonFirstLines();
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           SET UT-NUMERIC-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           MOVE 18.004 TO UT-EXPECTED-NUMERIC                                   ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append("           SET UT-RELATION-GT TO TRUE                                           ");
        NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT);

        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT = appendCommonFirstLines();
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append("           SET UT-NUMERIC-COMPARE TO TRUE                                       ");
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append("           MOVE 18 TO UT-EXPECTED-NUMERIC                                       ");
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append("           SET UT-RELATION-LT TO TRUE                                           ");
        NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(NUMERIC_LITERAL_LESS_THAN_EXPECTED_RESULT);

        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-NUMERIC-COMPARE TO TRUE                                       ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-EXPECTED TO UT-EXPECTED-NUMERIC                              ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT);

        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT = appendCommonFirstLines();
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-REVERSE-COMPARE TO TRUE                                       ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-NUMERIC-COMPARE TO TRUE                                       ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-ACTUAL TO UT-ACTUAL-NUMERIC                                  ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           MOVE WS-EXPECTED TO UT-EXPECTED-NUMERIC                              ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append("           SET UT-RELATION-EQ TO TRUE                                           ");
        NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.append(Constants.NEWLINE);
        appendCommonLastLines(NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT);
    }

    private static StringBuilder appendCommonFirstLines() {
        StringBuilder sb = new StringBuilder();
        sb.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        sb.append(Constants.NEWLINE);
        return sb;
    }

    private static void appendCommonLastLines(StringBuilder sb) {
        sb.append("           PERFORM UT-CHECK-EXPECTATION                                         ");
        sb.append(Constants.NEWLINE);
        sb.append("           PERFORM UT-AFTER                                                     ");
        sb.append(Constants.NEWLINE);
    }

    MockedStatic<Config> mockedConfig;

    @BeforeEach
    public void commonSetup() {
        testSourceOut = new StringWriter();
        mockedConfig = Mockito.mockStatic(Config.class);
        mockedConfig.when(() ->Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX))
                .thenReturn("UT-");
        testSuiteParser = new TestSuiteParser(new KeywordExtractor());
    }

    @AfterEach
    public void cleanUp(){
        mockedConfig.close();
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

    @ParameterizedTest
    @MethodSource("expectationCheckProvider")
    public void it_inserts_cobol_statements_corresponding_to_an_EXPECT_specification(
            String testSuiteInput,
            String fieldName,
            DataType dataType,
            String expectedResult) throws IOException {
        lenient().doReturn(dataType).when(numericFields).dataTypeOf(fieldName);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        testSuiteParser.parseTestSuite(testSuiteReader, testSourceOut, numericFields);
        assertEquals(expectedResult, testSourceOut.toString());
    }
    private static Stream<Arguments> expectationCheckProvider() {
        return Stream.of(
                Arguments.of("           EXPECT WS-88-LEVEL-ITEM TO BE TRUE",
                             "WS-88-LEVEL-ITEM", null,
                                                 BOOLEAN_88_LEVEL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE TO BE \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE TO EQUAL \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE = \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO BE \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO EQUAL \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE NOT = \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE != \"Hello\"",
                             "WS-MESSAGE", null, ALPHANUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE TO BE WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE TO EQUAL WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE = WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO BE WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE NOT TO EQUAL WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE NOT = WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-MESSAGE != WS-TEXT",
                             "WS-MESSAGE", null, ALPHANUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE TO BE 18.92",
                             "WS-VALUE", DataType.FLOATING_POINT,
                                                 NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE TO EQUAL 18.92",
                             "WS-VALUE", DataType.PACKED_DECIMAL,
                                                 NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE = 18.92",
                             "WS-VALUE", DataType.PACKED_DECIMAL,
                                                 NUMERIC_LITERAL_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE NOT TO BE 18.92",
                             "WS-VALUE", DataType.FLOATING_POINT,
                                                 NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE NOT TO EQUAL 18.92",
                             "WS-VALUE", DataType.FLOATING_POINT,
                                                 NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE NOT = 18.92",
                             "WS-VALUE", DataType.PACKED_DECIMAL,
                                                 NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-VALUE != 18.92",
                             "WS-VALUE", DataType.FLOATING_POINT,
                                                 NUMERIC_LITERAL_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL > 18.004",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL > 18.004",
                             "WS-ACTUAL", DataType.FLOATING_POINT,
                                                 NUMERIC_LITERAL_GREATER_THAN_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL TO BE WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL TO EQUAL WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL = WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL NOT TO BE WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL NOT TO EQUAL WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL NOT = WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString()),
                Arguments.of("           EXPECT WS-ACTUAL != WS-EXPECTED",
                             "WS-ACTUAL", DataType.PACKED_DECIMAL,
                                                 NUMERIC_FIELD_NON_EQUALITY_EXPECTED_RESULT.toString())
        );
    }

}
