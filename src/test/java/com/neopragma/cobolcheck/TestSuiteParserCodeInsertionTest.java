package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.testSuiteParser.BeforeAfterRepo;
import com.neopragma.cobolcheck.features.testSuiteParser.MockRepository;
import com.neopragma.cobolcheck.features.writer.CobolWriter;
import com.neopragma.cobolcheck.features.testSuiteParser.KeywordExtractor;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;
import com.neopragma.cobolcheck.features.testSuiteParser.TestSuiteParser;
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
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class TestSuiteParserCodeInsertionTest {

    Writer testSourceOut;
    CobolWriter cobolWriter;
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
    }

    MockedStatic<Config> mockedConfig;

    @BeforeEach
    public void commonSetup() {
        testSourceOut = new StringWriter();
        cobolWriter = new CobolWriter(testSourceOut);
        mockedConfig = Mockito.mockStatic(Config.class);
        mockedConfig.when(() ->Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX))
                .thenReturn("UT-");
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), new MockRepository(), new BeforeAfterRepo());
    }

    @AfterEach
    public void cleanUp(){
        mockedConfig.close();
    }

    @Test
    public void it_recognizes_the_end_of_a_user_written_cobol_statement_when_it_encounters_a_cobolcheck_keyword_that_can_follow_a_user_written_statement() {
        List<String> actualResult;
        List<String> expectedResult = new ArrayList<>();
        String testSuite = "            MOVE \"alpha\" TO WS-FIELDNAME                                           " +
                Constants.NEWLINE +
                "           EXPECT                                                                    " +
                Constants.NEWLINE;
        String expected1 = "            MOVE \"alpha\" TO WS-FIELDNAME";
        String expected2 = "           ADD 1 TO UT-TEST-CASE-COUNT";
        expectedResult.add(expected1);
        expectedResult.add(expected2);

        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite));
        actualResult = testSuiteParser.getParsedTestSuiteLines(testSuiteReader, numericFields);

        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_inserts_cobol_statements_to_display_the_testsuite_name() {
        List<String> actualResult = new ArrayList<>();
        List<String> expectedResult = new ArrayList<>();

        String expected = "      *============= \"Test Suite Name\" =============*";
        String expected1 = "           DISPLAY \"TESTSUITE:\"";
        String expected2 = "           DISPLAY \"Test Suite Name\"";
        String expected3 = "           MOVE \"Test Suite Name\"";
        String expected4 = "               TO UT-TEST-SUITE-NAME";
        expectedResult.add(expected);
        expectedResult.add(expected1);
        expectedResult.add(expected2);
        expectedResult.add(expected3);
        expectedResult.add(expected4);

        testSuiteParser.addTestSuiteNamelines("\"Test Suite Name\"", actualResult);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_inserts_cobol_statements_to_store_the_testcase_name() {
        List<String> actualResult = new ArrayList<>();
        List<String> expectedResult = new ArrayList<>();
        String expected1 = "      *-------- \"Test Case Name\"";
        String expected2 = "           MOVE SPACES";
        String expected3 = "               TO UT-TEST-CASE-NAME";
        String expected4 = "           PERFORM UT-BEFORE-EACH";
        String expected5 = "           MOVE \"Test Case Name\"";
        String expected6 = "               TO UT-TEST-CASE-NAME";
        String expected7 = "           PERFORM UT-INITIALIZE-MOCK-COUNT";
        expectedResult.add(expected1);
        expectedResult.add(expected2);
        expectedResult.add(expected3);
        expectedResult.add(expected4);
        expectedResult.add(expected5);
        expectedResult.add(expected6);
        expectedResult.add(expected7);

        testSuiteParser.addTestCaseNameLines("\"Test Case Name\"", actualResult);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_inserts_cobol_statements_to_perform_before_each_logic() {
        List<String> actualResult = new ArrayList<>();
        List<String> expectedResult = new ArrayList<>();
        String expected1 = "           MOVE SPACES";
        String expected2 = "               TO UT-TEST-CASE-NAME";
        String expected3 = "           PERFORM UT-BEFORE-EACH";
        expectedResult.add(expected1);
        expectedResult.add(expected2);
        expectedResult.add(expected3);
        testSuiteParser.addPerformBeforeEachLine(actualResult);
        assertEquals(expectedResult, actualResult);
    }

    @ParameterizedTest
    @MethodSource("expectationCheckProvider")
    public void it_inserts_cobol_statements_corresponding_to_an_EXPECT_specification(
            String testSuiteInput,
            String fieldName,
            DataType dataType,
            String expectedResult) {

        List<String> actualResult;
        List<String> expectedResultList = new ArrayList<>();

        for (String line : expectedResult.split("\n")){
            expectedResultList.add(StringHelper.removeTrailingSpaces(line));
        }

        lenient().doReturn(dataType).when(numericFields).dataTypeOf(fieldName);
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuiteInput));
        actualResult = testSuiteParser.getParsedTestSuiteLines(testSuiteReader, numericFields);
        assertEquals(expectedResultList, actualResult);
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
