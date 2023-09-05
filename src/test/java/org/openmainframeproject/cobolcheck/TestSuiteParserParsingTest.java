package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.exceptions.TestSuiteAlreadyExistsException;
import org.openmainframeproject.cobolcheck.exceptions.TestCaseAlreadyExistsException;
import org.openmainframeproject.cobolcheck.exceptions.VerifyReferencesNonexistentMockException;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.*;
import org.openmainframeproject.cobolcheck.features.writer.CobolWriter;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.DataType;
import org.openmainframeproject.cobolcheck.services.cobolLogic.NumericFields;
import org.openmainframeproject.cobolcheck.services.Config;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
public class TestSuiteParserParsingTest {
    private TestSuiteParser testSuiteParser;
    private TestSuiteParserController testSuiteParserController;
    private BufferedReader mockedReader;
    private StringBuilder testSuite;
    private MockRepository mockRepository;
    private BeforeAfterRepo beforeAfterRepo;

    CobolWriter cobolWriter;
    @Mock
    Writer mockTestProgramSource;
    @Mock
    NumericFields numericFields;

    @BeforeAll
    static void oneTimeSetup() {
        Config.load("testconfig.properties");
    }

    @BeforeEach
    void commonSetup() {
        mockRepository = new MockRepository();
        beforeAfterRepo = new BeforeAfterRepo();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository, beforeAfterRepo, new TestSuiteErrorLog());
        mockedReader = Mockito.mock(BufferedReader.class);
        testSuiteParserController = new TestSuiteParserController(mockedReader);
        testSuite = new StringBuilder();
        cobolWriter = new CobolWriter(mockTestProgramSource);
        numericFields = new NumericFields();
    }
    
    @Test 
    public void multiple_testsuites_with_same_name_exists_for_a_program() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTSUITE \"Name of test suite\"";
        
        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, null);

        Throwable ex = assertThrows(TestSuiteAlreadyExistsException.class, () -> testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields));
        assertEquals("ERR031: A test suite with the name \"Name of test suite\" already exists.", ex.getMessage());
    }

    @Test 
    public void multiple_testcases_with_same_name_exists_for_a_testsuite() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       TESTCASE \"Name of test case\"";
        
        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        Throwable ex = assertThrows(TestCaseAlreadyExistsException.class, () -> testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields));
        assertEquals("ERR032: A test case with the name \"Name of test case\" already exists in the test suite \"Name of test suite\".", ex.getMessage());
    }

    @Test
    public void it_stores_the_name_of_the_test_suite_after_detecting_the_TESTSUITE_keyword() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals("\"Name of test suite\"", testSuiteParser.getCurrentTestSuiteName());
    }

    @Test
    public void it_stores_the_name_of_a_test_case_after_detecting_the_TESTCASE_keyword() {
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals("\"Name of test case\"", testSuiteParser.getCurrentTestCaseName());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_a_single_line_with_no_period() {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append(expectedResult);
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_a_single_line_terminated_by_a_period() {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append(expectedResult);
        testSuite.append(".");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_multiple_lines_with_no_period() {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append("            MOVE \n\"alpha\" \nTO WS-FIELDNAME");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_multiple_lines_terminated_by_a_period() {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append("            MOVE \n\"alpha\" \nTO WS-FIELDNAME.");
        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_recognizes_the_start_of_a_user_written_cobol_statement_when_it_encounters_a_cobol_verb() {
        String expectedResult = "            MULTIPLY WS-TAXABLE-SUBTOTAL BY WS-SALES-TAX-RATE GIVING WS-TOTAL";
        testSuite.append("           MOVE \"first thing\" TO WS-FIELD-1\n");
        testSuite.append(expectedResult);
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_accepts_greater_than_in_cobol_statement() {
        String expectedResult = "            PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 50";
        testSuite.append(expectedResult);
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_ends_cobol_statement_at_correct_cobol_verbs() {
        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("            PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 50");
        expectedResult.add("            IF VALUE1 IN TABLE1 (TALLY)");
        expectedResult.add("            SET VALUE2 IN TABLE1 (TALLY) TO TRUE");
        expectedResult.add("            END-IF");
        expectedResult.add("            END-PERFORM");
        expectedResult.add("            PERFORM 100-HELLO");

        testSuite.append("            PERFORM VARYING TALLY FROM 1 BY 1"+ Constants.NEWLINE);
        testSuite.append("               UNTIL TALLY > 50"+ Constants.NEWLINE);
        testSuite.append("                  IF VALUE1 IN TABLE1 (TALLY)" + Constants.NEWLINE);
        testSuite.append("                      SET VALUE2 IN TABLE1 (TALLY)"+ Constants.NEWLINE);
        testSuite.append("                          TO TRUE"+ Constants.NEWLINE);
        testSuite.append("                  END-IF"+ Constants.NEWLINE);
        testSuite.append("            END-PERFORM"+ Constants.NEWLINE);
        testSuite.append(" "+ Constants.NEWLINE);
        testSuite.append("            PERFORM 100-HELLO"+ Constants.NEWLINE);
        List<String> lines =  testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, lines);
    }

    @Test
    public void it_captures_a_simple_item_name_from_an_EXPECT() {
        String expectedResult = "MOVE WS-FIELDNAME TO UT-ACTUAL";
        testSuite.append("           EXPECT WS-FIELDNAME TO BE \"some value\"");
        List<String> lines = testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, lines.get(3).trim());
    }

    @Test
    public void it_captures_a_qualified_item_name_from_an_EXPECT() {
        String expectedResult = "MOVE WS-FIELDNAME OF WS-GROUP TO UT-ACTUAL";
        testSuite.append("           EXPECT WS-FIELDNAME OF WS-GROUP TO BE \"some value\"");
        List<String> lines = testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, lines.get(3).trim());
    }

    @Test
    public void it_accepts_parenthesis_in_item_in_EXPECT() {
        String expectedResult = "MOVE TABLE-FIELD IN WS-FIELDS (2 3) TO UT-ACTUAL";
        testSuite.append("           EXPECT TABLE-FIELD IN WS-FIELDS (2 3) TO BE \"some value\"");
        List<String> lines = testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, lines.get(3).trim());
    }

    @Test
    public void it_finds_correct_data_type_for_table() {
        String expectedLine = "           SET UT-NUMERIC-COMPARE TO TRUE";
        testSuite.append("           EXPECT TABLE-FIELD IN WS-FIELDS (2 3) TO BE \"some value\"");
        numericFields.setDataTypeOf("TABLE-FIELD,WS-FIELDS", DataType.DISPLAY_NUMERIC);
        List<String> actual = testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedLine, actual.get(2));
    }

    @Test
    public void it_generates_lines_correctly_for_empty_testcase_followed_by_testcase() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       TESTCASE \"Name of test case1\"");
        testSuite.append( "       PERFORM 000-START");
        testSuite.append( "       EXPECT value-1 TO BE 'hi'");

        List<String> actualResult = testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertTrue(!actualResult.contains("            \"Name of test case1\""));
    }


    @Test
    public void it_generates_lines_for_verify_statement_with_exact_comparison() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       PERFORM 000-START");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED 1 TIME");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_lines_for_verify_statement_with_at_least_comparison() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       PERFORM 000-START");
        testSuite.append( "       PERFORM 000-START");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED AT LEAST ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-AT-LEAST TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_lines_for_verify_statement_with_no_more_than_comparison() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       PERFORM 000-START");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED NO MORE THAN 2 TIMES");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 2 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-NO-MORE-THAN TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_lines_for_verify_never_happened_statement() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       VERIFY SECTION 000-START NEVER HAPPENED");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 0 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_lines_for_verify_no_more_than_once_statement() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED NO MORE THAN ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-NO-MORE-THAN TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_lines_for_verify_happened_once_statement() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void if_local_and_global_mock_exists_in_scope_verify_attaches_to_local() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"global\" TO this");
        testSuite.append("          MOVE \"global, global\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED ONCE");

        //Global mock would be named UT-1-0-1-MOCK
        String expectedFirstLine = "           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED";

        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedFirstLine, actualResult.get(0));
    }

    @Test
    public void verify_can_attach_to_global_mock() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append( "       PERFORM 000-START");
        testSuite.append( "       VERIFY SECTION 000-START HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-0-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-0-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-0-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-0-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void verify_can_attach_to_call_mock_with_no_arguments() {
        testSuite.append("       TESTSUITE \"Name of test suite\"" + Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'PROG1'"+ Constants.NEWLINE);
        testSuite.append("          MOVE \"something\" TO this"+ Constants.NEWLINE);
        testSuite.append("          MOVE \"something else\" TO other"+ Constants.NEWLINE);
        testSuite.append("       END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       PERFORM 000-START"+ Constants.NEWLINE);
        testSuite.append("       VERIFY CALL 'PROG1' HAPPENED ONCE"+ Constants.NEWLINE);

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void verify_can_attach_to_call_mock_with_arguments() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this-default, BY CONTENT other-by-content");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this-default, BY CONTENT other-by-content");
        testSuite.append("             HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }


    @Test
    public void verify_can_attach_to_call_mock_with_one_qualified_argument() {
        testSuite.append("       TESTSUITE \"call_mock_with_arguments\"");
        testSuite.append("       TESTCASE \"qualified_arguments\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this in this-place");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this in this-place");
        testSuite.append("             HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void verify_can_attach_to_call_mock_with_qualified_arguments() {
        testSuite.append("       TESTSUITE \"call_mock_with_arguments\"");
        testSuite.append("       TESTCASE \"qualified_arguments\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this in this-place, BY CONTENT other in this-place");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this in this-place, BY CONTENT other in this-place");
        testSuite.append("             HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void verify_missing_call_mock_with_qualified_arguments() {
        testSuite.append("       TESTSUITE \"call_mock_with_arguments\"");
        testSuite.append("       TESTCASE \"qualified_arguments\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this in this-place, BY CONTENT other in this-place");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this in new-place, BY CONTENT other in new-place");
        testSuite.append("             HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");

        Throwable ex = assertThrows(VerifyReferencesNonexistentMockException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),numericFields);
            List<String> actualResult = new ArrayList<>();
            testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker()); });

        assertTrue(ex.getMessage().indexOf("nonexistent mock")>-1);
    }

    @Test
    public void verify_missing_call_mock_with_qualified_arguments_2() {
        testSuite.append("       TESTSUITE \"call_mock_with_arguments\"");
        testSuite.append("       TESTCASE \"qualified_arguments\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this in this-place, BY CONTENT other in this-place");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this in this-place, BY CONTENT other in this-place new-surprise-var");
        testSuite.append("             HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");

        Throwable ex = assertThrows(VerifyReferencesNonexistentMockException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),numericFields);
            List<String> actualResult = new ArrayList<>();
            testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker()); });

        assertTrue(ex.getMessage().indexOf("nonexistent mock")>-1);
    }


    @Test
    public void verify_call_mock_with_qualified_arguments_mixed() {
        testSuite.append("       TESTSUITE \"call_mock_with_arguments\"");
        testSuite.append("       TESTCASE \"qualified_arguments and not qualified\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this in this-place, BY CONTENT other in this-place last-one");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this in new-place BY CONTENT other in new-place last-one");
        testSuite.append("             HAPPENED ONCE");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");

        Throwable ex = assertThrows(VerifyReferencesNonexistentMockException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),numericFields);
            List<String> actualResult = new ArrayList<>();
            testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker()); });

        assertTrue(ex.getMessage().indexOf("nonexistent mock")>-1);
    }

    @Test
    public void it_throws_if_the_mock_that_verify_references_does_not_exist() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK SECTION 000-START");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append( "       END-MOCK");
        testSuite.append( "       VERIFY SECTION 100-WELCOME HAPPENED ONCE");

        String expectedResult = "Cannot verify nonexistent mock for: SECTION 100-WELCOME in the scope of testsuite:" +
                " \"Name of test suite\", testcase: \"Name of test case\"";

        Throwable ex = assertThrows(VerifyReferencesNonexistentMockException.class, () ->
                testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                        numericFields));
        assertEquals(expectedResult, ex.getMessage());
    }

    @Test
    public void it_generates_before_each_paragraph_correctly() throws IOException {
        String str1 = "       TESTSUITE \"TestSuite1\"";
        String str2 = "            BEFORE EACH";
        String str3 = "                MOVE \"prepare\" TO VALUE-1";
        String str4 = "            END-BEFORE";
        String str5 = "       TESTCASE \"TestCase1\"";
        String str6 = "            MOVE \"hello\" TO VALUE-1";
        String str7 = "       TESTSUITE \"TestSuite2\"";
        String str8 = "            BEFORE EACH";
        String str9 = "                MOVE \"prepare\" TO VALUE-1";
        String str10 = "            END-BEFORE";
        String str11 = "       TESTCASE \"TestCase2\"";
        String str12 = "            MOVE \"hello\" TO VALUE-1";
        String str13 = "       TESTSUITE \"TestSuite3\"";
        String str14 = "            BEFORE EACH";
        String str15 = "                MOVE \"prepare\" TO VALUE-1";
        String str16 = "            END-BEFORE";
        String str17 = "       TESTCASE \"TestCase3\"";
        String str18 = "            MOVE \"hello\" TO VALUE-1";

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("       UT-BEFORE-EACH.");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("      *This is performed before each Test Case");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("            EVALUATE UT-TEST-SUITE-NAME");
        expectedResult.add("                WHEN \"TestSuite1\"");
        expectedResult.add("           PERFORM UT-BEFORE-EACH-BRANCH-1");
        expectedResult.add("                WHEN \"TestSuite2\"");
        expectedResult.add("           PERFORM UT-BEFORE-EACH-BRANCH-2");
        expectedResult.add("                WHEN \"TestSuite3\"");
        expectedResult.add("           PERFORM UT-BEFORE-EACH-BRANCH-3");
        expectedResult.add("            END-EVALUATE");
        expectedResult.add("           .");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, str8, str9,
                str10, str11, str12, str13, str14, str15, str16, str17, str18, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actualResult = testSuiteParserController.generateBeforeParagraph();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_after_each_paragraph_correctly() throws IOException {
        String str1 = "       TESTSUITE \"TestSuite1\"";
        String str2 = "            AFTER EACH";
        String str3 = "                MOVE \"cleanup\" TO VALUE-1";
        String str4 = "            END-AFTER";
        String str5 = "       TESTCASE \"TestCase1\"";
        String str6 = "            MOVE \"hello\" TO VALUE-1";
        String str7 = "       TESTSUITE \"TestSuite2\"";
        String str8 = "            AFTER EACH";
        String str9 = "                MOVE \"cleanup\" TO VALUE-1";
        String str10 = "            END-AFTER";
        String str11 = "       TESTCASE \"TestCase2\"";
        String str12 = "            MOVE \"hello\" TO VALUE-1";
        String str13 = "       TESTSUITE \"TestSuite3\"";
        String str14 = "            AFTER EACH";
        String str15 = "                MOVE \"cleanup\" TO VALUE-1";
        String str16 = "            END-AFTER";
        String str17 = "       TESTCASE \"TestCase3\"";
        String str18 = "            MOVE \"hello\" TO VALUE-1";

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("       UT-AFTER-EACH.");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("      *This is performed after each Test Case");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("            EVALUATE UT-TEST-SUITE-NAME");
        expectedResult.add("                WHEN \"TestSuite1\"");
        expectedResult.add("           PERFORM UT-AFTER-EACH-BRANCH-1");
        expectedResult.add("                WHEN \"TestSuite2\"");
        expectedResult.add("           PERFORM UT-AFTER-EACH-BRANCH-2");
        expectedResult.add("                WHEN \"TestSuite3\"");
        expectedResult.add("           PERFORM UT-AFTER-EACH-BRANCH-3");
        expectedResult.add("            END-EVALUATE");
        expectedResult.add("           .");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, str8, str9,
                str10, str11, str12, str13, str14, str15, str16, str17, str18, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actualResult = testSuiteParserController.generateAfterParagraph();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_after_each_branching_paragraphs_correctly_with_comments() throws IOException {
        String str1 = "       TESTSUITE \"TestSuite1\"";
        String str2 = "            BEFORE EACH";
        String str3 = "                MOVE \"prepare\" TO VALUE-1";
        String str4 = "            END-BEFORE";
        String str5 = "            AFTER EACH";
        String str6 = "                MOVE \"cleanup\" TO VALUE-1";
        String str7 = "            END-AFTER";
        String str8 = "       TESTCASE \"TestCase1\"";
        String str9 = "            MOVE \"hello\" TO VALUE-1";

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("       UT-BEFORE-EACH-BRANCH-1.");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("      *Called before Test Cases in Test Suite:");
        expectedResult.add("      *\"TestSuite1\"");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("                MOVE \"prepare\" TO VALUE-1");
        expectedResult.add("           .");
        expectedResult.add("");
        expectedResult.add("       UT-AFTER-EACH-BRANCH-1.");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("      *Called after Test Cases in Test Suite:");
        expectedResult.add("      *\"TestSuite1\"");
        expectedResult.add("      *****************************************************************");
        expectedResult.add("                MOVE \"cleanup\" TO VALUE-1");
        expectedResult.add("           .");
        expectedResult.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actualResult = testSuiteParserController.generateBeforeAfterBranchParagraphs(true);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_generates_after_each_branching_paragraphs_correctly_no_comments() throws IOException {
        String str1 = "       TESTSUITE \"TestSuite1\"";
        String str2 = "            BEFORE EACH";
        String str3 = "                MOVE \"test1B\" TO VALUE-1";
        String str4 = "            END-BEFORE";
        String str5 = "            AFTER EACH";
        String str6 = "                MOVE \"test1A\" TO VALUE-1";
        String str7 = "            END-AFTER";
        String str8 = "       TESTCASE \"TestCase1\"";
        String str9 = "            MOVE \"hello\" TO VALUE-1";
        String str10 = "       TESTSUITE \"TestSuite2\"";
        String str11 = "            AFTER EACH";
        String str12 = "                MOVE \"test2A\" TO VALUE-1";
        String str13 = "            END-AFTER";
        String str14 = "       TESTCASE \"TestCase2\"";
        String str15 = "            MOVE \"hello\" TO VALUE-1";
        String str16 = "       TESTSUITE \"TestSuite3\"";
        String str17 = "            BEFORE EACH";
        String str18 = "                MOVE \"test3B\" TO VALUE-1";
        String str19 = "            END-BEFORE";
        String str20 = "       TESTCASE \"TestCase3\"";
        String str21 = "            MOVE \"hello\" TO VALUE-1";

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("       UT-BEFORE-EACH-BRANCH-1.");
        expectedResult.add("                MOVE \"test1B\" TO VALUE-1");
        expectedResult.add("           .");
        expectedResult.add("");
        expectedResult.add("       UT-BEFORE-EACH-BRANCH-3.");
        expectedResult.add("                MOVE \"test3B\" TO VALUE-1");
        expectedResult.add("           .");
        expectedResult.add("");
        expectedResult.add("       UT-AFTER-EACH-BRANCH-1.");
        expectedResult.add("                MOVE \"test1A\" TO VALUE-1");
        expectedResult.add("           .");
        expectedResult.add("");
        expectedResult.add("       UT-AFTER-EACH-BRANCH-2.");
        expectedResult.add("                MOVE \"test2A\" TO VALUE-1");
        expectedResult.add("           .");
        expectedResult.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, str8, str9,
                str10, str11, str12, str13, str14, str15, str16, str17, str18, str19, str20, str21, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actualResult = testSuiteParserController.generateBeforeAfterBranchParagraphs(false);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_parses_testsuite_with_sequnece_area() {
        testSuite.append("      *From file: C:doesNotExist.cut\n");
        testSuite.append("000100 TESTSUITE \"Name of test suite\"\n");
        testSuite.append("000200 TESTCASE \"Name of test case\"\n");
        testSuite.append("000300 MOCK CALL 'PROG1' USING this, BY CONTENT other\n");
        testSuite.append("000400    MOVE \"something\" TO this\n");
        testSuite.append("000500    MOVE \"something else\" TO other\n");
        testSuite.append("000600 END-MOCK\n");
        testSuite.append("000700*Just a comment\n");
        testSuite.append("000800 PERFORM 000-START\n");
        testSuite.append("000900 VERIFY CALL 'PROG1' USING\n");
        testSuite.append("001000       this, BY CONTENT other\n");
        testSuite.append("001100       HAPPENED ONCE\n");

        List<String> expectedResult = new ArrayList<>();
        expectedResult.add("           MOVE 1 TO UT-1-1-1-MOCK-EXPECTED");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES");
        expectedResult.add("           MOVE UT-1-1-1-MOCK-NAME TO UT-MOCK-OPERATION");
        expectedResult.add("           SET UT-VERIFY-EXACT TO TRUE");
        expectedResult.add("           ADD 1 TO UT-TEST-CASE-COUNT");
        expectedResult.add("           PERFORM UT-ASSERT-ACCESSES");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        List<String> actualResult = new ArrayList<>();
        testSuiteParser.handleEndOfVerifyStatement(actualResult,new TokenTracker());
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_inserts_start_and_end_tag_as_comments() throws IOException {
        String str1 = "       TESTSUITE \"TestSuite1\"";
        String str2 = "       TESTCASE \"TestCase1\"";
        String str3 = "            MOVE \"hello\" TO VALUE-1";
        String str4 = "            EXPECT VALUE-1 TO BE \"hello\"";


        String expectedStartTag = "      *###INJECT START###";
        String expectedEndTag = "      *###INJECT END###";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, null);

        List<String> actualResult = new ArrayList<>();

        try (MockedStatic<Config> configMockedStatic = Mockito.mockStatic(Config.class)) {
            configMockedStatic.when(Config::getInjectStartTag).thenReturn("###INJECT START###");
            configMockedStatic.when(Config::getInjectEndTag).thenReturn("###INJECT END###");

            testSuiteParserController.parseTestSuites(numericFields);
            actualResult = testSuiteParserController.getProcedureDivisionTestCode();
        }

        assertEquals(expectedStartTag, actualResult.get(0));
        assertEquals(expectedEndTag, actualResult.get(actualResult.size() - 1));
    }

    @Test
    public void test_stop_method() throws IOException {
        assertTrue(testSuiteParser.containStopValue("string,"));
        assertFalse(testSuiteParser.containStopValue("string"));
    }
}
