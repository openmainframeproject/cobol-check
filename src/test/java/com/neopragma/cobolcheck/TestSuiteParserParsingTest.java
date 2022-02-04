package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.exceptions.VerifyReferencesNonexistentMockException;
import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandler;
import com.neopragma.cobolcheck.features.testSuiteParser.MockRepository;
import com.neopragma.cobolcheck.features.writer.CobolWriter;
import com.neopragma.cobolcheck.features.testSuiteParser.KeywordExtractor;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;
import com.neopragma.cobolcheck.features.testSuiteParser.TestSuiteParser;
import com.neopragma.cobolcheck.services.Config;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
public class TestSuiteParserParsingTest {
    private TestSuiteParser testSuiteParser;
    private StringBuilder testSuite;
    private MockRepository mockRepository;

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
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository);
        testSuite = new StringBuilder();
        cobolWriter = new CobolWriter(mockTestProgramSource);
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
    public void it_captures_a_simple_item_name_from_an_EXPECT() {
        String expectedResult = "            WS-FIELDNAME";
        testSuite.append("           EXPECT WS-FIELDNAME TO BE \"some value\"");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_captures_a_qualified_item_name_from_an_EXPECT() {
        String expectedResult = "            WS-FIELDNAME OF WS-GROUP";
        testSuite.append("           EXPECT WS-FIELDNAME OF WS-GROUP TO BE \"some value\"");
        testSuiteParser.getParsedTestSuiteLines(
                new BufferedReader(new StringReader(testSuite.toString())),
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void verify_can_attach_to_call_mock_with_no_arguments() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK CALL 'PROG1'");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' HAPPENED ONCE");

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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void verify_can_attach_to_call_mock_with_arguments() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuite.append("       MOCK CALL 'PROG1' USING this, BY CONTENT other");
        testSuite.append("          MOVE \"something\" TO this");
        testSuite.append("          MOVE \"something else\" TO other");
        testSuite.append("       END-MOCK");
        testSuite.append("       PERFORM 000-START");
        testSuite.append("       VERIFY CALL 'PROG1' USING");
        testSuite.append("             this, BY CONTENT other");
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
        testSuiteParser.handleEndOfVerifyStatement(actualResult);
        assertEquals(expectedResult, actualResult);
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




}
