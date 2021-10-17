package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.parser.KeywordExtractor;
import com.neopragma.cobolcheck.features.parser.NumericFields;
import com.neopragma.cobolcheck.features.parser.TestSuiteParser;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Messages;
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

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class TestSuiteParserParsingTest {
    private TestSuiteParser testSuiteParser;
    private StringBuilder testSuite;

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
        testSuiteParser = new TestSuiteParser(new KeywordExtractor());
        testSuite = new StringBuilder();
    }

    @Test
    public void it_stores_the_name_of_the_test_suite_after_detecting_the_TESTSUITE_keyword() throws IOException {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        testSuiteParser.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource, numericFields);
        assertEquals("\"Name of test suite\"", testSuiteParser.getCurrentTestSuiteName());
    }

    @Test
    public void it_stores_the_name_of_a_test_case_after_detecting_the_TESTCASE_keyword() throws IOException {
        testSuite.append("       TESTCASE \"Name of test case\"");
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals("\"Name of test case\"", testSuiteParser.getCurrentTestCaseName());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_a_single_line_with_no_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append(expectedResult);
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_a_single_line_terminated_by_a_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append(expectedResult);
        testSuite.append(".");
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_multiple_lines_with_no_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append("            MOVE \n\"alpha\" \nTO WS-FIELDNAME");
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_multiple_lines_terminated_by_a_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append("            MOVE \n\"alpha\" \nTO WS-FIELDNAME.");
        testSuiteParser.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_recognizes_the_start_of_a_user_written_cobol_statement_when_it_encounters_a_cobol_verb() throws Exception {
        String expectedResult = "            MULTIPLY WS-TAXABLE-SUBTOTAL BY WS-SALES-TAX-RATE GIVING WS-TOTAL";
        testSuite.append("           MOVE \"first thing\" TO WS-FIELD-1\n");
        testSuite.append(expectedResult);
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_captures_a_simple_item_name_from_an_EXPECT() throws Exception {
        String expectedResult = "            WS-FIELDNAME";
        testSuite.append("           EXPECT WS-FIELDNAME TO BE \"some value\"");
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }

    @Test
    public void it_captures_a_qualified_item_name_from_an_EXPECT() throws Exception {
        String expectedResult = "            WS-FIELDNAME OF WS-GROUP";
        testSuite.append("           EXPECT WS-FIELDNAME OF WS-GROUP TO BE \"some value\"");
        testSuiteParser.parseTestSuite(
                new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource,
                numericFields);
        assertEquals(expectedResult, testSuiteParser.getCobolStatement());
    }
}
