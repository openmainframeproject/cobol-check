package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class GeneratorTestsuiteParsingTest {
    private Generator generator;
    KeywordExtractor keywordExtractor;
    private static final Messages messages = new Messages();
    private static final Config config = new Config(messages);
    private StringBuilder testSuite;

    @Mock
    Reader mockCobolSourceData;
    @Mock
    Writer mockTestProgramSource;

    @BeforeAll
    static void oneTimeSetup() {
        config.load("testconfig.properties");
    }

    @BeforeEach
    void commonSetup() {
        keywordExtractor = new KeywordExtractor();
        generator = new Generator(new Messages(),
                new StringTokenizerExtractor(messages),
                keywordExtractor,
                config);
        testSuite = new StringBuilder();
    }

    @Test
    public void it_stores_the_name_of_the_test_suite_after_detecting_the_TESTSUITE_keyword() throws IOException {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals("\"Name of test suite\"", generator.getCurrentTestSuiteName());
    }

    @Test
    public void it_stores_the_name_of_a_test_case_after_detecting_the_TESTCASE_keyword() throws IOException {
        testSuite.append("       TESTCASE \"Name of test case\"");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals("\"Name of test case\"", generator.getCurrentTestCaseName());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_a_single_line_with_no_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append(expectedResult);
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals(expectedResult, generator.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_a_single_line_terminated_by_a_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append(expectedResult);
        testSuite.append(".");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals(expectedResult, generator.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_multiple_lines_with_no_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append("            MOVE \n\"alpha\" \nTO WS-FIELDNAME");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals(expectedResult, generator.getCobolStatement());
    }

    @Test
    public void it_parses_a_user_written_cobol_statement_written_on_multiple_lines_terminated_by_a_period() throws IOException {
        String expectedResult = "            MOVE \"alpha\" TO WS-FIELDNAME";
        testSuite.append("            MOVE \n\"alpha\" \nTO WS-FIELDNAME.");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals(expectedResult, generator.getCobolStatement());
    }

    @Test
    public void it_recognizes_the_start_of_a_user_written_cobol_statement_when_it_encounters_a_cobol_verb() throws Exception {
        String expectedResult = "            MULTIPLY WS-TAXABLE-SUBTOTAL BY WS-SALES-TAX-RATE GIVING WS-TOTAL";
        testSuite.append("           MOVE \"first thing\" TO WS-FIELD-1\n");
        testSuite.append(expectedResult);
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals(expectedResult, generator.getCobolStatement());
    }

}
