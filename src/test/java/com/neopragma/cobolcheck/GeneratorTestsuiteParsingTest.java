package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.BufferedReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

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
    public void it_stores_the_name_of_the_test_suite_after_detecting_the_TESTSUITE_keyword() {
        testSuite.append("       TESTSUITE \"Name of test suite\"");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals("\"Name of test suite\"", generator.getCurrentTestSuiteName());
    }

    @Test
    public void it_stores_the_name_of_a_test_case_after_detecting_the_TESTCASE_keyword() {
        testSuite.append("       TESTCASE \"Name of test case\"");
        generator.parseTestSuite(new BufferedReader(new StringReader(testSuite.toString())),
                mockTestProgramSource);
        assertEquals("\"Name of test case\"", generator.getCurrentTestCaseName());
    }
}
