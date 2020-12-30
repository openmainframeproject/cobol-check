package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class GeneratorCodeInsertionTest implements Constants {

    Writer testSourceOut;
    Generator generator;

    @Mock
    Messages messages;
    @Mock
    TokenExtractor tokenExtractor;
    @Mock
    KeywordExtractor keywordExtractor;
    @Mock
    Config config;

    @BeforeEach
    public void commonSetup() {
        testSourceOut = new StringWriter();
        generator = new Generator(messages,tokenExtractor, keywordExtractor, config);
    }

    @Test
    public void it_inserts_cobol_statements_to_display_the_testsuite_name() throws IOException {
        String expectedResult =
                "           DISPLAY SPACE                                                        " + NEWLINE
              + "           DISPLAY TESTSUITE:                                                   " + NEWLINE
              + "           DISPLAY \"Test Suite Name\"                                            " + NEWLINE
              + "           DISPLAY SPACE                                                        " + NEWLINE;
        generator.insertTestSuiteNameIntoTestSource("Test Suite Name", testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_store_the_testcase_name() throws IOException {
        String expectedResult =
                "           MOVE \"Test Case Name\"                                                " + NEWLINE
                        + "               TO UT-TEST-CASE-NAME                                             " + NEWLINE;
        generator.insertTestCaseNameIntoTestSource("Test Case Name", testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_perform_before_each_logic() throws IOException {
        String expectedResult =
                "           PERFORM UT-BEFORE                                                    " + NEWLINE;
        generator.insertPerformBeforeEachIntoTestSource(testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }
}
