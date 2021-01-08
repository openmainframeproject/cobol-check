package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

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
        when(config.getString(RESOURCES_DIRECTORY_CONFIG_KEY))
                .thenReturn("src/test/resources");
        when(config.getString(COBOLCHECK_COPYBOOK_DIRECTORY_CONFIG_KEY))
                .thenReturn("copybooks");
        when(config.getString(COBOLCHECK_PREFIX_CONFIG_KEY, DEFAULT_COBOLCHECK_PREFIX))
                .thenReturn("UT-");
        generator = new Generator(messages,tokenExtractor, keywordExtractor, config);
    }

    @Test
    public void it_recognizes_the_end_of_a_user_written_cobol_statement_when_it_encounters_a_cobolcheck_keyword_that_can_follow_a_user_written_statement() throws IOException {
        generator = generatorWithKeywordExtractor();
        StringBuffer expectedResult = new StringBuffer();
        expectedResult.append("            MOVE \"alpha\" TO WS-FIELDNAME                                        ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        expectedResult.append(NEWLINE);
        StringBuffer testSuite = new StringBuffer();
        testSuite.append("            MOVE \"alpha\" TO WS-FIELDNAME                                           ");
        testSuite.append(NEWLINE);
        testSuite.append("           EXPECT                                                                    ");
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        generator.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(expectedResult.toString(), testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_display_the_testsuite_name() throws IOException {
        String expectedResult =
                "           DISPLAY \"TESTSUITE:\"                                                 " + NEWLINE
              + "           DISPLAY \"Test Suite Name\"                                            " + NEWLINE;
        generator.insertTestSuiteNameIntoTestSource("\"Test Suite Name\"", testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_store_the_testcase_name() throws IOException {
        String expectedResult =
                "           MOVE \"Test Case Name\"                                                " + NEWLINE
              + "               TO UT-TEST-CASE-NAME                                             " + NEWLINE
              + "           PERFORM UT-BEFORE                                                    " + NEWLINE;

        generator.insertTestCaseNameIntoTestSource("\"Test Case Name\"", testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_to_perform_before_each_logic() throws IOException {
        String expectedResult =
                "           PERFORM UT-BEFORE                                                    " + NEWLINE;
        generator.insertPerformBeforeEachIntoTestSource(testSourceOut);
        assertEquals(expectedResult, testSourceOut.toString());
    }

    @Test
    public void it_inserts_cobol_statements_for_an_alphanumeric_literal_equality_check_in_an_EXPECT() throws IOException {
        generator = generatorWithKeywordExtractor();
        StringBuffer expectedResult = new StringBuffer();
        expectedResult.append("           ADD 1 TO UT-TEST-CASE-COUNT                                          ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           SET UT-NORMAL-COMPARE TO TRUE                                        ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           MOVE WS-MESSAGE TO UT-ACTUAL                                         ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           MOVE \"Hello\"                                                         ");
        expectedResult.append(NEWLINE);
        expectedResult.append("               TO UT-EXPECTED                                                   ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           SET UT-COMPARE-DEFAULT TO TRUE                                       ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           PERFORM UT-ASSERT-EQUAL                                              ");
        expectedResult.append(NEWLINE);
        expectedResult.append("           PERFORM UT-AFTER                                                     ");
        expectedResult.append(NEWLINE);
        StringBuffer testSuite = new StringBuffer();
        testSuite.append("           EXPECT WS-MESSAGE TO BE \"Hello\"");
        BufferedReader testSuiteReader = new BufferedReader(new StringReader(testSuite.toString()));
        generator.parseTestSuite(testSuiteReader, testSourceOut);
        assertEquals(expectedResult.toString(), testSourceOut.toString());
    }

    private Generator generatorWithKeywordExtractor() {
        return new Generator(messages, tokenExtractor, new KeywordExtractor(), config);

    }

}
