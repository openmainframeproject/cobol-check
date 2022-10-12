package org.openmainframeproject.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.openmainframeproject.cobolcheck.exceptions.TestSuiteSyntaxException;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.*;
import org.openmainframeproject.cobolcheck.features.writer.CobolWriter;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.NumericFields;

import java.io.BufferedReader;
import java.io.StringReader;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class TestSuiteErrorLogTest {
    private TestSuiteParser testSuiteParser;
    private BufferedReader mockedReader;
    private StringBuilder testSuite;
    private MockRepository mockRepository;
    private BeforeAfterRepo beforeAfterRepo;
    private TestSuiteErrorLog testSuiteErrorLog;

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
        testSuiteErrorLog = new TestSuiteErrorLog();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository, beforeAfterRepo, testSuiteErrorLog);
        mockedReader = Mockito.mock(BufferedReader.class);
        testSuite = new StringBuilder();
        cobolWriter = new CobolWriter(mockTestProgramSource);
        numericFields = new NumericFields();
    }

    @Test
    public void it_catches_unexpected_keyword() {
        testSuite.append("       TESTSUITE VERIFY");

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:1:18:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 1, index 18:" + Constants.NEWLINE;
        expectedResult += "Following <TESTSUITE> classified as <TESTSUITE>" + Constants.NEWLINE;
        expectedResult += "Expected classification: [alphanumeric-literal]" + Constants.NEWLINE;
        expectedResult += "Got <VERIFY> classified as <VERIFY>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_ends__mock_context_after_call_mock_with_no_arguments() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("           MOCK CALL 'PROG1'"+ Constants.NEWLINE);
        testSuite.append("                MOVE \"From mocked PROG1\" TO VALUE-1"+ Constants.NEWLINE);
        testSuite.append("           END-MOCK"+ Constants.NEWLINE);
        testSuite.append("           PERFORM 600-MAKE-CALL"+ Constants.NEWLINE);
        testSuite.append("           EXPECT VALUE-1 TO BE \"From mocked PROG1\""+ Constants.NEWLINE);

        String expectedResult = "";

        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())), numericFields);

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_inside_mock_block() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 000-START"+ Constants.NEWLINE);
        testSuite.append("       END-BEFORE"+ Constants.NEWLINE);
        testSuite.append("       END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:4:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 4, index  8:" + Constants.NEWLINE;
        expectedResult += "Cannot have Cobol Check keyword <END-BEFORE> inside a MOCK block" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_inside_before_each_block() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       BEFORE-EACH"+ Constants.NEWLINE);
        testSuite.append("       VERIFY SECTION 000-START HAPPENED ONCE"+ Constants.NEWLINE);
        testSuite.append("       END-BEFORE"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:4:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 4, index  8:" + Constants.NEWLINE;
        expectedResult += "Cannot have Cobol Check keyword <VERIFY> inside a BEFORE EACH block" + Constants.NEWLINE + Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:33:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 33:" + Constants.NEWLINE;
        expectedResult += "Cannot have Cobol Check keyword <HAPPENED> inside a BEFORE EACH block" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_two_identical_mocks_local() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 000-START"+ Constants.NEWLINE);
        testSuite.append("       MOVE 'hi' TO value1"+ Constants.NEWLINE);
        testSuite.append("       END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 000-START"+ Constants.NEWLINE);
        testSuite.append("       MOVE 'hi' TO value1"+ Constants.NEWLINE);
        testSuite.append("       END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "RUNTIME ERROR in file: null:6:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 6, index  8:" + Constants.NEWLINE;
        expectedResult += "Mock <000-START> already exists in this Local testcase scope" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_two_identical_mocks_global() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 000-START"+ Constants.NEWLINE);
        testSuite.append("       MOVE 'hi' TO value1"+ Constants.NEWLINE);
        testSuite.append("       END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 000-START"+ Constants.NEWLINE);
        testSuite.append("       MOVE 'hi' TO value1"+ Constants.NEWLINE);
        testSuite.append("       END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "RUNTIME ERROR in file: null:5:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 5, index  8:" + Constants.NEWLINE;
        expectedResult += "Mock <000-START> already exists in this Global testsuite scope" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_verify_with_no_mock() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       VERIFY SECTION 000-START HAPPENED ONCE"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "RUNTIME ERROR in file: null:3:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index  8:" + Constants.NEWLINE;
        expectedResult += "Verify references non existent mock. Mock does not exist for:  SECTION 000-START with no arguments" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
                    testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                            numericFields);
                });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_detects_no_errros_with_mock_block_followed_by_before_each() {
        testSuite.append("           TESTSUITE 'TEST'"+ Constants.NEWLINE);
        testSuite.append("           MOCK SECTION INC-KALD-KISM567"+ Constants.NEWLINE);
        testSuite.append("           DISPLAY 'INC-KALD-KISM567 MOCK'"+ Constants.NEWLINE);
        testSuite.append("           SET STATU-OK IN RETURKODE-AREAL IN KISM567-PARM TO TRUE"+ Constants.NEWLINE);
        testSuite.append("           END-MOCK"+ Constants.NEWLINE);
        testSuite.append(""+ Constants.NEWLINE);
        testSuite.append("           BEFORE-EACH"+ Constants.NEWLINE);
        testSuite.append("           MOVE 0 TO BANKNR IN AARM503-PARM"+ Constants.NEWLINE);
        testSuite.append("           END-BEFORE"+ Constants.NEWLINE);

        String expectedResult = "";

        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())), numericFields);

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_detects_no_errros_on_a_stub() {
        testSuite.append("           TESTSUITE 'TEST'"+ Constants.NEWLINE);
        testSuite.append("           MOCK SECTION 000-START END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";

        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())), numericFields);

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }
}
