package org.openmainframeproject.cobolcheck;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.exceptions.TestCaseAlreadyExistsException;
import org.openmainframeproject.cobolcheck.exceptions.TestSuiteSyntaxException;
import org.openmainframeproject.cobolcheck.features.interpreter.InterpreterController;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.*;
import org.openmainframeproject.cobolcheck.features.writer.CobolWriter;
import org.openmainframeproject.cobolcheck.features.writer.WriterController;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.DataType;
import org.openmainframeproject.cobolcheck.services.cobolLogic.NumericFields;
import org.openmainframeproject.cobolcheck.workers.Generator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TestSuiteErrorLogTest {
    private Generator generator;
    private TestSuiteParserController testSuiteParserController;
    private BufferedReader mockedParserReader;
    private InterpreterController interpreterController;
    private BufferedReader mockedInterpreterReader;
    private WriterController writerController;
    private Writer writer;
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
        mockedInterpreterReader = Mockito.mock(BufferedReader.class);
        interpreterController = new InterpreterController(mockedInterpreterReader);

        writer = new StringWriter();
        cobolWriter = new CobolWriter(writer);
        writerController = new WriterController(cobolWriter);

        mockedParserReader = Mockito.mock(BufferedReader.class);
        testSuiteParserController = new TestSuiteParserController(mockedParserReader);

        mockRepository = new MockRepository();
        beforeAfterRepo = new BeforeAfterRepo();
        testSuiteErrorLog = new TestSuiteErrorLog();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository, beforeAfterRepo, testSuiteErrorLog);
        mockedReader = Mockito.mock(BufferedReader.class);
        testSuite = new StringBuilder();
        cobolWriter = new CobolWriter(mockTestProgramSource);
        numericFields = new NumericFields();
        ContextHandler.forceContextExit();
    }

    @AfterAll
    static void cleanup(){
        ContextHandler.forceContextExit();
    }

    @Test
    public void it_catches_unmocked_calls_from_paragraph() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       01  FILLER.";
        String s3 = "          05  VALUE-1           PIC X(80).";
        String s4 = "       PROCEDURE DIVISION.";
        String s5 = "       100-MAKE-CALL.";
        String s6 = "           CALL 'PROG'";
        String s7 = "           .";

        String t1 = "           TestSuite \"Sample TestSuite\"";
        String t2 = "           TestCase \"Sample TestCase\"";
        String t3 = "           PERFORM 100-MAKE-CALL";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, null);

        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> new Generator(interpreterController, writerController, testSuiteParserController));
        assertTrue(ex.getMessage().contains("ERR033: Call Statement in Line 6 of the source code is not mocked in testcase \"Sample TestCase\" of testSuite \"Sample TestSuite\"."));
    }

    @Test
    public void it_catches_unmocked_calls_from_section() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       01  FILLER.";
        String s3 = "          05  VALUE-1           PIC X(80).";
        String s4 = "       PROCEDURE DIVISION.";
        String s5 = "       100-MAKE-CALL SECTION.";
        String s6 = "           CALL 'PROG'";
        String s7 = "           .";

        String t1 = "           TestSuite \"Sample TestSuite\"";
        String t2 = "           TestCase \"Sample TestCase\"";
        String t3 = "           PERFORM 100-MAKE-CALL";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, null);

        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> new Generator(interpreterController, writerController, testSuiteParserController));
        assertTrue(ex.getMessage().contains("ERR033: Call Statement in Line 6 of the source code is not mocked in testcase \"Sample TestCase\" of testSuite \"Sample TestSuite\"."));
    }

    @Test
    public void calls_are_considered_mocked_in_mocked_paragraph() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       01  FILLER.";
        String s3 = "          05  VALUE-1           PIC X(80).";
        String s4 = "       PROCEDURE DIVISION.";
        String s5 = "       100-MAKE-CALL.";
        String s6 = "           CALL 'PROG'";
        String s7 = "           .";

        String t1 = "           TestSuite \"Sample TestSuite\"";
        String t2 = "           TestCase \"Sample TestCase\"";
        String t3 = "           MOCK PARAGRAPH 100-MAKE-CALL";
        String t4 = "               MOVE \"From mocked paragraph\" TO VALUE-1";
        String t5 = "           END-MOCK";
        String t6 = "           PERFORM 100-MAKE-CALL";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, t4, t5, t6, null);

        assertDoesNotThrow(() -> new Generator(interpreterController, writerController, testSuiteParserController));
    }

    @Test
    public void calls_are_considered_mocked_in_mocked_section() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       01  FILLER.";
        String s3 = "          05  VALUE-1           PIC X(80).";
        String s4 = "       PROCEDURE DIVISION.";
        String s5 = "       100-MAKE-CALL SECTION.";
        String s6 = "           CALL 'PROG'";
        String s7 = "           .";

        String t1 = "           TestSuite \"Sample TestSuite\"";
        String t2 = "           TestCase \"Sample TestCase\"";
        String t3 = "           MOCK SECTION 100-MAKE-CALL";
        String t4 = "               MOVE \"From mocked paragraph\" TO VALUE-1";
        String t5 = "           END-MOCK";
        String t6 = "           PERFORM 100-MAKE-CALL";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, t4, t5, t6, null);

        assertDoesNotThrow(() -> new Generator(interpreterController, writerController, testSuiteParserController));
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
        expectedResult += "SYNTAX ERROR in file: null:2:42:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 2, index 42:" + Constants.NEWLINE;
        expectedResult += "Cannot have Cobol Check keyword <ONCE> inside a BEFORE EACH block" + Constants.NEWLINE + Constants.NEWLINE;

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

    @Test
    public void it_catches_unexpected_keyword_in_a_mock_context() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 2 100-HELLO END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:21:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 21:" + Constants.NEWLINE;
        expectedResult += "Following <SECTION> classified as <mock-type>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK: [fieldname, alphanumeric-literal]" + Constants.NEWLINE;
        expectedResult += "Got <    2> classified as <numeric-literal>" + Constants.NEWLINE + Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:23:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 23:" + Constants.NEWLINE;
        expectedResult += "Following <2> classified as <numeric-literal>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK:   []" + Constants.NEWLINE;
        expectedResult += "Got <100-HELLO> classified as <fieldname>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_at_the_end_of_mock_context() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK SECTION 100-HELLO HAPPENED END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:2:13:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 2, index 13:" + Constants.NEWLINE;
        expectedResult += "Cannot have Cobol Check keyword <HAPPENED> inside a MOCK block" + Constants.NEWLINE+ Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_at_the_end_of_mock_context_with_arguments_and_commas_1() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'value' USING BY CONTENT VALUE-1, VALUE-2 ONCE END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:60:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 60:" + Constants.NEWLINE;
        expectedResult += "Following <VALUE-2> classified as <fieldname>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK: [END-MOCK, fieldname, BY REFERENCE, BY CONTENT, BY VALUE, USING]" + Constants.NEWLINE;
        expectedResult += "Got < ONCE> classified as <  ONCE>" + Constants.NEWLINE+ Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:11:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 11:" + Constants.NEWLINE;
        expectedResult += "Following <ONCE> classified as <ONCE>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK:   []" + Constants.NEWLINE;
        expectedResult += "Got <END-MOCK> classified as <END-MOCK>" + Constants.NEWLINE+ Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_at_the_end_of_mock_context_with_arguments_and_commas_2() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'value' USING BY CONTENT VALUE-1, VALUE-2 VERIFY END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:60:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 60:" + Constants.NEWLINE;
        expectedResult += "Following <VALUE-2> classified as <fieldname>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK: [END-MOCK, fieldname, BY REFERENCE, BY CONTENT, BY VALUE, USING]" + Constants.NEWLINE;
        expectedResult += "Got <VERIFY> classified as <VERIFY>" + Constants.NEWLINE+ Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:11:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 11:" + Constants.NEWLINE;
        expectedResult += "Following <VERIFY> classified as <VERIFY>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK:   []" + Constants.NEWLINE;
        expectedResult += "Got <END-MOCK> classified as <END-MOCK>" + Constants.NEWLINE+ Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_at_the_end_of_mock_context_with_arguments_without_commas_1() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'value' USING BY CONTENT VALUE-1 VALUE-2 ONCE END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:59:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 59:" + Constants.NEWLINE;
        expectedResult += "Following <VALUE-2> classified as <fieldname>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK: [END-MOCK, fieldname, BY REFERENCE, BY CONTENT, BY VALUE, USING]" + Constants.NEWLINE;
        expectedResult += "Got < ONCE> classified as <  ONCE>" + Constants.NEWLINE+ Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:11:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 11:" + Constants.NEWLINE;
        expectedResult += "Following <ONCE> classified as <ONCE>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK:   []" + Constants.NEWLINE;
        expectedResult += "Got <END-MOCK> classified as <END-MOCK>" + Constants.NEWLINE+ Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_at_the_end_of_mock_context_with_arguments_without_commas_2() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'value' USING BY CONTENT VALUE-1 VALUE-2 VERIFY END-MOCK"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:59:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 59:" + Constants.NEWLINE;
        expectedResult += "Following <VALUE-2> classified as <fieldname>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK: [END-MOCK, fieldname, BY REFERENCE, BY CONTENT, BY VALUE, USING]" + Constants.NEWLINE;
        expectedResult += "Got <VERIFY> classified as <VERIFY>" + Constants.NEWLINE+ Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:11:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 11:" + Constants.NEWLINE;
        expectedResult += "Following <VERIFY> classified as <VERIFY>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of MOCK:   []" + Constants.NEWLINE;
        expectedResult += "Got <END-MOCK> classified as <END-MOCK>" + Constants.NEWLINE+ Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_in_an_expect_context() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       EXPECT WS-HELLO HAPPENED ONCE"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:24:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 24:" + Constants.NEWLINE;
        expectedResult += "Following <WS-HELLO> classified as <fieldname>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of EXPECT: [TO BE, =, TO EQUAL, NOT, <, !=, <, =, >, " +
                ">=, <=, alphanumeric-literal, fieldname, qualified-field-name, parenthesis-enclosed]" + Constants.NEWLINE;
        expectedResult += "Got <HAPPENED> classified as <HAPPENED>" + Constants.NEWLINE + Constants.NEWLINE;
        expectedResult += "SYNTAX ERROR in file: null:3:33:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 33:" + Constants.NEWLINE;
        expectedResult += "Following <HAPPENED> classified as <HAPPENED>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of EXPECT:   []" + Constants.NEWLINE;
        expectedResult += "Got < ONCE> classified as <  ONCE>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_in_a_verify_context() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       VERIFY CALL MOVE 'PROG3' HAPPENED ONCE"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:3:25:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 25:" + Constants.NEWLINE;
        expectedResult += "Following <MOVE> classified as <fieldname>" + Constants.NEWLINE;
        expectedResult += "Expected classification in the context of VERIFY: [fieldname, BY REFERENCE, BY CONTENT, BY VALUE, USING, HAPPENED, NEVER HAPPENED]" + Constants.NEWLINE;
        expectedResult += "Got <'PROG3'> classified as <alphanumeric-literal>" + Constants.NEWLINE + Constants.NEWLINE;
        expectedResult += "RUNTIME ERROR in file: null:3:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index  8:" + Constants.NEWLINE;
        expectedResult += "Verify references non existent mock. Mock does not exist for:  CALL MOVE with no arguments" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_after_verify() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'PROG3' END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       VERIFY CALL 'PROG3' HAPPENED ONCE"+ Constants.NEWLINE);
        testSuite.append("       BEFORE EACH"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:5:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 5, index  8:" + Constants.NEWLINE;
        expectedResult += "Following <ONCE> classified as <ONCE>" + Constants.NEWLINE;
        expectedResult += "Expected classification: [cobol-token, TESTSUITE, TESTCASE, MOCK, VERIFY, EXPECT]" + Constants.NEWLINE;
        expectedResult += "Got <BEFORE EACH> classified as <BEFORE EACH>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_after_verify_() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'PROG3' END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       VERIFY CALL 'PROG3' HAPPENED ONCE"+ Constants.NEWLINE);
        testSuite.append("       BEFORE EACH"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:5:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 5, index  8:" + Constants.NEWLINE;
        expectedResult += "Following <ONCE> classified as <ONCE>" + Constants.NEWLINE;
        expectedResult += "Expected classification: [cobol-token, TESTSUITE, TESTCASE, MOCK, VERIFY, EXPECT]" + Constants.NEWLINE;
        expectedResult += "Got <BEFORE EACH> classified as <BEFORE EACH>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_after_verify_without_commas() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'PROG3' END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       VERIFY CALL 'PROG3' HAPPENED ONCE"+ Constants.NEWLINE);
        testSuite.append("       BEFORE EACH"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:5:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 5, index  8:" + Constants.NEWLINE;
        expectedResult += "Following <ONCE> classified as <ONCE>" + Constants.NEWLINE;
        expectedResult += "Expected classification: [cobol-token, TESTSUITE, TESTCASE, MOCK, VERIFY, EXPECT]" + Constants.NEWLINE;
        expectedResult += "Got <BEFORE EACH> classified as <BEFORE EACH>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void it_catches_unexpected_keyword_after_verify_without_commas_() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       MOCK CALL 'PROG3' END-MOCK"+ Constants.NEWLINE);
        testSuite.append("       VERIFY CALL 'PROG3' HAPPENED ONCE"+ Constants.NEWLINE);
        testSuite.append("       BEFORE EACH"+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "SYNTAX ERROR in file: null:5:8:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 5, index  8:" + Constants.NEWLINE;
        expectedResult += "Following <ONCE> classified as <ONCE>" + Constants.NEWLINE;
        expectedResult += "Expected classification: [cobol-token, TESTSUITE, TESTCASE, MOCK, VERIFY, EXPECT]" + Constants.NEWLINE;
        expectedResult += "Got <BEFORE EACH> classified as <BEFORE EACH>" + Constants.NEWLINE + Constants.NEWLINE;

        assertThrows(TestSuiteSyntaxException.class, () -> {
            testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())),
                    numericFields);
        });

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }


    @Test
    public void it_catches_type_mismatch_of_numeric_and_alphanumeric_for_explicit_numeric_in_unit_test() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       EXPECT WS-ALPHA-VALUE TO BE NUMERIC \"Hello\""+ Constants.NEWLINE);

        String expectedResult = "";
        expectedResult += "WARNING in file: null:3:44:" + Constants.NEWLINE;
        expectedResult += "Unexpected token on line 3, index 44:" + Constants.NEWLINE;
        expectedResult += "Expected compare to be of type <NUMERIC>, but the variable was classified as the type <ALPHANUMERIC>" + Constants.NEWLINE;
        expectedResult += "The test was carried out with the compare type <ALPHANUMERIC>" + Constants.NEWLINE + Constants.NEWLINE;

        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())), numericFields);

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

    @Test
    public void explicit_numeric_gives_no_warning_when_variable_is_numeric() {
        testSuite.append("       TESTSUITE \"Name of test suite\""+ Constants.NEWLINE);
        testSuite.append("       TESTCASE \"Name of test case\""+ Constants.NEWLINE);
        testSuite.append("       EXPECT WS-NUMERIC-VALUE TO BE NUMERIC 1"+ Constants.NEWLINE);

        String expectedResult = "";

        numericFields.setDataTypeOf("WS-NUMERIC-VALUE", DataType.PACKED_DECIMAL);

        testSuiteParser.getParsedTestSuiteLines(new BufferedReader(new StringReader(testSuite.toString())), numericFields);

        String actualResult = testSuiteErrorLog.getErrorMessages();
        assertEquals(expectedResult, actualResult);
    }

}