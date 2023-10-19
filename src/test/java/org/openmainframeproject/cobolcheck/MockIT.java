package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.features.interpreter.InterpreterController;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import org.openmainframeproject.cobolcheck.features.writer.CobolWriter;
import org.openmainframeproject.cobolcheck.features.writer.WriterController;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter;
import org.openmainframeproject.cobolcheck.workers.Generator;
import org.openmainframeproject.cobolcheck.testhelpers.Utilities;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MockIT {

    private Generator generator;
    private TestSuiteParserController testSuiteParserController;
    private BufferedReader mockedParserReader;
    private InterpreterController interpreterController;
    private BufferedReader mockedInterpreterReader;
    private Interpreter interpreter;
    private CobolWriter cobolWriter;
    private WriterController writerController;
    private Writer writer;

    private List<String> boilerPLateWS;
    private List<String> boilerPLateDP;
    List<String> boilerPlateTags = Arrays.asList("* CCHECKWS.CPY", "* CCHECKPARAGRAPHSPD.CPY", "* CCHECKRESULTPD.CPY");

    @BeforeAll
    static void oneTimeSetup() {
        Config.load("testconfig.properties");
    }

    @BeforeEach
    void commonSetup() throws IOException {
        mockedInterpreterReader = Mockito.mock(BufferedReader.class);
        interpreterController = new InterpreterController(mockedInterpreterReader);

        writer = new StringWriter();
        cobolWriter = new CobolWriter(writer);
        writerController = new WriterController(cobolWriter);

        mockedParserReader = Mockito.mock(BufferedReader.class);
        testSuiteParserController = new TestSuiteParserController(mockedParserReader);

        if (boilerPLateWS == null){
            boilerPLateWS = testSuiteParserController.getBoilerplateCodeFromCopybooks("CCHECKWS.CPY");
            boilerPLateDP = testSuiteParserController.getBoilerplateCodeFromCopybooks("CCHECKPARAGRAPHSPD.CPY");
        }
    }

    @Test
    public void it_inserts_a_mock_correctly() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       000-START SECTION.";
        String s4 = "           MOVE \"Value1\" to VALUE-1";
        String s5 = "           EXIT SECTION";
        String s6 = "           .";

        String t1 = "           TestSuite \"Mocking tests\"";
        String t2 = "           MOCK SECTION 000-START";
        String t3 = "           PERFORM 100-WELCOME";
        String t4 = "           END-MOCK";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, t4, null);


        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities.getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));
        assertEquals(Utilities.getTrimmedList(expected1), actual);
    }

    @Test
    public void it_inserts_mocks_correctly_with_source_code_changing_code_style() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       000-START SECTION.";
        String s4 = "           MOVE \"Value1\" to VALUE-1";
        String s5 = "           EXIT SECTION.";
        String s6 = "       100-WELCOME SECTION";
        String s7 = "";
        String s8 = "           .";
        String s9 = "           MOVE \"Hello\" to VALUE-1.";
        String s10 = "       200-GOODBYE SECTION                   .";
        String s11 = "           MOVE \"Bye\" to VALUE-1";
        String s12 = "          .";

        String t1 = "           TestSuite \"Mocking tests\"";
        String t2 = "           MOCK SECTION 000-START";
        String t3 = "           PERFORM 100-WELCOME";
        String t4 = "           END-MOCK";
        String t5 = "           TestCase \"Local mock overwrites global mock\"";
        String t6 = "           MOCK SECTION 000-START";
        String t7 = "                MOVE \"This is\" TO VALUE-1";
        String t8 = "           END-MOCK";
        String t9 = "           PERFORM 000-START";
        String t10 = "           Expect VALUE-1 to be \"This is\"";
        String t11 = "           TestCase \"Simply a test\"";
        String t12 = "           MOCK SECTION 000-START";
        String t13 = "           END-MOCK";
        String t14 = "           MOCK SECTION 100-WELCOME";
        String t15 = "           END-MOCK";
        String t16 = "           MOCK SECTION 200-GOODBYE";
        String t17 = "           END-MOCK";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
                s11, s12, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11,
                t12, t13, t14, t15, t16, t17, null);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities.getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));
        assertEquals(Utilities.getTrimmedList(expected2), actual);
    }

    @Test
    public void it_generates_code_correctly_when_no_mock_is_presennt() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       000-START SECTION.";
        String s4 = "           MOVE \"Value1\" to VALUE-1";
        String s5 = "           EXIT SECTION";
        String s6 = "           .";

        String t1 = "           TestSuite \"No mocks\"";
        String t2 = "           TESTCASE \"My testCase\"";
        String t3 = "               EXPECT VALUE-1 TO BE \"something\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, null);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities.getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));
        assertEquals(Utilities.getTrimmedList(expected3), actual);
    }

    @Test
    public void it_inserts_call_mocks_correctly() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       000-START SECTION.";
        String s4 = "           MOVE \"Value1\" to VALUE-1";
        String s5 = "           EXIT SECTION.";
        String s6 = "       100-WELCOME SECTION.";
        String s7 = "           CALL 'prog1' USING";
        String s8 = "               BY CONTENT VALUE-1, VALUE-2.";
        String s9 = "           MOVE \"Hello\" to VALUE-1.";
        String s10 = "       200-GOODBYE SECTION.";
        String s11 = "          MOVE \"Bye\" to VALUE-1";
        String s12 = "          CALL bogus USING VALUE-1";
        String s13 = "";
        String s14 = "          CALL 'prog2' USING VALUE-1";
        String s15 = "          CALL 'prog2' USING VALUE-1.";
        String s16 = "          .";
        String s17 = "      * Ending with comment";

        String t1 = "           TestSuite \"Mocking tests\"";
        String t2 = "           MOCK SECTION 100-WELCOME";
        String t3 = "               MOVE \"mock\" TO VALUE-1";
        String t4 = "           END-MOCK";
        String t5 = "           MOCK CALL 'prog2' USING VALUE-1";
        String t6 = "               MOVE \"prog2\" TO VALUE-1";
        String t7 = "           END-MOCK";
        String t8 = "           TestCase \"Local mock overwrites global mock\"";
        String t9 = "           MOCK SECTION 200-GOODBYE";
        String t10 = "                MOVE \"Goodbye\" TO VALUE-1";
        String t11 = "           END-MOCK";
        String t12 = "           PERFORM 200-GOODBYE";
        String t13 = "           Expect VALUE-1 to be \"Goodbye\"";
        String t14 = "           TestCase \"Simply a test\"";
        String t15 = "           MOCK SECTION 000-START";
        String t16 = "           END-MOCK";
        String t17 = "           MOCK CALL 'prog1' USING BY CONTENT VALUE-1, VALUE-2";
        String t18 = "           END-MOCK";
        String t19 = "           MOCK SECTION 200-GOODBYE";
        String t20 = "           END-MOCK";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
                s11, s12, s13, s14, s15, s16, s17, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11,
                t12, t13, t14, t15, t16, t17, t18, t19, t20, null);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities.getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));
        assertEquals(Utilities.getTrimmedList(expected4), actual);
    }

    @Test
    public void it_inserts_call_mocks_without_commas_correctly() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       000-START SECTION.";
        String s4 = "           MOVE \"Value1\" to VALUE-1";
        String s5 = "           EXIT SECTION.";
        String s6 = "       100-WELCOME SECTION.";
        String s7 = "           CALL 'prog1' USING";
        String s8 = "               BY CONTENT VALUE-1, VALUE-2.";
        String s9 = "           MOVE \"Hello\" to VALUE-1.";
        String s10 = "       200-GOODBYE SECTION.";
        String s11 = "          MOVE \"Bye\" to VALUE-1";
        String s12 = "          CALL bogus USING VALUE-1";
        String s13 = "";
        String s14 = "          CALL 'prog2' USING VALUE-1";
        String s15 = "          CALL 'prog2' USING VALUE-1.";
        String s16 = "          .";
        String s17 = "      * Ending with comment";

        String t1 = "           TestSuite \"Mocking tests\"";
        String t2 = "           MOCK SECTION 100-WELCOME";
        String t3 = "               MOVE \"mock\" TO VALUE-1";
        String t4 = "           END-MOCK";
        String t5 = "           MOCK CALL 'prog2' USING VALUE-1";
        String t6 = "               MOVE \"prog2\" TO VALUE-1";
        String t7 = "           END-MOCK";
        String t8 = "           TestCase \"Local mock overwrites global mock\"";
        String t9 = "           MOCK SECTION 200-GOODBYE";
        String t10 = "                MOVE \"Goodbye\" TO VALUE-1";
        String t11 = "           END-MOCK";
        String t12 = "           PERFORM 200-GOODBYE";
        String t13 = "           Expect VALUE-1 to be \"Goodbye\"";
        String t14 = "           TestCase \"Simply a test\"";
        String t15 = "           MOCK SECTION 000-START";
        String t16 = "           END-MOCK";
        String t17 = "           MOCK CALL 'prog1' USING BY CONTENT VALUE-1 VALUE-2";
        String t18 = "           END-MOCK";
        String t19 = "           MOCK SECTION 200-GOODBYE";
        String t20 = "           END-MOCK";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
                s11, s12, s13, s14, s15, s16, s17, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11,
                t12, t13, t14, t15, t16, t17, t18, t19, t20, null);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities.getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));
        assertEquals(Utilities.getTrimmedList(expected4), actual);
    }

    private String expected1 =
            "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       01  UT-MOCKS-GENERATED.                                                  " + Constants.NEWLINE +
                    "           05  UT-1-0-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
                    "           05  UT-1-0-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
                    "           05  UT-1-0-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
                    "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"Mocking tests\" =============*                             " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"Mocking tests\"                                              " + Constants.NEWLINE +
                    "           MOVE \"Mocking tests\"                                                 " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       UT-PROCESS-UNMOCK-CALL.                                                     " + Constants.NEWLINE +                                               
                    "           Add 1 to UT-NUMBER-UNMOCK-CALL                                       " + Constants.NEWLINE +                                     
                    "           display \"Call not mocked in testcase: \" UT-TEST-CASE-NAME          " + Constants.NEWLINE +
                    "           display \"               in testsuite: \" UT-TEST-SUITE-NAME         " + Constants.NEWLINE +
                    "           display \"All used calls should be mocked, to ensure the unit        " + Constants.NEWLINE +       
                    "      -    \"test has control over input data\"                                 " + Constants.NEWLINE + 
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +                                                               
                    "                                                                                " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           MOVE 0 TO UT-1-0-1-MOCK-COUNT                                        " + Constants.NEWLINE +
                    "           MOVE 0 TO UT-1-0-1-MOCK-EXPECTED                                     " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Paragraphs called when mocking                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "       UT-1-0-1-MOCK.                                                              " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Global mock of: SECTION: 000-START                                       " + Constants.NEWLINE +
                    "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           ADD 1 TO UT-1-0-1-MOCK-COUNT                                         " + Constants.NEWLINE +
                    "           PERFORM 100-WELCOME                                                  " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       000-START SECTION.                                                       " + Constants.NEWLINE +
                    "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
                    "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
                    "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
                    "                   ALSO ANY                                                     " + Constants.NEWLINE +
                    "                    PERFORM UT-1-0-1-MOCK                                          " + Constants.NEWLINE +
                    "           WHEN OTHER                                                           " + Constants.NEWLINE +
                    "                    PERFORM UT-1-0-0-WO                                           " + Constants.NEWLINE +
                    "            END-EVALUATE                                                         " + Constants.NEWLINE +
                    "           .                                                                   " + Constants.NEWLINE +
                    "                                                                              " + Constants.NEWLINE +
                    "      *****************************************************************     " + Constants.NEWLINE +
                    "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
                    "      *****************************************************************     " + Constants.NEWLINE +
                    "       UT-1-0-0-WO SECTION.                                                 " + Constants.NEWLINE +
                    "      *****************************************************************     " + Constants.NEWLINE +
                    "      *WhenOther of: SECTION: 000-START                                        " + Constants.NEWLINE +
                    "      *****************************************************************     " + Constants.NEWLINE +
                    "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
                    "           EXIT SECTION                                                         " + Constants.NEWLINE +
                    "           .                                                                   " + Constants.NEWLINE+
                    "           .                                                                   " + Constants.NEWLINE+
                    "                                                                              " + Constants.NEWLINE+
                    "                                                                                "+ Constants.NEWLINE;

    private String expected2 =
            "       WORKING-STORAGE SECTION.                                                 " +      Constants.NEWLINE  +
            "       01  UT-MOCKS-GENERATED.                                                  " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 100-WELCOME\".                                 " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 200-GOODBYE\".                                 " + Constants.NEWLINE +
            "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
            "      *============= \"Mocking tests\" =============*                             " + Constants.NEWLINE +
            "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
            "           DISPLAY \"Mocking tests\"                                              " + Constants.NEWLINE +
            "           MOVE \"Mocking tests\"                                                 " + Constants.NEWLINE +
            "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
            "      *-------- \"Local mock overwrites global mock\"                             " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-BEFORE-EACH                                               " + Constants.NEWLINE +
            "           MOVE \"Local mock overwrites global mock\"                             " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
            "            PERFORM 000-START                                                   " + Constants.NEWLINE +
            "           ADD 1 TO UT-TEST-CASE-COUNT                                          " + Constants.NEWLINE +
            "           SET UT-NORMAL-COMPARE TO TRUE                                        " + Constants.NEWLINE +
            "           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  " + Constants.NEWLINE +
            "           MOVE VALUE-1 TO UT-ACTUAL                                            " + Constants.NEWLINE +
            "           MOVE \"This is\"                                                       " + Constants.NEWLINE +
            "               TO UT-EXPECTED                                                   " + Constants.NEWLINE +
            "           SET UT-RELATION-EQ TO TRUE                                           " + Constants.NEWLINE +
            "           PERFORM UT-CHECK-EXPECTATION                                         " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-AFTER-EACH                                                " + Constants.NEWLINE +
            "      *-------- \"Simply a test\"                                                 " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-BEFORE-EACH                                               " + Constants.NEWLINE +
            "           MOVE \"Simply a test\"                                                 " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-AFTER-EACH                                                " + Constants.NEWLINE +
            "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           CONTINUE                                                             " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           CONTINUE                                                             " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-PROCESS-UNMOCK-CALL.                                                     " + Constants.NEWLINE +                                               
            "           Add 1 to UT-NUMBER-UNMOCK-CALL                                       " + Constants.NEWLINE +                                     
            "           display \"Call not mocked in testcase: \" UT-TEST-CASE-NAME          " + Constants.NEWLINE +
            "           display \"               in testsuite: \" UT-TEST-SUITE-NAME         " + Constants.NEWLINE +
            "           display \"All used calls should be mocked, to ensure the unit        " + Constants.NEWLINE +
            "      -    \"test has control over input data\"                                 " + Constants.NEWLINE +     
            "           CONTINUE                                                             " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +                                                               
            "                                                                                " + Constants.NEWLINE +
            "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-1-MOCK-COUNT                                        " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-1-MOCK-EXPECTED                                     " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Paragraphs called when mocking                                           " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "       UT-1-0-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Global mock of: SECTION: 000-START                                       " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-0-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           PERFORM 100-WELCOME                                                  " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-1-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Local mock overwrites global mock\"                         " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-1-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           MOVE \"This is\" TO VALUE-1                                            " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-2-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-2-2-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 100-WELCOME                                      " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-2-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-2-3-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 200-GOODBYE                                      " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-3-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       000-START SECTION.                                                       " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                           " + Constants.NEWLINE +
            "                   ALSO \"Local mock overwrites global mock\"                                                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-1-1-MOCK                                          " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                                           " + Constants.NEWLINE +
            "                   ALSO \"Simply a test\"                                                         " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-1-MOCK                                                           " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                                         " + Constants.NEWLINE +
            "                   ALSO ANY                                                        " + Constants.NEWLINE +
            "                    PERFORM UT-1-0-1-MOCK                                                         " + Constants.NEWLINE +
            "           WHEN OTHER                                                         " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-0-WO                                            " + Constants.NEWLINE +
            "            END-EVALUATE                                                         " + Constants.NEWLINE +
            "           .                                                                   " + Constants.NEWLINE +
            "                                                                                 " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "       UT-1-2-0-WO SECTION.                                             " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
            "           EXIT SECTION.                                                        " + Constants.NEWLINE +
            "           .                                                                   " + Constants.NEWLINE+
            "           .                                                                   " + Constants.NEWLINE+
            "                                                                                " + Constants.NEWLINE+
            "                                                                                " + Constants.NEWLINE+
            "       100-WELCOME SECTION.                                                     " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO \"Simply a test\"                                                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-2-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-1-WO                                                           " + Constants.NEWLINE +
            "            END-EVALUATE                                                           " + Constants.NEWLINE +
            "           .                                                           " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "       UT-1-2-1-WO SECTION.                                                          " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther of: SECTION: 100-WELCOME                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "           MOVE \"Hello\" to VALUE-1.                                              " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "                                                                                " + Constants.NEWLINE+
            "       200-GOODBYE SECTION                   .                                                    " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO \"Simply a test\"                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-3-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-2-WO                                                           " + Constants.NEWLINE +
            "            END-EVALUATE                                                           " + Constants.NEWLINE +
            "          .                                                          " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "       UT-1-2-2-WO SECTION.                                                           " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther of: SECTION: 200-GOODBYE                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "           MOVE \"Bye\" to VALUE-1                                                 " + Constants.NEWLINE +
            "          .                                                                    " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "                                                                                " + Constants.NEWLINE;

    private String expected3 =
            "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"No mocks\" =============*                                  " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"No mocks\"                                                   " + Constants.NEWLINE +
                    "           MOVE \"No mocks\"                                                      " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "      *-------- \"My testCase\"                                                   " + Constants.NEWLINE +
                    "           MOVE SPACES                                                          " + Constants.NEWLINE +
                    "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
                    "           PERFORM UT-BEFORE-EACH                                               " + Constants.NEWLINE +
                    "           MOVE \"My testCase\"                                                   " + Constants.NEWLINE +
                    "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
                    "           ADD 1 TO UT-TEST-CASE-COUNT                                          " + Constants.NEWLINE +
                    "           SET UT-NORMAL-COMPARE TO TRUE                                        " + Constants.NEWLINE +
                    "           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  " + Constants.NEWLINE +
                    "           MOVE VALUE-1 TO UT-ACTUAL                                            " + Constants.NEWLINE +
                    "           MOVE \"something\"                                                     " + Constants.NEWLINE +
                    "               TO UT-EXPECTED                                                   " + Constants.NEWLINE +
                    "           SET UT-RELATION-EQ TO TRUE                                           " + Constants.NEWLINE +
                    "           PERFORM UT-CHECK-EXPECTATION                                         " + Constants.NEWLINE +
                    "           MOVE SPACES                                                          " + Constants.NEWLINE +
                    "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
                    "           PERFORM UT-AFTER-EACH                                                " + Constants.NEWLINE +
                    "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       UT-PROCESS-UNMOCK-CALL.                                                     " + Constants.NEWLINE +                                               
                    "           Add 1 to UT-NUMBER-UNMOCK-CALL                                       " + Constants.NEWLINE +                                     
                    "           display \"Call not mocked in testcase: \" UT-TEST-CASE-NAME          " + Constants.NEWLINE +
                    "           display \"               in testsuite: \" UT-TEST-SUITE-NAME         " + Constants.NEWLINE +
                    "           display \"All used calls should be mocked, to ensure the unit        " + Constants.NEWLINE +       
                    "      -    \"test has control over input data\"                                 " + Constants.NEWLINE +  
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +                                                               
                    "                                                                                " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       000-START SECTION.                                                       " + Constants.NEWLINE +
                    "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
                    "           EXIT SECTION                                                         " + Constants.NEWLINE +
                    "           .                                                                   ".replace(" UT-", " " + Config.getTestCodePrefix());

    private String expected4 =
            "       WORKING-STORAGE SECTION.                                                 " +     Constants.NEWLINE +
            "       01  UT-MOCKS-GENERATED.                                                  " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 100-WELCOME\".                                 " + Constants.NEWLINE +
            "           05  UT-1-0-2-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-0-2-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-0-2-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"CALL 'prog2'\".                                        " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 200-GOODBYE\".                                 " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"CALL 'prog1'\".                                        " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-COUNT       PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-EXPECTED    PIC 9(18) VALUE ZERO COMP.             " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 200-GOODBYE\".                                 " + Constants.NEWLINE +
            "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
            "      *============= \"Mocking tests\" =============*                             " + Constants.NEWLINE +
            "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
            "           DISPLAY \"Mocking tests\"                                              " + Constants.NEWLINE +
            "           MOVE \"Mocking tests\"                                                 " + Constants.NEWLINE +
            "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
            "      *-------- \"Local mock overwrites global mock\"                             " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-BEFORE-EACH                                               " + Constants.NEWLINE +
            "           MOVE \"Local mock overwrites global mock\"                             " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
            "            PERFORM 200-GOODBYE                                                 " + Constants.NEWLINE +
            "           ADD 1 TO UT-TEST-CASE-COUNT                                          " + Constants.NEWLINE +
            "           SET UT-NORMAL-COMPARE TO TRUE                                        " + Constants.NEWLINE +
            "           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  " + Constants.NEWLINE +
            "           MOVE VALUE-1 TO UT-ACTUAL                                            " + Constants.NEWLINE +
            "           MOVE \"Goodbye\"                                                       " + Constants.NEWLINE +
            "               TO UT-EXPECTED                                                   " + Constants.NEWLINE +
            "           SET UT-RELATION-EQ TO TRUE                                           " + Constants.NEWLINE +
            "           PERFORM UT-CHECK-EXPECTATION                                         " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-AFTER-EACH                                                " + Constants.NEWLINE +
            "      *-------- \"Simply a test\"                                                 " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-BEFORE-EACH                                               " + Constants.NEWLINE +
            "           MOVE \"Simply a test\"                                                 " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
            "           MOVE SPACES                                                          " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-AFTER-EACH                                                " + Constants.NEWLINE +
            "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           CONTINUE                                                             " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           CONTINUE                                                             " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-PROCESS-UNMOCK-CALL.                                                     " + Constants.NEWLINE +                                               
            "           Add 1 to UT-NUMBER-UNMOCK-CALL                                       " + Constants.NEWLINE +                                     
            "           display \"Call not mocked in testcase: \" UT-TEST-CASE-NAME          " + Constants.NEWLINE +
            "           display \"               in testsuite: \" UT-TEST-SUITE-NAME         " + Constants.NEWLINE +
            "           display \"All used calls should be mocked, to ensure the unit        " + Constants.NEWLINE +
            "      -    \"test has control over input data\"                                 " + Constants.NEWLINE +  
            "           CONTINUE                                                             " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +                                                               
            "                                                                                " + Constants.NEWLINE +
            "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-1-MOCK-COUNT                                        " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-1-MOCK-EXPECTED                                     " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-2-MOCK-COUNT                                        " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-2-MOCK-EXPECTED                                     " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Paragraphs called when mocking                                           " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "       UT-1-0-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Global mock of: SECTION: 100-WELCOME                                     " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-0-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           MOVE \"mock\" TO VALUE-1                                               " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-0-2-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Global mock of: CALL: 'prog2'                                            " + Constants.NEWLINE +
            "      *With args: REFERENCE VALUE-1                                             " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-0-2-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           MOVE \"prog2\" TO VALUE-1                                              " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-1-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 200-GOODBYE                                      " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Local mock overwrites global mock\"                         " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-1-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           MOVE \"Goodbye\" TO VALUE-1                                           " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-2-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-2-2-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: CALL: 'prog1'                                             " + Constants.NEWLINE +
            "      *With args: CONTENT VALUE-1, REFERENCE VALUE-2                            " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-2-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       UT-1-2-3-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 200-GOODBYE                                      " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-3-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       000-START SECTION.                                                       " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                           " + Constants.NEWLINE +
            "                   ALSO \"Simply a test\"                                                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-1-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-0-WO                                            " + Constants.NEWLINE +
            "            END-EVALUATE                                                         " + Constants.NEWLINE +
            "           .                                                                   " + Constants.NEWLINE +
            "                                                                                 " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "       UT-1-2-0-WO SECTION.                                             " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
            "           EXIT SECTION.                                                        " + Constants.NEWLINE +
            "           .                                                                   " + Constants.NEWLINE+
            "           .                                                           " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "                                                                                " + Constants.NEWLINE+
            "       100-WELCOME SECTION.                                                     " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO ANY                                                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-0-1-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-1-WO                                                           " + Constants.NEWLINE +
            "            END-EVALUATE                                                           " + Constants.NEWLINE +
            "           .                                                           " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "       UT-1-2-1-WO SECTION.                                                          " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther of: SECTION: 100-WELCOME                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *    CALL 'prog1' USING                                                   " + Constants.NEWLINE +
            "      *        BY CONTENT VALUE-1, VALUE-2.                                     " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO \"Simply a test\"                                         " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-2-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                             " + Constants.NEWLINE +
            "                    PERFORM UT-PROCESS-UNMOCK-CALL                                   " + Constants.NEWLINE +
            "            END-EVALUATE                                                          " + Constants.NEWLINE +
            "            CONTINUE                                                        " + Constants.NEWLINE +
            "           MOVE \"Hello\" to VALUE-1.                                              " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "           .                                                           " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "                                                                                " + Constants.NEWLINE+
            "       200-GOODBYE SECTION.                                                     " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO \"Local mock overwrites global mock\"                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-1-1-MOCK                                          " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO \"Simply a test\"                                         " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-3-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-1-2-2-WO                                                           " + Constants.NEWLINE +
            "            END-EVALUATE                                                           " + Constants.NEWLINE +
            "          .                                                          " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE+
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther Paragraph or Section called                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "       UT-1-2-2-WO SECTION.                                                           " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "      *WhenOther of: SECTION: 200-GOODBYE                                        " + Constants.NEWLINE +
            "      *****************************************************************     " + Constants.NEWLINE +
            "          MOVE \"Bye\" to VALUE-1                                                 " + Constants.NEWLINE +
            "      *   CALL bogus USING VALUE-1                                              " + Constants.NEWLINE +
            "           PERFORM UT-PROCESS-UNMOCK-CALL                                         " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "      *   CALL 'prog2' USING VALUE-1                                            " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO ANY                                                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-0-2-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-PROCESS-UNMOCK-CALL                                    " + Constants.NEWLINE +
            "            END-EVALUATE                                                        " + Constants.NEWLINE +
            "            CONTINUE                                                        " + Constants.NEWLINE +
            "      *   CALL 'prog2' USING VALUE-1.                                           " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME                                         " + Constants.NEWLINE +
            "                   ALSO UT-TEST-CASE-NAME                                       " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                   ALSO ANY                                                     " + Constants.NEWLINE +
            "                    PERFORM UT-1-0-2-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "                    PERFORM UT-PROCESS-UNMOCK-CALL                                    " + Constants.NEWLINE +
            "            END-EVALUATE                                                        " + Constants.NEWLINE +
            "            CONTINUE                                                        " + Constants.NEWLINE +
            "          .                                                                    " + Constants.NEWLINE +
            "           .                                                           " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "      * Ending with comment                                                    ";

}


