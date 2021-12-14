package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.interpreter.InterpreterController;
import com.neopragma.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import com.neopragma.cobolcheck.features.writer.CobolWriter;
import com.neopragma.cobolcheck.features.writer.WriterController;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;
import com.neopragma.cobolcheck.workers.Generator;
import jdk.vm.ci.meta.Constant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
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

        List<String> actual = getTrimmedList(removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(getTrimmedList(expected1), actual);
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

        List<String> actual = getTrimmedList(removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(getTrimmedList(expected2), actual);
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

        List<String> actual = getTrimmedList(removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(getTrimmedList(expected3), actual);
    }

    private List<String> getTrimmedList(String text){
        String[] lines = text.split(Constants.NEWLINE);
        List<String> result = new ArrayList<>();
        for (String line : lines){
            result.add(StringHelper.removeTrailingSpaces(line));
        }
        return result;
    }

    private String removeBoilerPlateCode(String code, List<String> boilerPlateTags){
        boolean insideBoilerPlate = false;
        String result = "";
        String[] lines = code.split(Constants.NEWLINE);
        for (String line : lines){
            if (line.contains("*")){
                boolean skip = false;
                for(String tag : boilerPlateTags){
                    if (line.contains(tag)){
                        skip = true;
                        if (line.contains("END")){
                            insideBoilerPlate = false;
                            continue;
                        }
                        else {
                            insideBoilerPlate = true;
                            continue;
                        }
                    }
                }
                if (skip){
                    continue;
                }
            }
            if (!insideBoilerPlate){
                result += line + Constants.NEWLINE;
            }
        }
        return result;
    }

    private String expected1 =
            "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       01  UT-MOCKS-GENERATED.                                                  " + Constants.NEWLINE +
                    "           05  UT-1-0-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
                    "           05  UT-1-0-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
                    "           05  UT-1-0-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
                    "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"Mocking tests\"                                              " + Constants.NEWLINE +
                    "           MOVE \"Mocking tests\"                                                 " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           MOVE 0 TO UT-1-0-1-MOCK-COUNT                                        " + Constants.NEWLINE +
                    "           MOVE 0 TO UT-1-0-1-MOCK-EXPECTED                                     " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Paragraphs called when mocking                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "       1-0-1-MOCK.                                                              " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Global mock of: SECTION: 000-START                                       " + Constants.NEWLINE +
                    "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           ADD 1 TO UT-1-0-1-MOCK-COUNT                                         " + Constants.NEWLINE +
                    "           PERFORM 100-WELCOME                                                  " + Constants.NEWLINE +
                    "       .                                                                        " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       000-START SECTION.                                                       " + Constants.NEWLINE +
                    "            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME                  " + Constants.NEWLINE +
                    "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
                    "                ALSO ANY                                                        " + Constants.NEWLINE +
                    "                    PERFORM 1-0-1-MOCK                                          " + Constants.NEWLINE +
                    "           WHEN OTHER                                                           " + Constants.NEWLINE +
                    "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
                    "            END-EVALUATE                                                        " + Constants.NEWLINE +
                    "           EXIT SECTION                                                         " + Constants.NEWLINE +
                    "           .                                                                   " + Constants.NEWLINE;

    private String expected2 =
            "       WORKING-STORAGE SECTION.                                                 " +      Constants.NEWLINE  +
            "       01  UT-MOCKS-GENERATED.                                                  " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-0-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-1-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-2-1-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 000-START\".                                   " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-2-2-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 100-WELCOME\".                                 " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  " + Constants.NEWLINE +
            "           05  UT-1-2-3-MOCK-NAME        PIC X(40)                              " + Constants.NEWLINE +
            "                   VALUE \"SECTION 200-GOODBYE\".                                 " + Constants.NEWLINE +
            "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
            "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
            "           DISPLAY \"Mocking tests\"                                              " + Constants.NEWLINE +
            "           MOVE \"Mocking tests\"                                                 " + Constants.NEWLINE +
            "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
            "           MOVE \"Local mock overwrites global mock\"                             " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-BEFORE                                                    " + Constants.NEWLINE +
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
            "           PERFORM UT-AFTER                                                     " + Constants.NEWLINE +
            "           MOVE \"Simply a test\"                                                 " + Constants.NEWLINE +
            "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
            "           PERFORM UT-BEFORE                                                    " + Constants.NEWLINE +
            "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
            "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-1-MOCK-COUNT                                        " + Constants.NEWLINE +
            "           MOVE 0 TO UT-1-0-1-MOCK-EXPECTED                                     " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Paragraphs called when mocking                                           " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "       1-0-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Global mock of: SECTION: 000-START                                       " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-0-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "           PERFORM 100-WELCOME                                                  " + Constants.NEWLINE +
            "       .                                                                        " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       1-1-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Local mock overwrites global mock\"                         " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-1-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "                MOVE \"This is\" TO VALUE-1                                       " + Constants.NEWLINE +
            "       .                                                                        " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       1-2-1-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 000-START                                        " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-1-MOCK-COUNT                                         " + Constants.NEWLINE +
            "       .                                                                        " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       1-2-2-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 100-WELCOME                                      " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-2-MOCK-COUNT                                         " + Constants.NEWLINE +
            "       .                                                                        " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       1-2-3-MOCK.                                                              " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "      *Local mock of: SECTION: 200-GOODBYE                                      " + Constants.NEWLINE +
            "      *In testsuite: \"Mocking tests\"                                            " + Constants.NEWLINE +
            "      *In testcase: \"Simply a test\"                                             " + Constants.NEWLINE +
            "      *****************************************************************         " + Constants.NEWLINE +
            "           ADD 1 TO UT-1-2-3-MOCK-COUNT                                         " + Constants.NEWLINE +
            "       .                                                                        " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "       000-START SECTION.                                                       " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME                  " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                ALSO \"Local mock overwrites global mock\"                        " + Constants.NEWLINE +
            "                    PERFORM 1-1-1-MOCK                                          " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                ALSO \"Simply a test\"                                            " + Constants.NEWLINE +
            "                    PERFORM 1-2-1-MOCK                                          " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                ALSO ANY                                                        " + Constants.NEWLINE +
            "                    PERFORM 1-0-1-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
            "            END-EVALUATE                                                        " + Constants.NEWLINE +
            "           EXIT SECTION.                                                        " + Constants.NEWLINE +
            "       100-WELCOME SECTION.                                                     " + Constants.NEWLINE +
            "                                                                                " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME                  " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                ALSO \"Simply a test\"                                            " + Constants.NEWLINE +
            "                    PERFORM 1-2-2-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "           MOVE \"Hello\" to VALUE-1                                              " + Constants.NEWLINE +
            "            END-EVALUATE                                                        " + Constants.NEWLINE +
            "           .                                                                    " + Constants.NEWLINE +
            "       200-GOODBYE SECTION                   .                                  " + Constants.NEWLINE +
            "            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME                  " + Constants.NEWLINE +
            "                WHEN \"Mocking tests\"                                            " + Constants.NEWLINE +
            "                ALSO \"Simply a test\"                                            " + Constants.NEWLINE +
            "                    PERFORM 1-2-3-MOCK                                          " + Constants.NEWLINE +
            "           WHEN OTHER                                                           " + Constants.NEWLINE +
            "           MOVE \"Bye\" to VALUE-1                                                " + Constants.NEWLINE +
            "            END-EVALUATE                                                        " + Constants.NEWLINE +
            "          .     " + Constants.NEWLINE;

    private String expected3 =
            "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"No mocks\"                                                   " + Constants.NEWLINE +
                    "           MOVE \"No mocks\"                                                      " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "           MOVE \"My testCase\"                                                   " + Constants.NEWLINE +
                    "               TO UT-TEST-CASE-NAME                                             " + Constants.NEWLINE +
                    "           PERFORM UT-BEFORE                                                    " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE-MOCK-COUNT                                     " + Constants.NEWLINE +
                    "           ADD 1 TO UT-TEST-CASE-COUNT                                          " + Constants.NEWLINE +
                    "           SET UT-NORMAL-COMPARE TO TRUE                                        " + Constants.NEWLINE +
                    "           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  " + Constants.NEWLINE +
                    "           MOVE VALUE-1 TO UT-ACTUAL                                            " + Constants.NEWLINE +
                    "           MOVE \"something\"                                                     " + Constants.NEWLINE +
                    "               TO UT-EXPECTED                                                   " + Constants.NEWLINE +
                    "           SET UT-RELATION-EQ TO TRUE                                           " + Constants.NEWLINE +
                    "           PERFORM UT-CHECK-EXPECTATION                                         " + Constants.NEWLINE +
                    "           PERFORM UT-AFTER                                                     " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "       000-START SECTION.                                                       " + Constants.NEWLINE +
                    "           MOVE \"Value1\" to VALUE-1                                             " + Constants.NEWLINE +
                    "           EXIT SECTION                                                         " + Constants.NEWLINE +
                    "           .                                                                   " + Constants.NEWLINE;

}


