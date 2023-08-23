package org.openmainframeproject.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.openmainframeproject.cobolcheck.features.interpreter.InterpreterController;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import org.openmainframeproject.cobolcheck.features.writer.CobolWriter;
import org.openmainframeproject.cobolcheck.features.writer.WriterController;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter;
import org.openmainframeproject.cobolcheck.testhelpers.Utilities;
import org.openmainframeproject.cobolcheck.workers.Generator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ExpanderTest {

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
        String s2 = "       EXEC SQL INCLUDE TEXEM  END-EXEC.";
        String s3 = "       01  FILLER.";
        String s4 = "          05  WS-FIELD-1           PIC X(80).";
        String s5 = "          05  ws-Field-2           PIC X(80).";
        String s6 = "       PROCEDURE DIVISION.";
        String s7 = "       000-START SECTION.";
        String s8 = "           MOVE \"Value1\" to WS-FIELD-1";
        String s9 = "           EXIT SECTION";
        String s10 = "           .";

        String t1 = "           TestSuite \"Basic test\"";
        String t2 = "           PERFORM 000-START";
        String t3 = "           EXPECT WS-FIELD-1 TO BE \"Value1\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1, t2, t3, null);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities.getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(Utilities.getTrimmedList(expected1), actual);
    }

    private String expected1 =
            "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "      *EXEC SQL INCLUDE TEXEM  END-EXEC.                                       " + Constants.NEWLINE +
                    "       01  TEXEM.                                                               " + Constants.NEWLINE +
                    "           10 FIRST-NAME           PIC X(10).                                   " + Constants.NEWLINE +
                    "           10 LAST-NAME            PIC X(10).                                   " + Constants.NEWLINE +
                    "           10 TMS-CREA             PIC X(26).                                   " + Constants.NEWLINE +
                    "       01  FILLER.                                                              " + Constants.NEWLINE +
                    "          05  WS-FIELD-1           PIC X(80).                                   " + Constants.NEWLINE +
                    "          05  ws-Field-2           PIC X(80).                                   " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"Basic test\" =============*                             " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"Basic test\"                                              " + Constants.NEWLINE +
                    "           MOVE \"Basic test\"                                                 " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "            PERFORM 000-START                                                  " + Constants.NEWLINE +
                    "           ADD 1 TO UT-TEST-CASE-COUNT                                         " + Constants.NEWLINE +
                    "           SET UT-NORMAL-COMPARE TO TRUE                                       " + Constants.NEWLINE +
                    "           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                 " + Constants.NEWLINE +
                    "           MOVE WS-FIELD-1 TO UT-ACTUAL                                        " + Constants.NEWLINE +
                    "           MOVE \"Value1\"                                                     " + Constants.NEWLINE +
                    "               TO UT-EXPECTED                                                  " + Constants.NEWLINE +
                    "           SET UT-RELATION-EQ TO TRUE                                          " + Constants.NEWLINE +
                    "           PERFORM UT-CHECK-EXPECTATION                                        " + Constants.NEWLINE +
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
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "       000-START SECTION.                                                       " + Constants.NEWLINE +
                    "           MOVE \"Value1\" to WS-FIELD-1                                        " + Constants.NEWLINE +
                    "           EXIT SECTION                                                         " + Constants.NEWLINE +
                    "           .                                                                    "  + Constants.NEWLINE;
}


