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
    private String[] nullArray = null;

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

    
    @Test
    public void it_inserts_code_correctly_when_call_has_exception_handling_with_end_call_terminator()
            throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       CALL \"PROGRAM\" USING VALUE-1";
        String s4 = "           ON EXCEPTION";
        String s5 = "           PERFORM 100-WELCOME";
        String s6 = "       END-CALL.";

        String t1 = "           TestSuite \"test\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1,nullArray);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities
                .getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(Utilities.getTrimmedList(expected2), actual);
    }

    @Test
    public void it_inserts_code_correctly_when_call_has_exception_handling_with_period_terminator() throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       CALL \"PROGRAM\" USING VALUE-1";
        String s4 = "           ON EXCEPTION";
        String s5 = "           PERFORM 100-WELCOME";
        String s6 = "       .";

        String t1 = "           TestSuite \"test\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1,nullArray);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities
                .getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(Utilities.getTrimmedList(expected3), actual);
    }

    @Test
    public void it_inserts_code_correctly_when_call_has_different_exception_handling()
            throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       CALL \"PROGRAM\" USING VALUE-1";
        String s4 = "           ON EXCEPTION";
        String s5 = "           DISPLAY \"HELLO WORLD\"";
        String s6 = "       .";

        String t1 = "           TestSuite \"test\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1,nullArray);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities
                .getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(Utilities.getTrimmedList(expected4), actual);
    }

    @Test
    public void it_inserts_code_correctly_when_call_has_a_new_call_as_exception_handling()
            throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       CALL \"PROGRAM\" USING VALUE-1";
        String s4 = "           ON EXCEPTION";
        String s5 = "           CALL \"PROGRAM2\" USING VALUE-1";
        String s6 = "           ON EXCEPTION";
        String s7 = "           DISPLAY \"HELLO WORLD\"";
        String s8 = "           END-CALL";
        String s9 = "       END-CALL";
        String s10 = "      DISPLAY \"NO COMMENTS\"";

        String t1 = "           TestSuite \"test\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1,nullArray);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities
                .getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(Utilities.getTrimmedList(expected5), actual);
    }

    @Test
    public void it_inserts_code_correctly_when_each_call_in_series_has_an_exception_handling()
            throws IOException {
        String s1 = "       WORKING-STORAGE SECTION.";
        String s2 = "       PROCEDURE DIVISION.";
        String s3 = "       CALL \"PROGRAM\" USING DATA-1 ON EXCEPTION";
        String s4 = "           DISPLAY \"ERROR\".";
        String s5 = "       CALL \"PROGRAM\" USING DATA-1 ON EXCEPTION";
        String s6 = "           DISPLAY \"ERROR\".";

        String t1 = "           TestSuite \"test\"";

        Mockito.when(mockedInterpreterReader.readLine()).thenReturn(s1, s2, s3, s4, s5, s6, null);
        Mockito.when(mockedParserReader.readLine()).thenReturn(t1,nullArray);

        generator = new Generator(interpreterController, writerController, testSuiteParserController);

        List<String> actual = Utilities
                .getTrimmedList(Utilities.removeBoilerPlateCode(writer.toString(), boilerPlateTags));

        assertEquals(Utilities.getTrimmedList(expected6), actual);
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
    private String expected2 = 
                    "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                       " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"test\" =============*                                      " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"test\"                                                       " + Constants.NEWLINE +
                    "           MOVE \"test\"                                                          " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "      *CALL \"PROGRAM\" USING VALUE-1                                           " + Constants.NEWLINE +
                    "      *    ON EXCEPTION                                                         " + Constants.NEWLINE +
                    "      *    PERFORM 100-WELCOME                                                  " + Constants.NEWLINE +
                    "      *END-CALL.                                                                  " + Constants.NEWLINE +
                    "            CONTINUE                                                            " + Constants.NEWLINE +
                    "           .                                                                    ";
            
    private String expected3 = 
                    "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                       " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"test\" =============*                                      " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"test\"                                                       " + Constants.NEWLINE +
                    "           MOVE \"test\"                                                          " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "      *CALL \"PROGRAM\" USING VALUE-1                                           " + Constants.NEWLINE +
                    "      *    ON EXCEPTION                                                         " + Constants.NEWLINE +
                    "      *    PERFORM 100-WELCOME.                                                 " + Constants.NEWLINE +
                    "            CONTINUE                                                            " + Constants.NEWLINE +
                    "           .                                                                    ";
            
    private String expected4 = 
                    "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                       " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"test\" =============*                                      " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"test\"                                                       " + Constants.NEWLINE +
                    "           MOVE \"test\"                                                          " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "      *CALL \"PROGRAM\" USING VALUE-1                                           " + Constants.NEWLINE + 
                    "      *    ON EXCEPTION                                                         " + Constants.NEWLINE + 
                    "      *    DISPLAY \"HELLO WORLD\".                                             " + Constants.NEWLINE +
                    "            CONTINUE                                                            " + Constants.NEWLINE +
                    "           .                                                                    ";
            
    private String expected5 = 
                    "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                       " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"test\" =============*                                      " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                                 " + Constants.NEWLINE +
                    "           DISPLAY \"test\"                                                       " + Constants.NEWLINE +
                    "           MOVE \"test\"                                                          " + Constants.NEWLINE +
                    "               TO UT-TEST-SUITE-NAME                                            " + Constants.NEWLINE +
                    "       UT-BEFORE-EACH.                                                          " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed before each Test Case                                  " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-AFTER-EACH.                                                           " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *This is performed after each Test Case                                   " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                 " + Constants.NEWLINE +
                    "      *CALL \"PROGRAM\" USING VALUE-1                                           " + Constants.NEWLINE +
                    "      *    ON EXCEPTION                                                         " + Constants.NEWLINE + 
                    "      *    CALL \"PROGRAM2\" USING VALUE-1                                      " + Constants.NEWLINE +
                    "      *    ON EXCEPTION                                                         " + Constants.NEWLINE +
                    "      *    DISPLAY \"HELLO WORLD\"                                              " + Constants.NEWLINE +
                    "      *    END-CALL                                                             " + Constants.NEWLINE + 
                    "      *END-CALL                                                                 " + Constants.NEWLINE +
                    "            CONTINUE                                                            " + Constants.NEWLINE +
                    "      DISPLAY \"NO COMMENTS\"                                                   " + Constants.NEWLINE;
    
    private String expected6 =
                    "       WORKING-STORAGE SECTION.                                                 " + Constants.NEWLINE +
                    "       PROCEDURE DIVISION.                                                      " + Constants.NEWLINE +
                    "           PERFORM UT-INITIALIZE                                                " + Constants.NEWLINE +
                    "      *============= \"test\" =============*                                    " + Constants.NEWLINE +
                    "           DISPLAY \"TESTSUITE:\"                                               " + Constants.NEWLINE +
                    "           DISPLAY \"test\"                                                     " + Constants.NEWLINE +
                    "           MOVE \"test\"                                                        " + Constants.NEWLINE +
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
                    "       UT-INITIALIZE-MOCK-COUNT.                                                " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "      *Sets all global mock counters and expected count to 0                    " + Constants.NEWLINE +
                    "      *****************************************************************         " + Constants.NEWLINE +
                    "           CONTINUE                                                             " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "                                                                                " + Constants.NEWLINE +
                    "      *CALL \"PROGRAM\" USING DATA-1 ON EXCEPTION                               " + Constants.NEWLINE +
                    "      *    DISPLAY \"ERROR\".                                                   " + Constants.NEWLINE +
                    "            CONTINUE                                                            " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE +
                    "      *CALL \"PROGRAM\" USING DATA-1 ON EXCEPTION                               " + Constants.NEWLINE +
                    "      *    DISPLAY \"ERROR\".                                                   " + Constants.NEWLINE +
                    "            CONTINUE                                                            " + Constants.NEWLINE +
                    "           .                                                                    " + Constants.NEWLINE;

}


