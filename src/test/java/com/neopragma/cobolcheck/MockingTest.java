package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.testSuiteParser.KeywordExtractor;
import com.neopragma.cobolcheck.features.testSuiteParser.MockRepository;
import com.neopragma.cobolcheck.features.testSuiteParser.TestSuiteParser;
import com.neopragma.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import com.neopragma.cobolcheck.features.writer.CobolWriter;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.Writer;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MockingTest {

    private TestSuiteParser testSuiteParser;
    private TestSuiteParserController testSuiteParserController;
    private BufferedReader mockedReader;
    private MockRepository mockRepository;

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
        testSuiteParser = new TestSuiteParser(new KeywordExtractor());
        mockedReader = Mockito.mock(BufferedReader.class);
        testSuiteParserController = new TestSuiteParserController(mockedReader);
        cobolWriter = new CobolWriter(mockTestProgramSource);
        mockRepository = new MockRepository();
    }

    @Test
    public void it_creates_a_new_section_mock() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals(1, mockRepository.getMocks().size());
    }

    @Test
    public void section_mock_gets_correct_identifier() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals("000-START", mockRepository.getMocks().get(0).getIdentifier());
    }

    @Test
    public void section_mocks_generates_unique_identifiers() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";
        String str7 = "       MOCK SECTION 100-START";
        String str8 = "          MOVE \"something\" TO this";
        String str9 = "          MOVE \"something else\" TO other";
        String str10 = "       END-MOCK";
        String str11 = "       TESTCASE \"test case 2\"";
        String str12 = "       MOCK SECTION 000-START";
        String str13 = "          MOVE \"something\" TO this";
        String str14 = "          MOVE \"something else\" TO other";
        String str15 = "       END-MOCK";
        String str16 = "       TESTSUITE \"Test suite 2\"";
        String str17 = "       TESTCASE \"test case 1\"";
        String str18 = "       MOCK SECTION 000-START";
        String str19 = "          MOVE \"something\" TO this";
        String str20 = "          MOVE \"something else\" TO other";
        String str21 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19,
                str20, str21, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals("1-1-1-MOCK", mockRepository.getMocks().get(0).getGeneratedMockIdentifier());
        assertEquals("1-1-2-MOCK", mockRepository.getMocks().get(1).getGeneratedMockIdentifier());
        assertEquals("1-2-1-MOCK", mockRepository.getMocks().get(2).getGeneratedMockIdentifier());
        assertEquals("2-1-1-MOCK", mockRepository.getMocks().get(3).getGeneratedMockIdentifier());
    }

    @Test
    public void section_mock_gets_section_type() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals("SECTION", mockRepository.getMocks().get(0).getType());
    }

    @Test
    public void mock_gets_correct_lines() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add(str4);
        expected.add(str5);

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals(expected, mockRepository.getMocks().get(0).getLines());
    }

    @Test
    public void mock_gets_global_scope() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       MOCK SECTION 000-START";
        String str3 = "          MOVE \"something\" TO this";
        String str4 = "          MOVE \"something else\" TO other";
        String str5 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals("Global", mockRepository.getMocks().get(0).getScope().name());
    }

    @Test
    public void mock_gets_local_scope() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields, mockRepository);
        assertEquals("Local", mockRepository.getMocks().get(0).getScope().name());
    }

    @Test
    public void single_mock_section_gets_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("      *****************************************************************");
        expected.add("      *Sections called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       1-1-1-MOCK SECTION.");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.getProcedureDivisionTestCode(numericFields);

        List<String> actual = testSuiteParserController.generateMockSections();

        assertEquals(expected, actual);
    }

    @Test
    public void multiple_mock_sections_gets_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";
        String str7 = "       MOCK SECTION 100-START";
        String str8 = "          MOVE \"hey\" TO greeting";
        String str9 = "          MOVE \"bye\" TO greeting2";
        String str10 = "       END-MOCK";
        String str11 = "       TESTCASE \"test case 2\"";
        String str12 = "       MOCK SECTION 000-START";
        String str13 = "          ADD 1 TO WS-COUNT";
        String str14 = "          MOVE \"something else\" TO other";
        String str15 = "       END-MOCK";
        String str16 = "       TESTSUITE \"Test suite 2\"";
        String str17 = "       TESTCASE \"test case 1\"";
        String str18 = "       MOCK SECTION 000-START";
        String str19 = "          MOVE \"something\" TO this";
        String str20 = "          ADD 1 TO WS-COUNT";
        String str21 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("      *****************************************************************");
        expected.add("      *Sections called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       1-1-1-MOCK SECTION.");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");
        expected.add("       1-1-2-MOCK SECTION.");
        expected.add(str8);
        expected.add(str9);
        expected.add("       .");
        expected.add("");
        expected.add("       1-2-1-MOCK SECTION.");
        expected.add(str13);
        expected.add(str14);
        expected.add("       .");
        expected.add("");
        expected.add("       2-1-1-MOCK SECTION.");
        expected.add(str19);
        expected.add(str20);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19,
                str20, str21, null);

        testSuiteParserController.getProcedureDivisionTestCode(numericFields);

        List<String> actual = testSuiteParserController.generateMockSections();

        assertEquals(expected, actual);
    }

    @Test
    public void single_global_mock_perform_evaluate_is_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       MOCK SECTION 000-START";
        String str3 = "          MOVE \"something\" TO this";
        String str4 = "          MOVE \"something else\" TO other";
        String str5 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME");
        expected.add("                WHEN \"Name of test suite\" ALSO ANY");
        expected.add("                    PERFORM 1-0-1-MOCK");
        expected.add("           WHEN OTHER");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        testSuiteParserController.getProcedureDivisionTestCode(numericFields);

        List<String> actual = testSuiteParserController.generateMockPerformCalls("000-START");

        assertEquals(expected, actual);
    }

    @Test
    public void single_local_mock_perform_evaluate_is_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME");
        expected.add("                WHEN \"Name of test suite\" ALSO \"Name of test case\"");
        expected.add("                    PERFORM 1-1-1-MOCK");
        expected.add("           WHEN OTHER");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.getProcedureDivisionTestCode(numericFields);

        List<String> actual = testSuiteParserController.generateMockPerformCalls("000-START");

        assertEquals(expected, actual);
    }

    @Test
    public void multiple_mock_perform_evaluates_are_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       MOCK SECTION 000-START";
        String str3 = "          MOVE \"global\" TO this";
        String str4 = "          MOVE \"mock\" TO other";
        String str5 = "       END-MOCK";
        String str6 = "       TESTCASE \"Name of test case\"";
        String str7 = "       MOCK SECTION 000-START";
        String str8 = "          MOVE \"something\" TO this";
        String str9 = "          MOVE \"something else\" TO other";
        String str10 = "       END-MOCK";
        String str11 = "       MOCK SECTION 100-START";
        String str12 = "          MOVE \"hey\" TO greeting";
        String str13 = "          MOVE \"bye\" TO greeting2";
        String str14 = "       END-MOCK";
        String str15 = "       TESTCASE \"test case 2\"";
        String str16 = "       MOCK SECTION 000-START";
        String str17 = "          ADD 1 TO WS-COUNT";
        String str18 = "          MOVE \"something else\" TO other";
        String str19 = "       END-MOCK";
        String str20 = "       TESTSUITE \"Test suite 2\"";
        String str21 = "       TESTCASE \"test case 1\"";
        String str22 = "       MOCK SECTION 000-START";
        String str23 = "          MOVE \"something\" TO this";
        String str24 = "          ADD 1 TO WS-COUNT";
        String str25 = "       END-MOCK";

        List<String> expected1 = new ArrayList<>();
        expected1.add("            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME");
        expected1.add("                WHEN \"Name of test suite\" ALSO \"Name of test case\"");
        expected1.add("                    PERFORM 1-1-1-MOCK");
        expected1.add("                WHEN \"Name of test suite\" ALSO \"test case 2\"");
        expected1.add("                    PERFORM 1-2-1-MOCK");
        expected1.add("                WHEN \"Test suite 2\" ALSO \"test case 1\"");
        expected1.add("                    PERFORM 2-1-1-MOCK");
        expected1.add("                WHEN \"Name of test suite\" ALSO ANY");
        expected1.add("                    PERFORM 1-0-1-MOCK");
        expected1.add("           WHEN OTHER");

        List<String> expected2 = new ArrayList<>();
        expected2.add("            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME");
        expected2.add("                WHEN \"Name of test suite\" ALSO \"Name of test case\"");
        expected2.add("                    PERFORM 1-1-2-MOCK");
        expected2.add("           WHEN OTHER");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19,
                str20, str21, str22, str23, str24, str25, null);

        testSuiteParserController.getProcedureDivisionTestCode(numericFields);

        List<String> actual1 = testSuiteParserController.generateMockPerformCalls("000-START");
        List<String> actual2 = testSuiteParserController.generateMockPerformCalls("100-START");

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }




}
