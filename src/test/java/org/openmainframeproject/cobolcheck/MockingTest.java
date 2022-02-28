package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.exceptions.ComponentMockedTwiceInSameScopeException;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.*;
import org.openmainframeproject.cobolcheck.features.writer.CobolWriter;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.NumericFields;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class MockingTest {

    private TestSuiteParser testSuiteParser;
    private TestSuiteParserController testSuiteParserController;
    private BufferedReader mockedReader;
    private MockRepository mockRepository;
    private BeforeAfterRepo beforeAfterRepo;

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
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository, beforeAfterRepo);
        mockedReader = Mockito.mock(BufferedReader.class);
        testSuiteParserController = new TestSuiteParserController(mockedReader);
        cobolWriter = new CobolWriter(mockTestProgramSource);
        numericFields = new NumericFields();
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

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
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

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("000-START", mockRepository.getMocks().get(0).getIdentifier());
    }

    @Test
    public void call_mock_gets_correct_identifier() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1'";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("'prog1'", mockRepository.getMocks().get(0).getIdentifier());
    }

    @Test
    public void call_mock_gets_correct_identifier_when_variable() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL VALUE-1";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("VALUE-1", mockRepository.getMocks().get(0).getIdentifier());
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

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("UT-1-1-1-MOCK", mockRepository.getMocks().get(0).getGeneratedMockIdentifier());
        assertEquals("UT-1-1-2-MOCK", mockRepository.getMocks().get(1).getGeneratedMockIdentifier());
        assertEquals("UT-1-2-1-MOCK", mockRepository.getMocks().get(2).getGeneratedMockIdentifier());
        assertEquals("UT-2-1-1-MOCK", mockRepository.getMocks().get(3).getGeneratedMockIdentifier());
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

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("SECTION", mockRepository.getMocks().get(0).getType());
    }

    @Test
    public void paragraph_mock_gets_paragraph_type() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK PARAGRAPH 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("PARAGRAPH", mockRepository.getMocks().get(0).getType());
    }

    @Test
    public void call_mock_gets_call_type() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK call 'prog1'";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals("CALL", mockRepository.getMocks().get(0).getType());
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

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals(expected, mockRepository.getMocks().get(0).getLines());
    }

    @Test
    public void call_mock_gets_correct_lines() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1'";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add(str4);
        expected.add(str5);

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals(expected, mockRepository.getMocks().get(0).getLines());
    }

    @Test
    public void call_mock_gets_correct_lines_with_args() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1' USING VALUE-1";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add(str4);
        expected.add(str5);

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals(expected, mockRepository.getMocks().get(0).getLines());
    }

    @Test
    public void call_mock_gets_correct_arguments() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1' USING BY CONTENT VALUE-1,";
        String str4 = "             BY REFERENCE VALUE-2,";
        String str5 = "             BY VALUE VALUE-3,";
        String str6 = "             VALUE-4, BY CONTENT VALUE-5";
        String str7 = "          MOVE \"something\" TO this";
        String str8 = "          MOVE \"something else\" TO other";
        String str9 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("CONTENT VALUE-1");
        expected.add("REFERENCE VALUE-2");
        expected.add("VALUE VALUE-3");
        expected.add("REFERENCE VALUE-4");
        expected.add("CONTENT VALUE-5");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, str8,
                str9, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
        assertEquals(expected, mockRepository.getMocks().get(0).getArguments());
    }

    @Test
    public void mock_gets_global_scope() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       MOCK SECTION 000-START";
        String str3 = "          MOVE \"something\" TO this";
        String str4 = "          MOVE \"something else\" TO other";
        String str5 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
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

        testSuiteParser.getParsedTestSuiteLines(mockedReader, numericFields);
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
        expected.add("      *Paragraphs called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       UT-1-1-1-MOCK.");
        expected.add("           ADD 1 TO UT-1-1-1-MOCK-COUNT");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(false);

        assertEquals(expected, actual);
    }

    @Test
    public void single_mock_section_gets_generated_correctly_with_comment() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("      *****************************************************************");
        expected.add("      *Paragraphs called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       UT-1-1-1-MOCK.");
        expected.add("      *****************************************************************");
        expected.add("      *Local mock of: SECTION: 000-START");
        expected.add("      *In testsuite: \"Name of test suite\"");
        expected.add("      *In testcase: \"Name of test case\"");
        expected.add("      *****************************************************************");
        expected.add("           ADD 1 TO UT-1-1-1-MOCK-COUNT");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(true);

        assertEquals(expected, actual);
    }

    @Test
    public void single_mock_paragraph_gets_generated_correctly_with_comment() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK PARAGRAPH 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("      *****************************************************************");
        expected.add("      *Paragraphs called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       UT-1-1-1-MOCK.");
        expected.add("      *****************************************************************");
        expected.add("      *Local mock of: PARAGRAPH: 000-START");
        expected.add("      *In testsuite: \"Name of test suite\"");
        expected.add("      *In testcase: \"Name of test case\"");
        expected.add("      *****************************************************************");
        expected.add("           ADD 1 TO UT-1-1-1-MOCK-COUNT");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(true);

        assertEquals(expected, actual);
    }

    @Test
    public void single_mock_call_gets_generated_correctly_with_comment() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1'";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("      *****************************************************************");
        expected.add("      *Paragraphs called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       UT-1-1-1-MOCK.");
        expected.add("      *****************************************************************");
        expected.add("      *Local mock of: CALL: 'prog1'");
        expected.add("      *In testsuite: \"Name of test suite\"");
        expected.add("      *In testcase: \"Name of test case\"");
        expected.add("      *****************************************************************");
        expected.add("           ADD 1 TO UT-1-1-1-MOCK-COUNT");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(true);

        assertEquals(expected, actual);
    }

    @Test
    public void single_mock_call_with_args_gets_generated_correctly_with_comment() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1' USING BY CONTENT V-1, V-2";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("      *****************************************************************");
        expected.add("      *Paragraphs called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       UT-1-1-1-MOCK.");
        expected.add("      *****************************************************************");
        expected.add("      *Local mock of: CALL: 'prog1'");
        expected.add("      *With args: CONTENT V-1, REFERENCE V-2");
        expected.add("      *In testsuite: \"Name of test suite\"");
        expected.add("      *In testcase: \"Name of test case\"");
        expected.add("      *****************************************************************");
        expected.add("           ADD 1 TO UT-1-1-1-MOCK-COUNT");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(true);

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
        expected.add("      *Paragraphs called when mocking");
        expected.add("      *****************************************************************");
        expected.add("       UT-1-1-1-MOCK.");
        expected.add("           ADD 1 TO UT-1-1-1-MOCK-COUNT");
        expected.add(str4);
        expected.add(str5);
        expected.add("       .");
        expected.add("");
        expected.add("       UT-1-1-2-MOCK.");
        expected.add("           ADD 1 TO UT-1-1-2-MOCK-COUNT");
        expected.add(str8);
        expected.add(str9);
        expected.add("       .");
        expected.add("");
        expected.add("       UT-1-2-1-MOCK.");
        expected.add("           ADD 1 TO UT-1-2-1-MOCK-COUNT");
        expected.add(str13);
        expected.add(str14);
        expected.add("       .");
        expected.add("");
        expected.add("       UT-2-1-1-MOCK.");
        expected.add("           ADD 1 TO UT-2-1-1-MOCK-COUNT");
        expected.add(str19);
        expected.add(str20);
        expected.add("       .");
        expected.add("");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19,
                str20, str21, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(false);

        assertEquals(expected, actual);
    }

    @Test
    public void no_mock_paragraphs_are_generated_if_no_mocks_exists() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "          EXPECT VALUE TO BE \"something\"";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockSections(false);

        assertTrue(actual.isEmpty());
    }

    @Test
    public void single_global_mock_perform_evaluate_is_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       MOCK SECTION 000-START";
        String str3 = "          MOVE \"something\" TO this";
        String str4 = "          MOVE \"something else\" TO other";
        String str5 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("            EVALUATE UT-TEST-SUITE-NAME");
        expected.add("                   ALSO UT-TEST-CASE-NAME");
        expected.add("                WHEN \"Name of test suite\"");
        expected.add("                   ALSO ANY");
        expected.add("                    PERFORM UT-1-0-1-MOCK");
        expected.add("           WHEN OTHER");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockPerformCalls("000-START",
                Constants.SECTION_TOKEN, new ArrayList<>());

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
        expected.add("            EVALUATE UT-TEST-SUITE-NAME");
        expected.add("                   ALSO UT-TEST-CASE-NAME");
        expected.add("                WHEN \"Name of test suite\"");
        expected.add("                   ALSO \"Name of test case\"");
        expected.add("                    PERFORM UT-1-1-1-MOCK");
        expected.add("           WHEN OTHER");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockPerformCalls("000-START",
                Constants.SECTION_TOKEN, new ArrayList<>());

        assertEquals(expected, actual);
    }

    @Test
    public void single_local_mock_call_perform_evaluate_is_generated_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1' USING VALUE-1";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("            EVALUATE UT-TEST-SUITE-NAME");
        expected.add("                   ALSO UT-TEST-CASE-NAME");
        expected.add("                WHEN \"Name of test suite\"");
        expected.add("                   ALSO \"Name of test case\"");
        expected.add("                    PERFORM UT-1-1-1-MOCK");
        expected.add("            END-EVALUATE");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockPerformCalls("'prog1'",
                Constants.CALL_TOKEN, Arrays.asList("REFERENCE VALUE-1"));

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
        expected1.add("            EVALUATE UT-TEST-SUITE-NAME");
        expected1.add("                   ALSO UT-TEST-CASE-NAME");
        expected1.add("                WHEN \"Name of test suite\"");
        expected1.add("                   ALSO \"Name of test case\"");
        expected1.add("                    PERFORM UT-1-1-1-MOCK");
        expected1.add("                WHEN \"Name of test suite\"");
        expected1.add("                   ALSO \"test case 2\"");
        expected1.add("                    PERFORM UT-1-2-1-MOCK");
        expected1.add("                WHEN \"Test suite 2\"");
        expected1.add("                   ALSO \"test case 1\"");
        expected1.add("                    PERFORM UT-2-1-1-MOCK");
        expected1.add("                WHEN \"Name of test suite\"");
        expected1.add("                   ALSO ANY");
        expected1.add("                    PERFORM UT-1-0-1-MOCK");
        expected1.add("           WHEN OTHER");

        List<String> expected2 = new ArrayList<>();
        expected2.add("            EVALUATE UT-TEST-SUITE-NAME");
        expected2.add("                   ALSO UT-TEST-CASE-NAME");
        expected2.add("                WHEN \"Name of test suite\"");
        expected2.add("                   ALSO \"Name of test case\"");
        expected2.add("                    PERFORM UT-1-1-2-MOCK");
        expected2.add("           WHEN OTHER");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19,
                str20, str21, str22, str23, str24, str25, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual1 = testSuiteParserController.generateMockPerformCalls("000-START",
                Constants.SECTION_TOKEN, new ArrayList<>());
        List<String> actual2 = testSuiteParserController.generateMockPerformCalls("100-START",
                Constants.SECTION_TOKEN, new ArrayList<>());

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }



    @Test
    public void no_evaluate_perform_is_generated_when_no_mock_present() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "          EXPECT VALUE TO BE \"something\"";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        testSuiteParserController.parseTestSuites(numericFields);
        testSuiteParserController.getProcedureDivisionTestCode();

        List<String> actual = testSuiteParserController.generateMockPerformCalls("000-START",
                Constants.SECTION_TOKEN, new ArrayList<>());

        assertTrue(actual.isEmpty());
    }

    @Test
    public void it_generates_working_storage_mock_counting_fields_correctly() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"something\" TO this";
        String str5 = "          MOVE \"something else\" TO other";
        String str6 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("       01  UT-MOCKS-GENERATED.");
        expected.add("           05  UT-1-1-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.");
        expected.add("           05  UT-1-1-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.");
        expected.add("           05  UT-1-1-1-MOCK-NAME        PIC X(40)");
        expected.add("                   VALUE \"SECTION 000-START\".");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, null);

        testSuiteParserController.parseTestSuites(numericFields);

        List<String> actual = testSuiteParserController.generateMockCountingFields();

        assertEquals(expected, actual);
    }

    @Test
    public void no_working_storage_mock_counting_fields_are_generated_if_no_mocks_is_present() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "          EXPECT VALUE TO BE \"something\"";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        testSuiteParserController.parseTestSuites(numericFields);

        List<String> actual = testSuiteParserController.generateMockCountingFields();

        assertTrue(actual.isEmpty());
    }

    @Test
    public void it_generates_lines_for_initialize_mock_count_section_correctly() throws IOException {
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
        String str21 = "       MOCK SECTION 000-START";
        String str22 = "          MOVE \"something\" TO this";
        String str23 = "          ADD 1 TO WS-COUNT";
        String str24 = "       END-MOCK";

        List<String> expected = new ArrayList<>();
        expected.add("       UT-INITIALIZE-MOCK-COUNT.");
        expected.add("      *****************************************************************");
        expected.add("      *Sets all global mock counters and expected count to 0");
        expected.add("      *****************************************************************");
        expected.add("           MOVE 0 TO UT-1-0-1-MOCK-COUNT");
        expected.add("           MOVE 0 TO UT-1-0-1-MOCK-EXPECTED");
        expected.add("           MOVE 0 TO UT-2-0-1-MOCK-COUNT");
        expected.add("           MOVE 0 TO UT-2-0-1-MOCK-EXPECTED");
        expected.add("           .");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, str16, str17, str18, str19,
                str20, str21, str22, str23, str24, null);

        testSuiteParserController.parseTestSuites(numericFields);

        List<String> actual = testSuiteParserController.generateMockCountInitializer();

        assertEquals(expected, actual);
    }

    @Test
    public void if_no_mocks_the_initialize_mock_count_paragraph_is_empty() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "          EXPECT VALUE TO BE \"something\"";

        List<String> expected = new ArrayList<>();
        expected.add("       UT-INITIALIZE-MOCK-COUNT.");
        expected.add("      *****************************************************************");
        expected.add("      *Sets all global mock counters and expected count to 0");
        expected.add("      *****************************************************************");
        expected.add("           .");

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        testSuiteParserController.parseTestSuites(numericFields);

        List<String> actual = testSuiteParserController.generateMockCountInitializer();

        assertEquals(expected, actual);
    }


    @Test
    public void it_throws_when_identical_mocks_are_in_same_global_scope() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       MOCK SECTION 000-START";
        String str3 = "          MOVE \"global\" TO this";
        String str4 = "          MOVE \"mock\" TO other";
        String str5 = "       END-MOCK";
        String str6 = "       MOCK SECTION 000-START";
        String str7 = "          MOVE \"global\" TO this";
        String str8 = "          MOVE \"mock\" TO other";
        String str9 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, null);

        String expectedMessage = "Mock for 000-START in testsuite: \"Name of test suite\", testcase: N/A" +
                " already exists in this Global testsuite scope.";

        Throwable ex = assertThrows(ComponentMockedTwiceInSameScopeException.class,
                () -> testSuiteParserController.parseTestSuites(numericFields));
        assertEquals(expectedMessage, ex.getMessage());
    }

    @Test
    public void it_throws_when_identical_mocks_are_in_same_local_scope() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK SECTION 000-START";
        String str4 = "          MOVE \"global\" TO this";
        String str5 = "          MOVE \"mock\" TO other";
        String str6 = "       END-MOCK";
        String str7 = "       MOCK SECTION 000-START";
        String str8 = "          MOVE \"global\" TO this";
        String str9 = "          MOVE \"mock\" TO other";
        String str10 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, null);

        String expectedMessage = "Mock for 000-START in testsuite: \"Name of test suite\", testcase: " +
                "\"Name of test case\" already exists in this Local testcase scope.";

        Throwable ex = assertThrows(ComponentMockedTwiceInSameScopeException.class,
                () -> testSuiteParserController.parseTestSuites(numericFields));
        assertEquals(expectedMessage, ex.getMessage());
    }

    @Test
    public void it_throws_when_identical_call_mocks_are_in_same_local_scope() throws IOException {
        String str1 = "       TESTSUITE \"Name of test suite\"";
        String str2 = "       TESTCASE \"Name of test case\"";
        String str3 = "       MOCK CALL 'prog1' USING V1, V2";
        String str4 = "          MOVE \"global\" TO this";
        String str5 = "          MOVE \"mock\" TO other";
        String str6 = "       END-MOCK";
        String str7 = "       MOCK CALL 'prog1' USING V1, V2";
        String str8 = "          MOVE \"global\" TO this";
        String str9 = "          MOVE \"mock\" TO other";
        String str10 = "       END-MOCK";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, null);

        String expectedMessage = "Mock for 'prog1' in testsuite: \"Name of test suite\", testcase: " +
                "\"Name of test case\" already exists with the given arguments in this Local testcase scope.";

        Throwable ex = assertThrows(ComponentMockedTwiceInSameScopeException.class,
                () -> testSuiteParserController.parseTestSuites(numericFields));
        assertEquals(expectedMessage, ex.getMessage());
    }










}
