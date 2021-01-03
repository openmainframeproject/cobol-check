package com.neopragma.cobolcheck;

import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.StringReader;

@ExtendWith(MockitoExtension.class)
public class TestSuiteConcatenatorIT implements Constants {
    private StringReader concatenatedTestSuites;
    private TestSuiteConcatenator concat;
    private static final String CONCATENATED_TEST_SUITES_CONFIG_KEY = "concatenated.test.suites";
    private static final String DEFAULT_CONCATENATED_TEST_SUITES_PATH = "./ALLTESTS";
    private static final String TESTS_OPTION = "tests";
    private static final String pathToResults = "testsuites/concatenatedTestsuites";

    @Mock
    Config config;
    @Mock
    Messages messages;
    @Mock
    GetOpt options;

//    @Test
//    public void it_concatenates_two_test_suite_files_specified_with_full_filenames() throws IOException {
//        when(config.getString(CONCATENATED_TEST_SUITES_CONFIG_KEY,
//                DEFAULT_CONCATENATED_TEST_SUITES_PATH))
//                .thenReturn(pathToResults);
//        when(config.getString(TEST_SUITE_DIRECTORY_CONFIG_KEY, Constants.CURRENT_DIRECTORY))
//                .thenReturn("src/test/cobol");
//        when(options.isSet(TESTS_OPTION))
//                .thenReturn(true);
//        when(options.getValueFor(TESTS_OPTION))
//                .thenReturn("GREETING-TEST");
//        when(config.getMessages())
//                .thenReturn(messages);
//        when(messages.get(any(),any()))
//                .thenReturn(EMPTY_STRING);
//
//        StringBuilder expectedResult = new StringBuilder();
//        BufferedReader testSuiteReader;
//        String line;
//        for (String filename : new String[] { "src/test/cobol/GREETING-TEST" }) {
//            testSuiteReader = new BufferedReader(new FileReader(filename));
//            while ((line = testSuiteReader.readLine()) != null) {
//                expectedResult.append(line);
//            }
//            testSuiteReader.close();
//        }
//
//        TestSuiteConcatenator concat = new TestSuiteConcatenator(config, options);
//
//        Reader concatenatedTestSuite = concat.concatenateTestSuites();
//        StringBuilder actualResult = new StringBuilder();
//        testSuiteReader = new BufferedReader(new FileReader(pathToResults));
//        while ((line = testSuiteReader.readLine()) != null) {
//            actualResult.append(line);
//        }
//        testSuiteReader.close();
//
//        assertEquals(expectedResult.toString(), actualResult.toString());
//    }
}
