package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class TestSuiteConcatenatorIT implements Constants {
    private StringReader concatenatedTestSuites;
    private TestSuiteConcatenator concat;
    private static final String CONCATENATED_TEST_SUITES_CONFIG_KEY = "concatenated.test.suites";
    private static final String TEST_SUITE_PATH_CONFIG_KEY = "test.suite.path";
    private static final String DEFAULT_CONCATENATED_TEST_SUITES_PATH = "./ALLTESTS";
    private static final String TEST_SUITE_PATH_OPTION = "test-suite-path";
    private static final String pathToResults = "testsuites/concatenatedTestsuites";

    @Mock
    Config config;
    @Mock
    Messages messages;
    @Mock
    GetOpt options;

    @Test
    public void it_concatenates_two_test_suite_files_specified_with_full_filenames() throws IOException {
        when(config.getString(CONCATENATED_TEST_SUITES_CONFIG_KEY,
                DEFAULT_CONCATENATED_TEST_SUITES_PATH))
                .thenReturn(pathToResults);
        when(config.getString(TEST_SUITE_PATH_CONFIG_KEY, Constants.EMPTY_STRING))
                .thenReturn("testsuites/testsuite2");
        when(options.isSet(TEST_SUITE_PATH_OPTION))
                .thenReturn(true);
        when(options.getValueFor(TEST_SUITE_PATH_OPTION))
                .thenReturn("testsuites/testsuite1");
        when(config.getMessages())
                .thenReturn(messages);
        when(messages.get(any(),any()))
                .thenReturn(EMPTY_STRING);

        StringBuilder expectedResult = new StringBuilder();
        BufferedReader testSuiteReader;
        String line;
        for (String filename : new String[] { "testsuites/testsuite1", "testsuites/testsuite2" }) {
            testSuiteReader = new BufferedReader(new FileReader(filename));
            while ((line = testSuiteReader.readLine()) != null) {
                expectedResult.append(line);
            }
            testSuiteReader.close();
        }

        TestSuiteConcatenator concat = new TestSuiteConcatenator(config, options);

        Reader concatenatedTestSuite = concat.concatenateTestSuites();
        StringBuilder actualResult = new StringBuilder();
        testSuiteReader = new BufferedReader(new FileReader(pathToResults));
        while ((line = testSuiteReader.readLine()) != null) {
            actualResult.append(line);
        }
        testSuiteReader.close();

        assertEquals(expectedResult.toString(), actualResult.toString());
    }
}
