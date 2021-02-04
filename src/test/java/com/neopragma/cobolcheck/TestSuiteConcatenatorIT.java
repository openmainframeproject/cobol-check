package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class TestSuiteConcatenatorIT {
    private StringReader concatenatedTestSuites;
    private TestSuiteConcatenator concat;
    private static final String pathToResults = "testsuites/concatenatedTestsuites";

    @Mock
    Config config;
    @Mock
    Messages messages;
    @Mock
    GetOpt options;

    @Test
    public void it_concatenates_two_test_suite_files_specified_with_full_filenames() throws IOException {
        when(config.getString(Constants.CONCATENATED_TEST_SUITES_CONFIG_KEY,
                Constants.DEFAULT_CONCATENATED_TEST_SUITES_PATH))
                .thenReturn(pathToResults);
        when(options.getValueFor(Constants.TESTS_OPTION))
                .thenReturn("GreetingByType");
        when(config.getMessages())
                .thenReturn(messages);
        StringBuilder expectedResult = new StringBuilder();
        String line;
        BufferedReader testSuiteReader;
        for (String filename : new String[] { "src/test/cobol/GREETING/GreetingByType" }) {
            testSuiteReader = new BufferedReader(new FileReader(filename));
            while ((line = testSuiteReader.readLine()) != null) {
                expectedResult.append(line);
            }
            testSuiteReader.close();
        }

        TestSuiteConcatenator concat = new TestSuiteConcatenator(config, options);

        Reader concatenatedTestSuite =
                concat.concatenateTestSuites("src/test/cobol/GREETING/");
        StringBuilder actualResult = new StringBuilder();
        testSuiteReader = new BufferedReader(new FileReader(pathToResults));
        while ((line = testSuiteReader.readLine()) != null) {
            actualResult.append(line);
        }
        testSuiteReader.close();
        assertEquals(expectedResult.toString(), actualResult.toString());
    }
}
