package org.openmainframeproject.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.TestSuiteConcatenator;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class TestSuiteConcatenatorIT {
    private StringReader concatenatedTestSuites;
    private TestSuiteConcatenator concat;
    private static final String pathToResults = "testsuites/concatenatedTestsuites";

    @BeforeAll
    public static void commonsSetup(){
        Config.load();
    }

    @Test
    public void it_concatenates_two_test_suite_files_specified_with_full_filenames() throws IOException {
        MockedStatic<Config> mockedConfig = Mockito.mockStatic(Config.class);
        mockedConfig.when(() -> Config.getConcatenatedTestSuitesPath())
                .thenReturn(pathToResults);
        StringBuilder expectedResult = new StringBuilder();
        String line;
        BufferedReader testSuiteReader;
        for (String filename : new String[] { "src/test/cobol/GREETING/GreetingByType.cut" }) {
            testSuiteReader = new BufferedReader(new FileReader(filename));
            while ((line = testSuiteReader.readLine()) != null) {
                expectedResult.append(line);
            }
            testSuiteReader.close();
        }

        TestSuiteParserController tspController = new TestSuiteParserController("GreetingByType.cut");
        tspController.concatenateTestSuites("src/test/cobol/GREETING/");

        StringBuilder actualResult = new StringBuilder();
        testSuiteReader = new BufferedReader(new FileReader(pathToResults));
        while ((line = testSuiteReader.readLine()) != null) {
            actualResult.append(line);
        }
        testSuiteReader.close();
        mockedConfig.close();
        assertEquals(expectedResult.toString(), actualResult.toString());
    }
}
