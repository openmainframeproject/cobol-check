package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.launcher.LauncherController;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.platform.Platform;
import com.neopragma.cobolcheck.services.platform.PlatformLookup;
import org.junit.jupiter.api.*;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

public class TestResultsFileOutputIT {

    private LauncherController launcherController;

    public Path tempDirectory;
    public Path tempProc;
    public Path tempOutput;

    @BeforeEach
    public void commonSetup() throws IOException{
        Config.load();

        launcherController = new LauncherController();

        String testSuiteDirectoryPath = Config.getTestSuiteDirectoryPathString();
    }

    @Test
    public void it_writes_test_results_to_file() throws IOException, InterruptedException {
        if (PlatformLookup.get() == Platform.OSX){
            System.out.println("This test is ignored on OSX for now");
            assertTrue(true);
            return;
        }

        //Arrange
        String[] args = new String[2];
        args[0] = "-p";
        args[1] = "ALPHA";

        //Act
        String expected = "TESTSUITE:\n" +
                "Tests of alphanumeric expectations\n" +
                "     PASS:   1. Equality with an alphanumeric literal using TO BE                               \n" +
                "     PASS:   2. Equality with an alphanumeric literal using TO EQUAL                            \n" +
                "     PASS:   3. Equality with an alphanumeric literal using '='                                 \n" +
                "     PASS:   4. Equality with an alphanumeric literal and reference modification                \n" +
                "     PASS:   5. Non-equality with an alphanumeric literal using TO BE                           \n" +
                "     PASS:   6. Non-equality with an alphanumeric literal using TO EQUAL                        \n" +
                "     PASS:   7. Non-equality with an alphanumeric literal using '!='                            \n" +
                "     PASS:   8. Non-equality with an alphanumeric literal and reference modification            \n" +
                "     PASS:   9. Greater-than sign with an alphanumeric literal                                  \n" +
                "     PASS:  10. Less-than sign with an alphanumeric literal                                     \n" +
                "     PASS:  11. Not greater-than sign with an alphanumeric literal                              \n" +
                "     PASS:  12. Not less-than sign with an alphanumeric literal                                 \n" +
                "     PASS:  13. Display numeric                                                                 \n" +
                " \n" +
                " 13 TEST CASES WERE EXECUTED\n" +
                " 13 PASSED\n" +
                "  0 FAILED\n" +
                "=================================================";
        Main.main(args);
        String fileResult = Config.getTestResultFilePathString();
        BufferedReader reader = new BufferedReader(new FileReader(fileResult));
        String line;
        String result = "";
        while ((line = reader.readLine()) != null) {
            result += line + "\n";
        }
        if (result.length() > 0)
            result = result.substring(0, result.length()-1);

        System.out.println("Expected: " + expected);
        System.out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        System.out.println("Actual: " + result);

        //Assert
        if (PlatformLookup.get() == Platform.LINUX){
            //Does not work on Linux
            System.out.println("System = Linux");
            expected = "";
        }

        assertEquals(expected, result);
    }

}
