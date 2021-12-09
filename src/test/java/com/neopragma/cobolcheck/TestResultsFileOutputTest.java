package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.launcher.LauncherController;
import com.neopragma.cobolcheck.features.launcher.ProcessLauncher;
import com.neopragma.cobolcheck.features.launcher.ProcessOutputWriter;
import com.neopragma.cobolcheck.features.launcher.WindowsProcessLauncher;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import org.graalvm.compiler.core.match.MatchRule;
import org.junit.jupiter.api.*;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

public class TestResultsFileOutputTest {

    private LauncherController launcherController;

    public Path tempDirectory;
    public Path tempProc;
    public Path tempOutput;

    @BeforeEach
    public void commonSetup() throws IOException{
        Config.load();

        launcherController = new LauncherController();

        String testSuiteDirectoryPath = Config.getTestSuiteDirectoryPathString();

        tempDirectory = Paths.get("temp");
        tempProc = Paths.get("temp" + Constants.FILE_SEPARATOR+ "proc.cmd");
        tempOutput = Paths.get("temp" + Constants.FILE_SEPARATOR + "output.txt");

        if (!new File(tempDirectory.toString()).exists()){
            Files.createDirectory(tempDirectory);
        }

        BufferedWriter bw = new BufferedWriter(new FileWriter(tempProc.toString()));
        bw.write("echo A\n");
        bw.close();
    }

    @AfterEach
    public void removeTempFiles() throws IOException{
        deleteDir(new File(tempDirectory.toString()));
    }

    @Test
    public void it_creates_file_if_does_not_exist() throws IOException, InterruptedException {
        launcherController.runProgram(tempProc.toString(), tempOutput.toString(), false);
        assertEquals(true, new File(tempOutput.toString()).exists());
    }

    @Test
    public void it_writes_test_results_to_file() throws IOException, InterruptedException {
        //Arrange
        BufferedReader reader;

        //Act
        launcherController.runProgram(tempProc.toString(), tempOutput.toString(), false);
        reader = new BufferedReader(new FileReader(tempOutput.toString()));
        String outputLine = reader.readLine();
        reader.close();
        //Assert
        assertEquals("A", outputLine);

    }

    void deleteDir(File file) {
        File[] contents = file.listFiles();
        if (contents != null) {
            for (File f : contents) {
                if (! Files.isSymbolicLink(f.toPath())) {
                    deleteDir(f);
                }
            }
        }
        file.delete();
    }

}