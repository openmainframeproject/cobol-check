package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.features.launcher.LauncherController;
import org.openmainframeproject.cobolcheck.features.launcher.ProcessOutputWriter;
import org.openmainframeproject.cobolcheck.services.Config;
import org.junit.jupiter.api.*;

import java.io.*;
import java.nio.file.Path;

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

        String testSuiteDirectoryPath = Config.getTestSourceDirectoryPathString();
    }

    @Test
    public void it_creates_test_results_file_if_does_not_exist() throws IOException, InterruptedException {
        ProcessOutputWriter outputWriter = new ProcessOutputWriter();
        boolean fileExists = new File(Config.getTestResultFilePathString()).exists();
        assertTrue(fileExists);
    }



}
