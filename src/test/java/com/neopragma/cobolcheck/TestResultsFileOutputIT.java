package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.launcher.LauncherController;
import com.neopragma.cobolcheck.features.launcher.ProcessOutputWriter;
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
    public void it_creates_test_results_file_if_does_not_exist() throws IOException, InterruptedException {
        ProcessOutputWriter outputWriter = new ProcessOutputWriter();
        boolean fileExists = new File(Config.getTestResultFilePathString()).exists();
        assertTrue(fileExists);
    }



}
