package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandler;
import com.neopragma.cobolcheck.features.launcher.ProcessLauncher;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.filehelpers.PathHelper;
import com.neopragma.cobolcheck.services.platform.Platform;
import com.neopragma.cobolcheck.workers.Initializer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class InitializerTests {
    Initializer driver;

//    @BeforeAll
//    static void oneTimeSetup() {
//        Config.load("testconfig.properties");
//    }
//
//    @BeforeEach
//    void commonSetup() {
//        String[] args = {"-p", "ALPHA"};
//        ArgumentHandler options = new ArgumentHandler(args, Constants.COMMAND_lINE_OPTIONS);
//        driver = new Initializer(options);
//    }
//
//    @Test
//    void it_gets_correct_platform() {
//        Platform platform = Platform.WINDOWS;
//        ProcessLauncher launcher = driver.getPlatformSpecificLauncher(platform);
//        assertEquals("windows", launcher.getProcessConfigKeyPrefix());
//    }
//
//    @Test
//    void cobol_source_directory_exists() {
//        String path = driver.getCobolSourceDirectory();
//        File file = new File(path);
//        assertTrue(file.exists());
//    }
//
//    @Test
//    void test_source_out_path_exists() {
//        String path = PathHelper.getTestSourceOutPath();
//        File file = new File(path);
//        assertTrue(file.exists());
//    }
//
//    @Test
//    void finds_correct_file_suffix() {
//        String[] suffixes = {".cbl", ".cob", ".txt", ".java"};
//        List<String> suffixList = Arrays.asList(suffixes);
//        String fullPath = "fake\\fake\\file.txt";
//        String pathWithNoSuffix = "fake\\fake\\file";
//        MockedStatic<Files> mockedFiles = Mockito.mockStatic(Files.class);
//        mockedFiles.when(() -> Files.isRegularFile(Paths.get(fullPath)))
//                .thenReturn(true);
//        String appended = driver.appendMatchingFileSuffix(pathWithNoSuffix, suffixList);
//
//        mockedFiles.close();
//
//        assertEquals(fullPath, appended);
//    }
}
