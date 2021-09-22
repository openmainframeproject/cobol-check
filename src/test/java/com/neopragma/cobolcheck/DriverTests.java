package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.graalvm.compiler.core.match.MatchRule;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.File;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class DriverTests {
    Driver driver;
    private static final Messages messages = new Messages();
    private static final Config config = new Config(messages);

    @BeforeAll
    static void oneTimeSetup() {
        config.load("testconfig.properties");
    }

    @BeforeEach
    void commonSetup() {
        String[] args = {"-p", "ALPHA"};
        GetOpt options = new GetOpt(args, Constants.COMMAND_lINE_OPTIONS, config);
        driver = new Driver(config, options);
    }

    @Test
    void it_gets_correct_platform() {
        Platform platform = Platform.WINDOWS;
        ProcessLauncher launcher = driver.getPlatformSpecificLauncher(platform);
        assertEquals("windows", launcher.getProcessConfigKeyPrefix());
    }

    @Test
    void cobol_source_directory_exists() {
        String path = driver.getCobolSourceDirectory();
        File file = new File(path);
        assertTrue(file.exists());
    }

    @Test
    void test_source_out_path_exists() {
        String path = driver.getTestSourceOutPath();
        File file = new File(path);
        assertTrue(file.exists());
    }

    @Test
    void finds_correct_file_suffix() {
        String[] suffixes = {".cbl", ".cob", ".txt", ".java"};
        List<String> suffixList = Arrays.asList(suffixes);
        String fullPath = "fake\\fake\\file.txt";
        String pathWithNoSuffix = "fake\\fake\\file";
        MockedStatic<Files> mockedFiles = Mockito.mockStatic(Files.class);
        mockedFiles.when(() -> Files.isRegularFile(Paths.get(fullPath)))
                .thenReturn(true);
        String appended = driver.appendMatchingFileSuffix(pathWithNoSuffix, suffixList);

        mockedFiles.close();

        assertEquals(fullPath, appended);
    }
}
