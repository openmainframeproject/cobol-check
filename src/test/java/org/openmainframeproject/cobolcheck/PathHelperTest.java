package org.openmainframeproject.cobolcheck;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class PathHelperTest {

    @BeforeAll
    static void oneTimeSetup() {
        Config.load("testconfig.properties");
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
        String appended = PathHelper.appendMatchingFileSuffix(pathWithNoSuffix, suffixList);

        mockedFiles.close();

        assertEquals(fullPath, appended);
    }
}
