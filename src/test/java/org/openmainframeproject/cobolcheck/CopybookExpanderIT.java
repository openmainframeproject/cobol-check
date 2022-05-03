package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.features.interpreter.CopybookExpander;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.StringTuple;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static java.nio.file.Files.readAllBytes;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
public class CopybookExpanderIT {
    private static final String applicationSourceFilenameSuffix = ".CBL";
    private CopybookExpander copybookExpander;
    private List<String> expectedResult;
    private String testCopybookFilename;
    private String testCopybookBasename;
    private static String pathToTestCobolCopybooks;

    @BeforeAll
    public static void oneTimeSetup() {
        Config.load("testconfig.properties");
        pathToTestCobolCopybooks = getPathFor("application.copybook.directory", "src/main/cobol/copy");
    }

    @BeforeEach
    public void commonSetup() {
        copybookExpander = new CopybookExpander();
        testCopybookFilename = Constants.EMPTY_STRING;
        expectedResult = new ArrayList<>();

    }

    @Test
    public void it_expands_a_simple_copybook() throws IOException {
        List<String> expandedSource =
                runTestCase("COPY001-padded", "COPY001-padded");
        assertEquals(expectedResult, removeTrailingSpacesFromLines(expandedSource));
    }

    @Test
    public void it_expands_nested_copybooks_one_level_deep() throws IOException {
        List<String> expandedSource =
                runTestCase("COPY002-padded", "EX002-padded");
        assertEquals(expectedResult, removeTrailingSpacesFromLines(expandedSource));
    }

    @Test
    public void it_expands_nested_copybooks_three_levels_deep() throws IOException {
        List<String> expandedSource =
                runTestCase("COPY005-padded", "EX005-padded");
        assertEquals(expectedResult, removeTrailingSpacesFromLines(expandedSource));
    }

    @Test
    public void it_handles_lower_case_and_mixed_case_code() throws IOException {
        List<String> expandedSource =
                runTestCase("mixed005-padded", "mixedex005-padded");
        assertEquals(expectedResult, removeTrailingSpacesFromLines(expandedSource));
    }

    @Test
    public void it_handles_copy_replacing() throws IOException {
        List<String> expandedSource =
                runTestCase("COPYR001-padded",
                        "EXR001-padded",
                        new StringTuple("A", "ALPHA"),
                        new StringTuple("B", "BETA"),
                        new StringTuple("C", "CHARLIE"),
                        new StringTuple("D", "DELTA"));
        assertEquals(expectedResult, removeTrailingSpacesFromLines(expandedSource));
    }

    @ParameterizedTest
    @MethodSource("textPatternAndTestFilenameProvider")
    public void it_handles_pseudo_text_replacement(
            String pseudoTextPattern,
            String replacementText,
            String testCopybookFilename,
            String expectedResultFilename) throws IOException {
        List<String> expandedSource =
                runTestCase(testCopybookFilename + "-padded",
                        expectedResultFilename + "-padded",
                        new StringTuple(pseudoTextPattern, replacementText));
        assertEquals(expectedResult, removeTrailingSpacesFromLines(expandedSource));
    }
    private static Stream<Arguments> textPatternAndTestFilenameProvider() {
        return Stream.of(
                Arguments.of("==XXX==", "NEW-TEXT","COPYP001", "EXP01"),
                Arguments.of("::XXX::", "NEW-TEXT", "COPYP002", "EXP01")
        );
    }


    private List<String> runTestCase(String testCopybookBasename,
                                     String expectedExpansionBasename) throws IOException {
            return runTestCase(testCopybookBasename,
                    expectedExpansionBasename,
                    new StringTuple(null, null));
    }
    private List<String> runTestCase(String testCopybookBasename,
                String expectedExpansionBasename,
                StringTuple... textReplacement) throws IOException {
        testCopybookFilename = testCopybookBasename + applicationSourceFilenameSuffix;
        expectedResult = getExpectedResult(expectedExpansionBasename + applicationSourceFilenameSuffix);
        List<String> expandedLines = new ArrayList<>();
            expandedLines = copybookExpander.expand(expandedLines, testCopybookBasename, textReplacement);
        return expandedLines;
    }

    private List<String> getExpectedResult(String copybookFilename) throws IOException {
        File f = new File(pathToTestCobolCopybooks + copybookFilename);
        String s = new String(readAllBytes(f.toPath()));
        String[] lines = s.split("\n");
        return removeTrailingSpacesFromLines(Arrays.asList(lines));
    }

    private static String getPathFor(String configPropertyName, String defaultValue) {
        StringBuilder directoryName = new StringBuilder();
        directoryName.append(new File("./").getAbsolutePath());
        directoryName.append(Constants.FILE_SEPARATOR);
        directoryName.append(Config.getString(configPropertyName, "src/main/cobol/copy"));
        if (!directoryName.toString().endsWith(Constants.FILE_SEPARATOR)) {
            directoryName.append(Constants.FILE_SEPARATOR);
        }
        return directoryName.toString();
    }

    //Needed as 'expected' and 'actual' have different trailing spaces, even
    //though their values are essentially the same.
    private List<String> removeTrailingSpacesFromLines(List<String> lines){
        List<String> newLines = new ArrayList<>();
        for (String line : lines){
            newLines.add(StringHelper.removeTrailingSpaces(line));
        }
        return newLines;
    }
}
