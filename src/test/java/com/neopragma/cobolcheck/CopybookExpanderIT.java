/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class CopybookExpanderIT implements StringHelper {
    private CopybookExpander copybookExpander;
    private String expectedResult;
    private String testCopybookFilename;
    private String testCopybookBasename;
    private static String copybookFilenameSuffix;
    private static String pathToTestCobolCopybooks;

    @Mock
    private static Messages messages;
    @InjectMocks
    private static Config config;

    @BeforeAll
    public static void oneTimeSetup() {
//TODO: Solve the mystery - mock messages object is always null
//  We don't need a real Messages object for this test class
//        when(messages.get(anyString())).thenReturn(EMPTY_STRING);
//        doReturn(EMPTY_STRING).when(messages).get(anyString(), anyString());
//        config = new Config(messages);

        System.out.println("********** CopybookExpanderIT **********");

        config = new Config(new Messages());

        config.load("testconfig.properties");
        copybookFilenameSuffix = config.getApplicationFilenameSuffix();
        pathToTestCobolCopybooks = getPathFor("application.copybook.directory", "src/main/cobol/copy");
    }

    @BeforeEach
    public void commonSetup() {
        copybookExpander = new CopybookExpander(config, messages);
        testCopybookFilename = Constants.EMPTY_STRING;
        expectedResult = Constants.EMPTY_STRING;

    }

    @Test
    public void it_expands_a_simple_copybook() throws IOException {
        Writer expandedSource =
                runTestCase("COPY001-padded", "COPY001-padded");
        assertEquals(expectedResult, expandedSource.toString());
    }

    @Test
    public void it_expands_nested_copybooks_one_level_deep() throws IOException {
        Writer expandedSource =
                runTestCase("COPY002-padded", "EX002-padded");
        assertEquals(expectedResult, runTestCase("COPY002-padded", "EX002-padded").toString());
    }

    @Test
    public void it_expands_nested_copybooks_three_levels_deep() throws IOException {
        Writer expandedSource =
                runTestCase("COPY005-padded", "EX005-padded");
        assertEquals(expectedResult, expandedSource.toString());
    }

    @Test
    public void it_handles_lower_case_and_mixed_case_code() throws IOException {
        Writer expandedSource =
                runTestCase("mixed005-padded", "mixedex005-padded");
        assertEquals(expectedResult, expandedSource.toString());
    }

    @Test
    public void it_handles_copy_replacing() throws IOException {
        Writer expandedSource =
                runTestCase("COPYR001-padded",
                        "EXR001-padded",
                        new StringTuple("A", "ALPHA"),
                        new StringTuple("B", "BETA"),
                        new StringTuple("C", "CHARLIE"),
                        new StringTuple("D", "DELTA"));
        assertEquals(expectedResult, expandedSource.toString());
    }

    @ParameterizedTest
    @MethodSource("textPatternAndTestFilenameProvider")
    public void it_handles_pseudo_text_replacement(
            String pseudoTextPattern,
            String replacementText,
            String testCopybookFilename,
            String expectedResultFilename) throws IOException {
        Writer expandedSource =
                runTestCase(testCopybookFilename + "-padded",
                        expectedResultFilename + "-padded",
                        new StringTuple(pseudoTextPattern, replacementText));
        assertEquals(expectedResult, expandedSource.toString());
    }
    private static Stream<Arguments> textPatternAndTestFilenameProvider() {
        return Stream.of(
                Arguments.of("==XXX==", "NEW-TEXT","COPYP001", "EXP01"),
                Arguments.of("::XXX::", "NEW-TEXT", "COPYP002", "EXP01")
        );
    }


    private Writer runTestCase(String testCopybookBasename,
                               String expectedExpansionBasename) throws IOException {
            return runTestCase(testCopybookBasename,
                    expectedExpansionBasename,
                    new StringTuple(null, null));
    }
        private Writer runTestCase(String testCopybookBasename,
                String expectedExpansionBasename,
                StringTuple... textReplacement) throws IOException {
        testCopybookFilename = testCopybookBasename + copybookFilenameSuffix;
        expectedResult = getExpectedResult(expectedExpansionBasename + copybookFilenameSuffix);
        Writer expandedSource = new StringWriter();
        expandedSource = copybookExpander.expand(
                expandedSource,
                testCopybookBasename,
                copybookFilenameSuffix,
                textReplacement);
        return expandedSource;
    }

    private String getExpectedResult(String copybookFilename) throws IOException {
        return Files.readString(Path.of(pathToTestCobolCopybooks + copybookFilename));
    }

    private static String getPathFor(String configPropertyName, String defaultValue) {
        StringBuilder directoryName = new StringBuilder();
        directoryName.append(new File("./").getAbsolutePath());
        directoryName.append(Constants.FILE_SEPARATOR);
        directoryName.append(config.getString(configPropertyName, "src/main/cobol/copy"));
        if (!directoryName.toString().endsWith(Constants.FILE_SEPARATOR)) {
            directoryName.append(Constants.FILE_SEPARATOR);
        }
        return directoryName.toString();
    }
}
