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

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
public class GeneratorTest {

    private StringBuilder cobolSourceData;
    private StringWriter testProgramSource;
    private Generator generator;
    private TestSuiteParser testSuiteParser;
    private static final Messages messages = new Messages();
    private static final Config config = new Config(messages);
    private NumericFields numericFields;

    @Mock
    Reader mockTestSuite;

    @Mock
    KeywordExtractor keywordExtractor;

    @BeforeAll
    static void oneTimeSetup() {
        config.load("testconfig.properties");
    }

    @BeforeEach
    void commonSetup() {
        cobolSourceData = new StringBuilder();
        testProgramSource = new StringWriter();
        generator = new Generator(
                keywordExtractor,
                config);
        testSuiteParser = new TestSuiteParser(
                keywordExtractor,
                config);
    }

    @Test
    void cobol_source_cannot_be_null__probable_internal_logic_error() {
        assertThrows(PossibleInternalLogicErrorException.class, () -> generator.mergeTestSuite(mockTestSuite, null, testProgramSource));
    }

    @Test
    void cobol_source_cannot_be_empty__probable_internal_logic_error() {
        cobolSourceData = new StringBuilder();
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                mergeTestSuiteAndVerifyResults(mockTestSuite, cobolSourceData, testProgramSource));
        assertTrue(ex.getMessage().contains("empty input stream"));
    }

    @Test
    void it_formats_a_Cobol_line_based_on_a_String_value() throws IOException {
        String originalText = "           MOVE ALPHA TO BETA.";
        String expectedLine = "           MOVE ALPHA TO BETA.                                                  ";
        expectedLine += Constants.NEWLINE;
        Writer cobolOutWriter = new StringWriter();
        testSuiteParser.writeCobolLine(originalText, cobolOutWriter);
        assertEquals(expectedLine, cobolOutWriter.toString());
    }

    @Test
    void it_formats_a_Cobol_continuation_line_based_on_a_long_String_value() throws IOException {
        String originalText = "           TESTCASE: ''This testcase name makes the line far, far too long for Cobol.''";
        String expectedLine1 = "           TESTCASE: ''This testcase name makes the line far, far too lo        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      -    \"ng for Cobol.''                                                     ";
        expectedLine2 += Constants.NEWLINE;
        Writer cobolOutWriter = new StringWriter();
        testSuiteParser.writeCobolLine(originalText, cobolOutWriter);
        assertEquals(expectedLine1 + expectedLine2, cobolOutWriter.toString());
    }

    @Test
    public void it_throws_when_copy_token_list_is_empty_for_copybook_expansion() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                generator.collectExpandedCopyStatements(List.of()));
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_throws_when_1st_token_in_copy_list_is_not_COPY() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                generator.collectExpandedCopyStatements(List.of("foo", "bar")));
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_throws_when_token_list_has_fewer_than_2_entries() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                generator.collectExpandedCopyStatements(List.of("COPY")));
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_recognizes_a_batch_file_IO_verb_on_a_source_line() {

    }

    private void loadInputData(String... lines) {
        for (String line : lines) {
            cobolSourceData.append(line);
        }
    }

    private void mergeTestSuiteAndVerifyResults(Reader testSuite,
                                                StringBuilder cobolSourceData,
                                                Writer testProgramSource) throws IOException {
        StringReader cobolProgramSource = new StringReader(cobolSourceData.toString());
        Writer mergedCobolData = generator.mergeTestSuite(testSuite, cobolProgramSource, testProgramSource);
        assertEquals(cobolSourceData.toString(), mergedCobolData.toString());
    }

}

