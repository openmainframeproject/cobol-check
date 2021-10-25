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
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
public class GeneratorTest {

    private StringBuilder cobolSourceData;
    private StringWriter testProgramSource;
    private Generator generator;
    private TestSuiteParser testSuiteParser;
    private static final Messages messages = new Messages();
    private TokenExtractor tokenExtractor = new StringTokenizerExtractor(messages);
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
                generator.collectExpandedCopyStatements(Arrays.asList()));
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_throws_when_1st_token_in_copy_list_is_not_COPY() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                generator.collectExpandedCopyStatements(Arrays.asList("foo", "bar")));
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_throws_when_token_list_has_fewer_than_2_entries() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                generator.collectExpandedCopyStatements(Arrays.asList("COPY")));
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_recognizes_a_batch_file_IO_verb_on_a_source_line() {

    }

    @Test
    public void it_recognizes_paragraph_header_format(){
        String paragraphHeader = ("       5400-WRITE-OUTPUT-RECORD.");
        List<String> tokens = tokenExtractor.extractTokensFrom(paragraphHeader);
        String nextLine = "           WRITE OUTPUT-RECORD";
        assertTrue(generator.isParagraphHeaderFormat(tokens, paragraphHeader, nextLine));
    }

    @Test
    public void it_recognizes_paragraph_header_format_with_extra_spaces(){
        String paragraphHeader = ("       5400-WRITE-OUTPUT-RECORD        .      ");
        List<String> tokens = tokenExtractor.extractTokensFrom(paragraphHeader);
        String nextLine = "           WRITE OUTPUT-RECORD";
        assertTrue(generator.isParagraphHeaderFormat(tokens, paragraphHeader, nextLine));
    }

    @Test
    public void it_recognizes_paragraph_header_format_with_period_on_next_line(){
        String paragraphHeader = ("       5400-WRITE-OUTPUT-RECORD");
        List<String> tokens = tokenExtractor.extractTokensFrom(paragraphHeader);
        String nextLine = "       .";
        assertTrue(generator.isParagraphHeaderFormat(tokens, paragraphHeader, nextLine));
    }

    @Test
    public void it_returns_false_if_not_a_paragraph_header_format(){
        String sectionHeader = ("       000-START SECTION.");
        List<String> tokens = tokenExtractor.extractTokensFrom(sectionHeader);
        String nextLine = "           PERFORM WITH TEST BEFORE";
        assertFalse(generator.isParagraphHeaderFormat(tokens, sectionHeader, nextLine));
    }

    @Test
    public void it_returns_false_if_not_in_correct_area(){
        String sectionHeader = ("          000-START SECTION.");
        List<String> tokens = tokenExtractor.extractTokensFrom(sectionHeader);
        String nextLine = "           PERFORM WITH TEST BEFORE";
        assertFalse(generator.isParagraphHeaderFormat(tokens, sectionHeader, nextLine));
    }

    @Test
    public void it_returns_paragraph_name(){
        String paragraphHeader = ("       5400-WRITE-OUTPUT-RECORD.");
        List<String> tokens = tokenExtractor.extractTokensFrom(paragraphHeader);
        assertEquals("5400-WRITE-OUTPUT-RECORD", generator.getSectionOrParagraphName(tokens, paragraphHeader));
    }

    @Test
    public void it_returns_section_name(){
        String sectionHeader = ("       000-START SECTION.");
        List<String> tokens = tokenExtractor.extractTokensFrom(sectionHeader);
        assertEquals("000-START", generator.getSectionOrParagraphName(tokens, sectionHeader));
    }

    @Test
    public void it_returns_section_name_while_containing_sequence_number(){
        String sectionHeader = ("001200 000-START SECTION.");
        List<String> tokens = tokenExtractor.extractTokensFrom(sectionHeader);
        assertEquals("000-START", generator.getSectionOrParagraphName(tokens, sectionHeader));
    }

    @Test
    public void it_recognizes_sequence_number_area(){
        String seqNumber = ("001200 DATA DIVISION.");
        assertEquals(Area.SEQUENCE_NUMBER, generator.getBeginningArea(seqNumber, false));
    }

    @Test
    public void it_recognizes_indicator_area_while_ignoring_sequence_number_area(){
        String indicator = ("001200-              \"World\"");
        assertEquals(Area.INDICATOR, generator.getBeginningArea(indicator, true));
    }

    @Test
    public void it_recognizes_a_area(){
        String a         = ("       000-START SECTION.");
        assertEquals(Area.A, generator.getBeginningArea(a, false));
    }

    @Test
    public void it_recognizes_b_area(){
        String b         = ("           PERFORM 003-DO-SOMETHING");
        assertEquals(Area.B, generator.getBeginningArea(b, false));
    }

    @Test
    public void it_returns_none_if_outside_areas(){
        String none      = ("                                                                         Too many spaces!");
        assertEquals(Area.NONE, generator.getBeginningArea(none, false));
    }

    @Test
    public void it_returns_none_if_ignoring_sequence_area_but_string_is_too_short(){
        String none      = ("01");
        assertEquals(Area.NONE, generator.getBeginningArea(none, true));
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

