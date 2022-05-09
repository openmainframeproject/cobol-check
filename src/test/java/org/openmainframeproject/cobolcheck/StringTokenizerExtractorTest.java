package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.features.interpreter.StringTokenizerExtractor;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class StringTokenizerExtractorTest {

    private StringTokenizerExtractor extractor;

    @BeforeEach
    public void commonSetup() {
        Locale.setDefault(new Locale("en", "US"));
        extractor = new StringTokenizerExtractor();
    }

    @Test
    public void when_the_line_is_a_null_reference_it_throws__probable_internal_logic_error() {
        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> extractor.extractTokensFrom(null));
        assertEquals("ERR001: sourceLine is null on entry to StringTokenizerExtractor.extractTokensFrom(sourceLine) method.", ex.getMessage());
    }

    @Test
    public void when_the_line_contains_only_a_newline_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(Constants.NEWLINE).size());
    }

    @Test
    public void when_the_line_contains_only_an_empty_string_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(Constants.EMPTY_STRING).size());
    }

    @Test
    public void when_the_line_contains_only_spaces_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom("                    ").size());
    }

    @Test
    public void when_the_line_contains_only_a_period_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(Constants.PERIOD).size());
    }

    @Test
    public void it_gets_a_simple_string_as_token() {
        String line = "MOVE 'hello there' TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("'hello there'");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void it_gets_a_long_string_as_token() {
        String line = "MOVE \"This is some kind of long string\" TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"This is some kind of long string\"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void end_of_token_can_start_string_start_of_token_can_end_string() {
        String line = "MOVE\" This is some kind of long string \"TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\" This is some kind of long string \"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void only_the_starting_string_char_can_end_the_string_1() {
        String line = "MOVE \"STRING' STRING\" TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"STRING' STRING\"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void only_the_starting_string_char_can_end_the_string_2() {
        String line = "MOVE \"STRING ' STRING\" TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"STRING ' STRING\"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void only_the_starting_string_char_can_end_the_string_3() {
        String line = "MOVE \"STRING 'STRING\" TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"STRING 'STRING\"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void only_the_starting_string_char_can_end_the_string_4() {
        String line = "MOVE \"THIS WON'T AND CAN'T END THE STRING\" TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"THIS WON'T AND CAN'T END THE STRING\"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void strings_without_spaces_is_still_its_own_token() {
        String line = "MOVE\"HI THERE\"TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"HI THERE\"");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void many_string_chars_with_varying_spaces() {
        String line = "MOVE\"HI '''THERE'\"+'HEY\"YOU\"' +\"''''\"\"''''\"+' HI' TO VALUE-1";
        List<String> expected = new ArrayList<>();
        expected.add("MOVE");
        expected.add("\"HI '''THERE'\"");
        expected.add("+");
        expected.add("'HEY\"YOU\"'");
        expected.add("+");
        expected.add("\"''''\"");
        expected.add("\"''''\"");
        expected.add("+");
        expected.add("' HI'");
        expected.add("TO");
        expected.add("VALUE-1");
        assertEquals(expected, extractor.extractTokensFrom(line));
    }

    @Test
    public void when_the_line_is_a_comment_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(makeCobolSourceLineContaining("      * this is a comment")).size());
    }

    @ParameterizedTest
    @CsvSource({
            "       Procedure Division.,PROCEDURE DIVISION",
            "       procedure division,PROCEDURE DIVISION",
            "       PROCEDURE DIVISION USING,PROCEDURE DIVISION",
            "       Procedure    Division,PROCEDURE DIVISION",
            "       ID DIVISION.,ID DIVISION",
            "       IDENTIFICATION DIVISION.,IDENTIFICATION DIVISION",
            "       environment division.,ENVIRONMENT DIVISION",
            "       Data Division.,DATA DIVISION"
    })
    public void it_treats_division_headers_as_single_tokens(String inputValue, String expectedValue) {
        assertEquals(expectedValue,
                extractor.extractTokensFrom(makeCobolSourceLineContaining(inputValue)).get(0));
    }

    @ParameterizedTest
    @CsvSource({
            "       Configuration Section.,CONFIGURATION SECTION",
            "       Input-Output Section.,INPUT-OUTPUT SECTION",
            "       File Section.,FILE SECTION",
            "       Working-Storage Section,WORKING-STORAGE SECTION",
            "       Local-Storage Section,LOCAL-STORAGE SECTION",
            "       Linkage Section,LINKAGE SECTION"
    })
    public void it_treats_section_headers_as_single_tokens(String inputValue, String expectedValue) {
        assertEquals(expectedValue,
                extractor.extractTokensFrom(makeCobolSourceLineContaining(inputValue)).get(0));
    }

    @ParameterizedTest
    @CsvSource({
            "       FILE CONTROL.,FILE CONTROL",
            "       File Control,FILE CONTROL",
            "       FILE STATUS,FILE STATUS",
            "       File Status,FILE STATUS",
            "       FILE SECTION.,FILE SECTION",
            "       File Section.,FILE SECTION"
    })
    public void it_distinguishes_FILE_CONTROL_and_FILE_STATUS_and_FILE_SECTION(
            String inputValue, String expectedValue) {
        assertEquals(expectedValue,
                extractor.extractTokensFrom(makeCobolSourceLineContaining(inputValue)).get(0));
    }

    private String makeCobolSourceLineContaining(String text) {
        return String.format("%73s", text).substring(6).toUpperCase();
    }
}
