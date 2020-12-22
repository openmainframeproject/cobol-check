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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class StringTokenizerExtractorTest implements Constants {

    private StringTokenizerExtractor extractor;

    @BeforeEach
    public void commonSetup() {
        Locale.setDefault(new Locale("en", "US"));
        extractor = new StringTokenizerExtractor(new Messages());
    }

    @Test
    public void when_the_line_is_a_null_reference_it_throws__probable_internal_logic_error() {
        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> extractor.extractTokensFrom(null));
        assertEquals("ERR001: sourceLine is null on entry to StringTokenizerExtractor.extractTokensFrom(sourceLine) method.", ex.getMessage());
    }

    @Test
    public void when_the_line_contains_only_a_newline_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(NEWLINE).size());
    }

    @Test
    public void when_the_line_contains_only_an_empty_string_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(EMPTY_STRING).size());
    }

    @Test
    public void when_the_line_contains_only_spaces_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom("                    ").size());
    }

    @Test
    public void when_the_line_contains_only_a_period_it_returns_an_empty_collection() {
        assertEquals(0, extractor.extractTokensFrom(PERIOD).size());
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

    private String makeCobolSourceLineContaining(String text) {
        return String.format("%73s", text).substring(6).toUpperCase();
    }
}
