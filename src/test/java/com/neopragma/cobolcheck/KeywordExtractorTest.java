package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class KeywordExtractorTest {

    private KeywordExtractor extractor = new KeywordExtractor();
    private List<String> tokens;

    @Test
    public void given_three_tokens_with_no_leading_whitespace_it_finds_three_tokens() {
        tokens = extractor.extractTokensFrom("A B C");
        assertEquals(3, tokens.size());
    }

    @Test
    public void given_three_tokens_with_leading_and_trailing_whitespace_it_finds_three_tokens() {
        tokens = extractor.extractTokensFrom("  A    B  C   ");
        assertEquals(3, tokens.size());
    }

    @Test
    public void given_a_double_quoted_string_it_treats_the_string_as_a_single_token() {
        tokens = extractor.extractTokensFrom("\"A double quoted string\"");
        assertEquals("\"A double quoted string\"", tokens.get(0));
        assertEquals(1, tokens.size());
    }

    @Test
    public void given_a_single_quoted_string_it_treats_the_string_as_a_single_token() {
        tokens = extractor.extractTokensFrom("\'A single quoted string\'");
        assertEquals("\"A single quoted string\"", tokens.get(0));
        assertEquals(1, tokens.size());
    }

    @Test
    public void given_a_two_word_keyword_it_treats_the_keyword_as_a_single_token() {
        tokens = extractor.extractTokensFrom("TO BE");
        assertEquals("TO BE", tokens.get(0));
        assertEquals(1, tokens.size());
    }

    @Test
    public void given_the_first_word_in_a_two_word_keyword_and_the_second_token_matches_the_expected_second_word_it_knows_it_is_not_a_match() {
        tokens = extractor.extractTokensFrom("TO BEST-WORLD");
        assertEquals("TO", tokens.get(0));
        assertEquals("BEST-WORLD", tokens.get(1));
        assertEquals(2, tokens.size());
    }

    @Test
    public void given_the_first_word_in_a_two_word_keyword_it_knows_the_second_word_is_not_a_match() {
        tokens = extractor.extractTokensFrom("TO WS-FIELDNAME");
        assertEquals("TO", tokens.get(0));
        assertEquals("WS-FIELDNAME", tokens.get(1));
        assertEquals(2, tokens.size());
    }

    @Test
    public void given_a_mixture_of_different_types_of_tokens_it_extracts_them_correctly() {
        tokens = extractor.extractTokensFrom("  EXPECT WS-FOO TO BE \"something\"");
        assertEquals("TO BE", tokens.get(2));
        assertEquals(4, tokens.size());
    }

    @Test
    public void it_removes_periods_used_as_statement_delimiters() {
        tokens = extractor.extractTokensFrom("           MOVE A TO B.");
        assertEquals("B", tokens.get(3));
    }
}
