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
    public void given_a_quoted_string_it_treats_the_string_as_a_single_token() {
        tokens = extractor.extractTokensFrom("\"A quoted string\"");
        assertEquals("\"A quoted string\"", tokens.get(0));
        assertEquals(1, tokens.size());
    }

    @Test
    public void given_a_two_word_keyword_it_treats_the_keyword_as_a_single_token() {
        tokens = extractor.extractTokensFrom("TO BE");
        assertEquals(1, tokens.size());
        assertEquals("TO BE", tokens.get(0));
    }

    @Test
    public void given_a_mixture_of_different_types_of_tokens_it_extracts_them_correctly() {
        tokens = extractor.extractTokensFrom("  EXPECT WS-FOO TO BE \"something\"");
        assertEquals(4, tokens.size());
        assertEquals("TO BE", tokens.get(2));
    }
}
