package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.parser.KeywordExtractor;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

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
        assertEquals("\'A single quoted string\'", tokens.get(0));
        assertEquals(1, tokens.size());
    }

    @Test
    public void given_a_two_word_keyword_it_treats_the_keyword_as_a_single_token() {
        tokens = extractor.extractTokensFrom("TO BE OR NOT TO BE");
        assertEquals("TO BE", tokens.get(0));
        assertEquals("OR", tokens.get(1));
        assertEquals("NOT", tokens.get(2));
        assertEquals("TO BE", tokens.get(3));
        assertEquals(4, tokens.size());
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

    @ParameterizedTest
    @MethodSource("numericLiteralProvider")
    public void it_handles_numeric_literals_in_various_positions_in_the_source_line(
            String sourceLine, int indexOfExpectedValue, String expectedValue, int numberOfTokens
    ) {
        tokens = extractor.extractTokensFrom(sourceLine);
        assertEquals(expectedValue, tokens.get(indexOfExpectedValue));
        assertEquals(numberOfTokens, tokens.size());
    }
    private static Stream<Arguments> numericLiteralProvider() {
        return Stream.of(
            Arguments.of("135", 0, "135", 1),
            Arguments.of("  TO BE 135  ", 1, "135", 2),
                Arguments.of("  TO BE 135.", 1, "135", 2),
            Arguments.of("135.64", 0, "135.64", 1),
            Arguments.of("135.64.", 0, "135.64", 1),
            Arguments.of("-52.7", 0, "-52.7", 1),
            Arguments.of("+8.06", 0, "+8.06", 1),
            Arguments.of("  \"number 537.52 inside string literal\"  ", 0, "\"number 537.52 inside string literal\"", 1)
        );
    }

    @ParameterizedTest
    @MethodSource("fieldNameProvider")
    public void it_handles_expect_field_to_equal_field(
            String sourceLine, List<String> expectedTokens
    ) {
        tokens = extractor.extractTokensFrom(sourceLine);
        assertEquals(expectedTokens, tokens);
    }
    private static Stream<Arguments> fieldNameProvider() {
        return Stream.of(
            Arguments.of("           EXPECT WS-FIELD-1 TO EQUAL WS-FIELD-2   ",
                          Arrays.asList("EXPECT", "WS-FIELD-1", "TO EQUAL", "WS-FIELD-2")),
            Arguments.of("        EXPECT WS-FIELD-1 TO BE WS-FIELD-2.",
                          Arrays.asList("EXPECT", "WS-FIELD-1", "TO BE", "WS-FIELD-2")),
            Arguments.of("      EXPECT WS-FIELD-1 NOT TO EQUAL WS-FIELD-2 ",
                          Arrays.asList("EXPECT", "WS-FIELD-1", "NOT", "TO EQUAL", "WS-FIELD-2"))
        );
    }

    @ParameterizedTest
    @MethodSource("symbolicRelationsProvider")
    public void it_handles_symbolic_relations_keywords(
            String sourceLine, List<String> expectedTokens) {
        tokens = extractor.extractTokensFrom(sourceLine);
        assertEquals(expectedTokens, tokens);
    }
    private static Stream<Arguments> symbolicRelationsProvider() {
        return Stream.of(
            Arguments.of("      EXPECT WS-FIELD-1 = WS-FIELD-2",
                        Arrays.asList("EXPECT", "WS-FIELD-1", "=", "WS-FIELD-2")),
                Arguments.of("      EXPECT WS-FIELD-1 != WS-FIELD-2",
                        Arrays.asList("EXPECT", "WS-FIELD-1", "!=", "WS-FIELD-2")),
                Arguments.of("      EXPECT WS-FIELD-1 > WS-FIELD-2",
                        Arrays.asList("EXPECT", "WS-FIELD-1", ">", "WS-FIELD-2")),
                Arguments.of("      EXPECT WS-FIELD-1 < WS-FIELD-2",
                        Arrays.asList("EXPECT", "WS-FIELD-1", "<", "WS-FIELD-2")),
                Arguments.of("      EXPECT WS-FIELD-1 >= WS-FIELD-2",
                        Arrays.asList("EXPECT", "WS-FIELD-1", ">=", "WS-FIELD-2")),
                Arguments.of("      EXPECT WS-FIELD-1 <= WS-FIELD-2",
                        Arrays.asList("EXPECT", "WS-FIELD-1", "<=", "WS-FIELD-2"))
                );
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
