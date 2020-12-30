package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class KeywordsTest implements Constants {

    @ParameterizedTest
    @MethodSource("KeywordProvider")
    public void it_returns_the_keyword_record_for_a_given_key(
            String key,
            String expectedKeywordValue,
            List<String> expectedValidNextKey,
            KeywordAction expectedKeywordAction) {
        Keyword keyword = Keywords.getKeywordFor(key);
        assertEquals(expectedKeywordValue, keyword.value());
        assertEquals(expectedKeywordAction, keyword.keywordAction());
        assertEquals(expectedValidNextKey, keyword.validNextKey());
    }

    private static Stream<Arguments> KeywordProvider() {
        return Stream.of(
                Arguments.of(EXPECT_KEYWORD, EXPECT_KEYWORD,
                        List.of(FIELDNAME_KEYWORD),
                        KeywordAction.ACTUAL_FIELDNAME),
                Arguments.of(FIELDNAME_KEYWORD, EMPTY_STRING,
                        List.of(TO_BE_KEYWORD, NOT_KEYWORD, COBOL_TOKEN),
                        KeywordAction.FIELDNAME),
                Arguments.of(NOT_KEYWORD, NOT_KEYWORD,
                        List.of(TO_BE_KEYWORD),
                        KeywordAction.REVERSE_LOGIC),
                Arguments.of(TO_BE_KEYWORD, TO_BE_KEYWORD,
                        List.of(FIELDNAME_KEYWORD,
                                ALPHANUMERIC_LITERAL_KEYWORD,
                                NUMERIC_LITERAL_KEYWORD,
                                TRUE,
                                FALSE),
                        KeywordAction.EXPECTED_VALUE),
                Arguments.of(TESTSUITE_KEYWORD, TESTSUITE_KEYWORD,
                        List.of(ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.TESTSUITE_NAME)
        );
    }

    @Test
    public void when_the_token_is_an_alphanumeric_literal_it_does_not_try_to_look_up_the_literal_as_a_key() {
        Keyword keyword = Keywords.getKeywordFor("\"alphanumeric literal value\"");
        assertEquals(ALPHANUMERIC_LITERAL_KEYWORD, keyword.value());
        assertEquals(KeywordAction.FIELDNAME, keyword.keywordAction());
        assertEquals(List.of(EXPECT_KEYWORD, COBOL_TOKEN), keyword.validNextKey());
    }

}
