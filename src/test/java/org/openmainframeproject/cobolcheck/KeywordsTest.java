package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.features.testSuiteParser.KeywordAction;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.Keywords;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.Keyword;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class KeywordsTest {

    @ParameterizedTest
    @MethodSource("KeywordProvider")
    public void it_returns_the_keyword_record_for_a_given_key(
            String key,
            String expectedKeywordValue,
            List<String> expectedValidNextKey,
            KeywordAction expectedKeywordAction) {
        Keyword keyword = Keywords.getKeywordFor(key, false);
        assertEquals(expectedKeywordValue, keyword.value());
        assertEquals(expectedKeywordAction, keyword.keywordAction());
        assertEquals(expectedValidNextKey, keyword.getValidNextKeys(null));
    }

    private static Stream<Arguments> KeywordProvider() {
        return Stream.of(
                Arguments.of(Constants.EXPECT_KEYWORD, Constants.EXPECT_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD),
                        KeywordAction.ACTUAL_FIELDNAME),
                Arguments.of(Constants.FIELDNAME_KEYWORD, Constants.FIELDNAME_KEYWORD,
                        Arrays.asList(Constants.TO_BE_KEYWORD,
                                Constants.NOT_KEYWORD,
                                Constants.NOT_EQUAL_SIGN_KEYWORD,
                                Constants.TO_EQUAL_KEYWORD,
                                Constants.EQUAL_SIGN_KEYWORD,
                                Constants.GREATER_THAN_SIGN_KEYWORD,
                                Constants.LESS_THAN_SIGN_KEYWORD,
                                Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                                Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                                Constants.HAPPENED_KEYWORD,
                                Constants.NEVER_HAPPENED_KEYWORD,
                                Constants.USING_TOKEN,
                                Constants.COBOL_TOKEN,
                                Constants.FIELDNAME_KEYWORD,
                                Constants.BY_REFERENCE_TOKEN,
                                Constants.BY_CONTENT_TOKEN,
                                Constants.BY_VALUE_TOKEN,
                                Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                                Constants.ENDMOCK_KEYWORD),
                        KeywordAction.FIELDNAME),
                Arguments.of(Constants.NOT_KEYWORD, Constants.NOT_KEYWORD,
                        Arrays.asList(Constants.TO_BE_KEYWORD,
                                Constants.TO_EQUAL_KEYWORD,
                                Constants.EQUAL_SIGN_KEYWORD,
                                Constants.NOT_EQUAL_SIGN_KEYWORD,
                                Constants.GREATER_THAN_SIGN_KEYWORD,
                                Constants.LESS_THAN_SIGN_KEYWORD,
                                Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                                Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD),
                        KeywordAction.REVERSE_LOGIC),
                Arguments.of(Constants.TO_BE_KEYWORD, Constants.TO_BE_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE),
                Arguments.of(Constants.TESTSUITE_KEYWORD, Constants.TESTSUITE_KEYWORD,
                        Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.TESTSUITE_NAME),
                Arguments.of(Constants.EQUAL_SIGN_KEYWORD, Constants.EQUAL_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE),
                Arguments.of(Constants.NOT_EQUAL_SIGN_KEYWORD, Constants.NOT_EQUAL_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.REVERSE_LOGIC),
                Arguments.of(Constants.GREATER_THAN_SIGN_KEYWORD, Constants.GREATER_THAN_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE),
                Arguments.of(Constants.LESS_THAN_SIGN_KEYWORD, Constants.LESS_THAN_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE),
                Arguments.of(Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD, Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE),
                Arguments.of(Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD, Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
    }

    @Test
    public void when_the_token_is_an_alphanumeric_literal_it_does_not_try_to_look_up_the_literal_as_a_key() {
        Keyword keyword = Keywords.getKeywordFor("\"alphanumeric literal value\"", false);
        assertEquals(Constants.ALPHANUMERIC_LITERAL_KEYWORD, keyword.value());
        assertEquals(KeywordAction.FIELDNAME, keyword.keywordAction());
        assertEquals(Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                Constants.EXPECT_KEYWORD,
                Constants.COBOL_TOKEN,
                Constants.TESTSUITE_KEYWORD,
                Constants.TESTCASE_KEYWORD,
                Constants.MOCK_KEYWORD,
                Constants.VERIFY_KEYWORD,
                Constants.BEFORE_EACH_TOKEN,
                Constants.BEFORE_EACH_TOKEN_HYPHEN,
                Constants.AFTER_EACH_TOKEN,
                Constants.AFTER_EACH_TOKEN_HYPHEN,
                Constants.HAPPENED_KEYWORD,
                Constants.NEVER_HAPPENED_KEYWORD,
                Constants.USING_TOKEN,
                Constants.ENDMOCK_KEYWORD), keyword.getValidNextKeys(null));
    }

    @Test
    public void when_the_token_is_a_numeric_literal_it_does_not_try_to_look_up_the_literal_as_a_key() {
        Keyword keyword = Keywords.getKeywordFor("5473.19", false);
        assertEquals(Constants.NUMERIC_LITERAL_KEYWORD, keyword.value());
        assertEquals(KeywordAction.FIELDNAME, keyword.keywordAction());
        assertEquals(Arrays.asList(Constants.EXPECT_KEYWORD, Constants.COBOL_TOKEN, Constants.TESTSUITE_KEYWORD,
                Constants.TESTCASE_KEYWORD, Constants.MOCK_KEYWORD, Constants.VERIFY_KEYWORD, Constants.TIME_KEYWORD,
                        Constants.TIMES_KEYWORD), keyword.getValidNextKeys(null));
    }

    @Test
    public void when_the_token_is_an_alphanumeric_literal_that_starts_with_a_digit_it_recognizes_the_token_as_a_cobol_token() {
        Keyword keyword = Keywords.getKeywordFor("2000-PARAGRAPH-NAME", false);
        assertEquals(Constants.COBOL_TOKEN, keyword.value());
        assertEquals(KeywordAction.COBOL_STATEMENT, keyword.keywordAction());
        assertEquals(Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                Constants.NUMERIC_LITERAL_KEYWORD,
                Constants.BOOLEAN_VALUE,
                Constants.FIELDNAME_KEYWORD,
                Constants.EXPECT_KEYWORD,
                Constants.COBOL_TOKEN,
                Constants.TESTSUITE_KEYWORD,
                Constants.TESTCASE_KEYWORD,
                Constants.MOCK_KEYWORD,
                Constants.ENDMOCK_KEYWORD,
                Constants.VERIFY_KEYWORD,
                Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                Constants.GREATER_THAN_SIGN_KEYWORD,
                Constants.LESS_THAN_SIGN_KEYWORD,
                Constants.NOT_KEYWORD,
                Constants.EQUAL_SIGN_KEYWORD,
                Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                Constants.TIME_KEYWORD,
                Constants.TIMES_KEYWORD), keyword.getValidNextKeys(null));
    }

    @Test
    public void when_the_token_is_TRUE_it_is_treated_as_a_boolean_expected_result() {
        Keyword keyword = Keywords.getKeywordFor("TRUE", false);
        assertEquals(Constants.BOOLEAN_VALUE, keyword.value());
        assertEquals(KeywordAction.BOOLEAN_COMPARE, keyword.keywordAction());
        assertEquals(Arrays.asList(Constants.EXPECT_KEYWORD,
                Constants.COBOL_TOKEN,
                Constants.TESTSUITE_KEYWORD,
                Constants.TESTCASE_KEYWORD,
                Constants.MOCK_KEYWORD,
                Constants.VERIFY_KEYWORD), keyword.getValidNextKeys(null));
    }

    @Test
    public void when_the_token_is_FALSE_it_is_treated_as_a_boolean_expected_result() {
        Keyword keyword = Keywords.getKeywordFor("FALSE", false);
        assertEquals(Constants.BOOLEAN_VALUE, keyword.value());
        assertEquals(KeywordAction.BOOLEAN_COMPARE, keyword.keywordAction());
        assertEquals(Arrays.asList(Constants.EXPECT_KEYWORD,
                Constants.COBOL_TOKEN,
                Constants.TESTSUITE_KEYWORD,
                Constants.TESTCASE_KEYWORD,
                Constants.MOCK_KEYWORD,
                Constants.VERIFY_KEYWORD), keyword.getValidNextKeys(null));
    }

}
