package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.util.*;

/**
 * This is a container for Keyword records. It is used when parsing test suites to identify cobol-check
 * keywords and handle them appropriately.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Keywords {
    private static final Map<String, Keyword> keywordInfo;
    private static final List<String> mockTypes;

    static {
        keywordInfo = new HashMap<>();
        keywordInfo.put(Constants.TESTSUITE_KEYWORD,
                new Keyword(Constants.TESTSUITE_KEYWORD,
                        Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.TESTSUITE_NAME));
        keywordInfo.put(Constants.TESTCASE_KEYWORD,
                new Keyword(Constants.TESTCASE_KEYWORD,
                        Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NEW_TESTCASE));
        keywordInfo.put(Constants.EXPECT_KEYWORD,
                new Keyword(Constants.EXPECT_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD),
                        KeywordAction.ACTUAL_FIELDNAME));
        keywordInfo.put(Constants.FIELDNAME_KEYWORD,
                new Keyword(Constants.FIELDNAME_KEYWORD,
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
                        KeywordAction.FIELDNAME));
        keywordInfo.put(Constants.NOT_KEYWORD,
                new Keyword(Constants.NOT_KEYWORD,
                        Arrays.asList(Constants.TO_BE_KEYWORD,
                                Constants.TO_EQUAL_KEYWORD,
                                Constants.EQUAL_SIGN_KEYWORD,
                                Constants.NOT_EQUAL_SIGN_KEYWORD,
                                Constants.GREATER_THAN_SIGN_KEYWORD,
                                Constants.LESS_THAN_SIGN_KEYWORD,
                                Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                                Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD),
                        KeywordAction.REVERSE_LOGIC));
        keywordInfo.put(Constants.NOT_EQUAL_SIGN_KEYWORD,
                new Keyword(Constants.NOT_EQUAL_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.REVERSE_LOGIC));
        keywordInfo.put(Constants.TO_BE_KEYWORD,
                new Keyword(Constants.TO_BE_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.TO_EQUAL_KEYWORD,
                new Keyword(Constants.TO_EQUAL_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.EQUAL_SIGN_KEYWORD,
                new Keyword(Constants.EQUAL_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.GREATER_THAN_SIGN_KEYWORD,
                new Keyword(Constants.GREATER_THAN_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                new Keyword(Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.LESS_THAN_SIGN_KEYWORD,
                new Keyword(Constants.LESS_THAN_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                new Keyword(Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.BOOLEAN_VALUE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                new Keyword(Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                        Arrays.asList(Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                                Constants.TO_BE_KEYWORD,
                                Constants.TO_EQUAL_KEYWORD,
                                Constants.NOT_KEYWORD,
                                Constants.EQUAL_SIGN_KEYWORD,
                                Constants.NOT_EQUAL_SIGN_KEYWORD,
                                Constants.GREATER_THAN_SIGN_KEYWORD,
                                Constants.LESS_THAN_SIGN_KEYWORD,
                                Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                                Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                                Constants.COBOL_TOKEN),
                        KeywordAction.COBOL_STATEMENT));
        keywordInfo.put(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                new Keyword(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                        Arrays.asList(Constants.EXPECT_KEYWORD,
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
                                Constants.ENDMOCK_KEYWORD),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(Constants.NUMERIC_LITERAL_KEYWORD,
                new Keyword(Constants.NUMERIC_LITERAL_KEYWORD,
                        Arrays.asList(Constants.EXPECT_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD,
                                Constants.TIME_KEYWORD,
                                Constants.TIMES_KEYWORD),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(Constants.COBOL_TOKEN,
                new Keyword(Constants.COBOL_TOKEN,
                        Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
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
                                Constants.PARENTHESIS_ENCLOSED_KEYWORD),
                        KeywordAction.COBOL_STATEMENT));
        keywordInfo.put(Constants.BOOLEAN_VALUE,
                new Keyword(Constants.BOOLEAN_VALUE,
                        Arrays.asList(Constants.EXPECT_KEYWORD,
                                Constants.COBOL_TOKEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD),
                        KeywordAction.BOOLEAN_COMPARE));
        keywordInfo.put(Constants.BEFORE_EACH_TOKEN,
                new Keyword(Constants.BEFORE_EACH_TOKEN,
                        Arrays.asList(Constants.END_BEFORE_TOKEN),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.END_BEFORE_TOKEN,
                new Keyword(Constants.END_BEFORE_TOKEN,
                        Arrays.asList(Constants.AFTER_EACH_TOKEN,
                                Constants.AFTER_EACH_TOKEN_HYPHEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.AFTER_EACH_TOKEN,
                new Keyword(Constants.AFTER_EACH_TOKEN,
                        Arrays.asList(Constants.END_AFTER_TOKEN),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.END_AFTER_TOKEN,
                new Keyword(Constants.END_AFTER_TOKEN,
                        Arrays.asList(Constants.COBOL_TOKEN,
                                Constants.BEFORE_EACH_TOKEN,
                                Constants.BEFORE_EACH_TOKEN_HYPHEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD),
                        KeywordAction.NONE));
        //TODO: Remove hyphen keyword option? Backwards compatibility
        keywordInfo.put(Constants.BEFORE_EACH_TOKEN_HYPHEN,
                new Keyword(Constants.BEFORE_EACH_TOKEN_HYPHEN,
                        Arrays.asList(Constants.END_BEFORE_TOKEN),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.AFTER_EACH_TOKEN_HYPHEN,
                new Keyword(Constants.AFTER_EACH_TOKEN_HYPHEN,
                        Arrays.asList(Constants.END_AFTER_TOKEN),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.MOCK_KEYWORD,
                new Keyword(Constants.MOCK_KEYWORD,
                        Arrays.asList(Constants.MOCK_TYPE),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.ENDMOCK_KEYWORD,
                new Keyword(Constants.ENDMOCK_KEYWORD,
                        Arrays.asList(Constants.COBOL_TOKEN,
                                Constants.BEFORE_EACH_TOKEN,
                                Constants.BEFORE_EACH_TOKEN_HYPHEN,
                                Constants.AFTER_EACH_TOKEN,
                                Constants.AFTER_EACH_TOKEN_HYPHEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD,
                                Constants.EXPECT_KEYWORD),
                        KeywordAction.NONE));

        keywordInfo.put(Constants.MOCK_TYPE,
                new Keyword(Constants.MOCK_TYPE,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NONE));

        keywordInfo.put(Constants.USING_TOKEN,
                new Keyword(Constants.USING_TOKEN,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD,
                                Constants.BY_REFERENCE_TOKEN,
                                Constants.BY_CONTENT_TOKEN,
                                Constants.BY_VALUE_TOKEN),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.BY_REFERENCE_TOKEN,
                new Keyword(Constants.BY_REFERENCE_TOKEN,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.BY_CONTENT_TOKEN,
                new Keyword(Constants.BY_CONTENT_TOKEN,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.BY_VALUE_TOKEN,
                new Keyword(Constants.BY_VALUE_TOKEN,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.VERIFY_KEYWORD,
                new Keyword(Constants.VERIFY_KEYWORD,
                        Arrays.asList(Constants.MOCK_TYPE),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.NEVER_HAPPENED_KEYWORD,
                new Keyword(Constants.NEVER_HAPPENED_KEYWORD,
                        Arrays.asList(Constants.COBOL_TOKEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.HAPPENED_KEYWORD,
                new Keyword(Constants.HAPPENED_KEYWORD,
                        Arrays.asList(Constants.ONCE_KEYWORD, Constants.AT_LEAST_KEYWORD,
                                Constants.NO_MORE_THAN_KEYWORD, Constants.NUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.ONCE_KEYWORD,
                new Keyword(Constants.ONCE_KEYWORD,
                        Arrays.asList(Constants.COBOL_TOKEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.AT_LEAST_KEYWORD,
                new Keyword(Constants.AT_LEAST_KEYWORD,
                        Arrays.asList(Constants.ONCE_KEYWORD, Constants.NUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.NO_MORE_THAN_KEYWORD,
                new Keyword(Constants.NO_MORE_THAN_KEYWORD,
                        Arrays.asList(Constants.ONCE_KEYWORD, Constants.NUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.TIME_KEYWORD,
                new Keyword(Constants.TIME_KEYWORD,
                        Arrays.asList(Constants.COBOL_TOKEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD),
                        KeywordAction.NONE));
        keywordInfo.put(Constants.TIMES_KEYWORD,
                new Keyword(Constants.TIMES_KEYWORD,
                        Arrays.asList(Constants.COBOL_TOKEN,
                                Constants.TESTSUITE_KEYWORD,
                                Constants.TESTCASE_KEYWORD,
                                Constants.MOCK_KEYWORD,
                                Constants.VERIFY_KEYWORD),
                        KeywordAction.NONE));

        //TODO: Add other types that can be mocked
        mockTypes = Arrays.asList(Constants.SECTION_TOKEN, Constants.PARAGRAPH_TOKEN, Constants.PARA_TOKEN, Constants.CALL_TOKEN);
    }



    public static Keyword getKeywordFor(String key, boolean expectFieldName) {
        Keyword result = null;
        if (key != null && ! key.isEmpty()) {
            Log.debug("Key: " + key);
            if (key.startsWith("\"") || key.startsWith("'")) {
                key = Constants.ALPHANUMERIC_LITERAL_KEYWORD;
            }
            if (key.startsWith("(")){
                key = Constants.PARENTHESIS_ENCLOSED_KEYWORD;
            } else {
                if (Character.isDigit(key.charAt(0))) {
                    boolean isNumeric = true;
                    for (char digit : key.toCharArray()) {
                        if (!Character.isDigit(digit) &&
                                digit != '.' &&
                                digit != ',') {
                            isNumeric = false;
                            break;
                        }
                    }
                    if (isNumeric) {
                        key = Constants.NUMERIC_LITERAL_KEYWORD;
                    }
                } else if (key.equals(Constants.ZERO_TOKEN)){
                    key = Constants.NUMERIC_LITERAL_KEYWORD;
                }else {
                    if (key.equals("TRUE") || key.equals("FALSE")) {
                        key = Constants.BOOLEAN_VALUE;
                    }
                } if (mockTypes.contains(key)){
                    key = Constants.MOCK_TYPE;
                }
            }
        }
        if (expectFieldName)
            result = keywordInfo.getOrDefault(key, keywordInfo.get(Constants.FIELDNAME_KEYWORD));
        else
            result = keywordInfo.getOrDefault(key, keywordInfo.get(Constants.COBOL_TOKEN));

        return result;
    }
}
