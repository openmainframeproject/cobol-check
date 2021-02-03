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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This is a container for Keyword records. It is used when parsing test suites to identify cobol-check
 * keywords and handle them appropriately.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Keywords {
    private static final Messages messages = new Messages();
    private static final Map<String, Keyword> keywordInfo;

    static {
        keywordInfo = new HashMap<>();
        keywordInfo.put(Constants.TESTSUITE_KEYWORD,
                new Keyword(Constants.TESTSUITE_KEYWORD,
                        List.of(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.TESTSUITE_NAME));
        keywordInfo.put(Constants.TESTCASE_KEYWORD,
                new Keyword(Constants.TESTCASE_KEYWORD,
                        List.of(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NEW_TESTCASE));
        keywordInfo.put(Constants.EXPECT_KEYWORD,
                new Keyword(Constants.EXPECT_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD),
                        KeywordAction.ACTUAL_FIELDNAME));
        keywordInfo.put(Constants.FIELDNAME_KEYWORD,
                new Keyword(Constants.EMPTY_STRING,
                        List.of(Constants.TO_BE_KEYWORD,
                                Constants.NOT_KEYWORD,
                                Constants.COBOL_TOKEN),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(Constants.NOT_KEYWORD,
                new Keyword(Constants.NOT_KEYWORD,
                        List.of(Constants.TO_BE_KEYWORD),
                        KeywordAction.REVERSE_LOGIC));
        keywordInfo.put(Constants.NOT_EQUAL_SIGN_KEYWORD,
                new Keyword(Constants.NOT_EQUAL_SIGN_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD),
                        KeywordAction.REVERSE_LOGIC));
        keywordInfo.put(Constants.TO_BE_KEYWORD,
                new Keyword(Constants.TO_BE_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.TRUE,
                                Constants.FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.TO_EQUAL_KEYWORD,
                new Keyword(Constants.TO_EQUAL_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.TRUE,
                                Constants.FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.EQUAL_SIGN_KEYWORD,
                new Keyword(Constants.EQUAL_SIGN_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.TRUE,
                                Constants.FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.NOT_EQUAL_SIGN_KEYWORD,
                new Keyword(Constants.NOT_EQUAL_SIGN_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.TRUE,
                                Constants.FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.GREATER_THAN_SIGN_KEYWORD,
                new Keyword(Constants.GREATER_THAN_SIGN_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.TRUE,
                                Constants.FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.LESS_THAN_SIGN_KEYWORD,
                new Keyword(Constants.LESS_THAN_SIGN_KEYWORD,
                        List.of(Constants.FIELDNAME_KEYWORD,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.NUMERIC_LITERAL_KEYWORD,
                                Constants.TRUE,
                                Constants.FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                new Keyword(Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                        List.of(Constants.EXPECT_KEYWORD, Constants.COBOL_TOKEN),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(Constants.NUMERIC_LITERAL_KEYWORD,
                new Keyword(Constants.NUMERIC_LITERAL_KEYWORD,
                        List.of(Constants.EXPECT_KEYWORD, Constants.COBOL_TOKEN),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(Constants.COBOL_TOKEN,
                new Keyword(Constants.COBOL_TOKEN,
                        List.of(Constants.COBOL_TOKEN,
                                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                                Constants.FIELDNAME_KEYWORD,
                                Constants.EXPECT_KEYWORD),
                        KeywordAction.COBOL_STATEMENT));
        keywordInfo.put(Constants.BOOLEAN_VALUE,
                new Keyword(Constants.BOOLEAN_VALUE,
                        List.of(Constants.EXPECT_KEYWORD, Constants.COBOL_TOKEN),
                        KeywordAction.BOOLEAN_COMPARE));
    }

    public static Keyword getKeywordFor(String key) {
        Keyword result = null;
        if (key != null) {
            if (key.startsWith("\"") || key.startsWith("'")) {
                key = Constants.ALPHANUMERIC_LITERAL_KEYWORD;
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
                } else {
                    if (key.equals("TRUE") || key.equals("FALSE")) {
                        key = Constants.BOOLEAN_VALUE;
                    }
                }
            }
        }
        result = keywordInfo.getOrDefault(key, keywordInfo.get(Constants.COBOL_TOKEN));
        return result;
    }
}
