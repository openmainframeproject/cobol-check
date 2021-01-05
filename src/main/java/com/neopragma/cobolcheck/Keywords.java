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
public class Keywords implements Constants {
    private static final Messages messages = new Messages();
    private static final Map<String, Keyword> keywordInfo;

    static {
        keywordInfo = new HashMap<>();
        keywordInfo.put(TESTSUITE_KEYWORD,
                new Keyword(TESTSUITE_KEYWORD,
                        List.of(ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.TESTSUITE_NAME));
        keywordInfo.put(TESTCASE_KEYWORD,
                new Keyword(TESTCASE_KEYWORD,
                        List.of(ALPHANUMERIC_LITERAL_KEYWORD),
                        KeywordAction.NEW_TESTCASE));
        keywordInfo.put(EXPECT_KEYWORD,
                new Keyword(EXPECT_KEYWORD,
                        List.of(FIELDNAME_KEYWORD),
                        KeywordAction.ACTUAL_FIELDNAME));
        keywordInfo.put(FIELDNAME_KEYWORD,
                new Keyword(EMPTY_STRING,
                        List.of(TO_BE_KEYWORD,
                                NOT_KEYWORD,
                                COBOL_TOKEN),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(NOT_KEYWORD,
                new Keyword(NOT_KEYWORD,
                        List.of(TO_BE_KEYWORD),
                        KeywordAction.REVERSE_LOGIC));
        keywordInfo.put(TO_BE_KEYWORD,
                new Keyword(TO_BE_KEYWORD,
                        List.of(FIELDNAME_KEYWORD,
                                ALPHANUMERIC_LITERAL_KEYWORD,
                                NUMERIC_LITERAL_KEYWORD,
                                TRUE,
                                FALSE),
                        KeywordAction.EXPECTED_VALUE));
        keywordInfo.put(ALPHANUMERIC_LITERAL_KEYWORD,
                new Keyword(ALPHANUMERIC_LITERAL_KEYWORD,
                        List.of(EXPECT_KEYWORD, COBOL_TOKEN),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(NUMERIC_LITERAL_KEYWORD,
                new Keyword(EMPTY_STRING,
                        List.of(),
                        KeywordAction.FIELDNAME));
        keywordInfo.put(COBOL_TOKEN,
                new Keyword(COBOL_TOKEN,
                        List.of(COBOL_TOKEN,
                                ALPHANUMERIC_LITERAL_KEYWORD,
                                FIELDNAME_KEYWORD,
                                EXPECT_KEYWORD),
                        KeywordAction.COBOL_STATEMENT));
    }

    public static Keyword getKeywordFor(String key) {
        Keyword result = null;
        if (key != null && (key.startsWith("\"") || key.startsWith("'"))) {
            key = ALPHANUMERIC_LITERAL_KEYWORD;
        }
        result = keywordInfo.getOrDefault(key, keywordInfo.get(COBOL_TOKEN));
        return result;
    }
}
