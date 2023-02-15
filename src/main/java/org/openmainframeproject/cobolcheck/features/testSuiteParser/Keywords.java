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
    private static final List<String> qualifiedNameKeywords;

    static {
        keywordInfo = new HashMap<>();
        //--------------------------------TESTSUITE
        keywordInfo.put(
            Constants.TESTSUITE_KEYWORD,
            new Keyword(
                Constants.TESTSUITE_KEYWORD,
                Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                KeywordAction.TESTSUITE_NAME
            )
        );

        //--------------------------------TESTCASE
        keywordInfo.put(
            Constants.TESTCASE_KEYWORD,
            new Keyword(Constants.TESTCASE_KEYWORD,
                Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD),
                KeywordAction.NEW_TESTCASE
            )
        );

        //--------------------------------EXPECT
        keywordInfo.put(Constants.EXPECT_KEYWORD,
            new Keyword(Constants.EXPECT_KEYWORD,
                Arrays.asList(Constants.FIELDNAME_KEYWORD),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                }},
                KeywordAction.ACTUAL_FIELDNAME
            )
        );

        //--------------------------------FIELD NAME
        keywordInfo.put(
            Constants.FIELDNAME_KEYWORD,
            new Keyword(
                Constants.FIELDNAME_KEYWORD,
                Arrays.asList(
                    Constants.NOT_KEYWORD,
                    Constants.EQUAL_SIGN_KEYWORD,
                    Constants.GREATER_THAN_SIGN_KEYWORD,
                    Constants.LESS_THAN_SIGN_KEYWORD,
                    Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                    Constants.FIELDNAME_KEYWORD,
                    Constants.BY_REFERENCE_TOKEN,
                    Constants.BY_CONTENT_TOKEN,
                    Constants.BY_VALUE_TOKEN,
                    Constants.USING_TOKEN,
                    Constants.QUALIFIED_FIELD_NAME
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD,
                        Arrays.asList(
                            Constants.ENDMOCK_KEYWORD,
                            Constants.FIELDNAME_KEYWORD,
                            Constants.BY_REFERENCE_TOKEN,
                            Constants.BY_CONTENT_TOKEN,
                            Constants.BY_VALUE_TOKEN,
                            Constants.USING_TOKEN
                        )
                    );
                    put(
                        Constants.EXPECT_KEYWORD, 
                        Arrays.asList(
                            Constants.TO_BE_KEYWORD,
                            Constants.EQUAL_SIGN_KEYWORD,
                            Constants.TO_EQUAL_KEYWORD,
                            Constants.NOT_KEYWORD,
                            Constants.LESS_THAN_SIGN_KEYWORD,
                            Constants.NOT_EQUAL_SIGN_KEYWORD,
                            Constants.LESS_THAN_SIGN_KEYWORD,
                            Constants.EQUAL_SIGN_KEYWORD,
                            Constants.GREATER_THAN_SIGN_KEYWORD,
                            Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                            Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.FIELDNAME_KEYWORD,
                            Constants.QUALIFIED_FIELD_NAME,
                            Constants.PARENTHESIS_ENCLOSED_KEYWORD
                        )
                    );
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.FIELDNAME_KEYWORD,
                            Constants.BY_REFERENCE_TOKEN,
                            Constants.BY_CONTENT_TOKEN,
                            Constants.BY_VALUE_TOKEN,
                            Constants.USING_TOKEN,
                            Constants.HAPPENED_KEYWORD,
                            Constants.NEVER_HAPPENED_KEYWORD
                        )
                    );
                }},
                KeywordAction.FIELDNAME
            )
        );

        //--------------------------------NOT
        keywordInfo.put(Constants.NOT_KEYWORD,
            new Keyword(
                Constants.NOT_KEYWORD,
                Arrays.asList(
                    Constants.EQUAL_SIGN_KEYWORD,
                    Constants.GREATER_THAN_SIGN_KEYWORD,
                    Constants.LESS_THAN_SIGN_KEYWORD,
                    Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD
                        Arrays.asList(
                            Constants.TO_BE_KEYWORD,
                            Constants.TO_EQUAL_KEYWORD,
                            Constants.EQUAL_SIGN_KEYWORD,
                            Constants.NOT_EQUAL_SIGN_KEYWORD,
                            Constants.GREATER_THAN_SIGN_KEYWORD,
                            Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                            Constants.LESS_THAN_SIGN_KEYWORD,
                            Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD
                        )
                    );
                }},
                KeywordAction.REVERSE_LOGIC
            )
        );

        //--------------------------------NOT EQUAL SIGN
        keywordInfo.put(
            Constants.NOT_EQUAL_SIGN_KEYWORD,
            new Keyword(Constants.NOT_EQUAL_SIGN_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.REVERSE_LOGIC
            )
        );

        //--------------------------------TO BE
        keywordInfo.put(
            Constants.TO_BE_KEYWORD,
            new Keyword(
                Constants.TO_BE_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------TO EQUAL
        keywordInfo.put(
            Constants.TO_EQUAL_KEYWORD,
            new Keyword(
                Constants.TO_EQUAL_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------EQUAL SIGN
        keywordInfo.put(
            Constants.EQUAL_SIGN_KEYWORD,
            new Keyword(Constants.EQUAL_SIGN_KEYWORD,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.NUMERIC_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Constants.NUMERIC_LITERAL_KEYWORD,
                    Constants.BOOLEAN_VALUE),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------GREATER THAN SIGN
        keywordInfo.put(
            Constants.GREATER_THAN_SIGN_KEYWORD,
            new Keyword(
                Constants.GREATER_THAN_SIGN_KEYWORD,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.NUMERIC_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Constants.NUMERIC_LITERAL_KEYWORD,
                    Constants.BOOLEAN_VALUE
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------GREATER THAN EQUAL TO SIGN
        keywordInfo.put(
            Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
            new Keyword(
                Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.NUMERIC_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Constants.NUMERIC_LITERAL_KEYWORD,
                    Constants.BOOLEAN_VALUE
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD, 
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------LESS THAN SIGN
        keywordInfo.put(
            Constants.LESS_THAN_SIGN_KEYWORD,
            new Keyword(
                Constants.LESS_THAN_SIGN_KEYWORD,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.NUMERIC_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Constants.NUMERIC_LITERAL_KEYWORD,
                    Constants.BOOLEAN_VALUE
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD, 
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------LESS THAN EQUAL TO SIGN
        keywordInfo.put(
            Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
            new Keyword(
                Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.NUMERIC_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Constants.NUMERIC_LITERAL_KEYWORD,
                    Constants.BOOLEAN_VALUE
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------NUMERIC
        keywordInfo.put(
            Constants.NUMERIC_KEYWORD,
            new Keyword(Constants.NUMERIC_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD, 
                        Arrays.asList(
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.BOOLEAN_VALUE,
                            Constants.COBOL_TOKEN
                        )
                    );
                }},
                KeywordAction.EXPECTED_VALUE
            )
        );

        //--------------------------------PARENTHESIS-ENCLOSED
        keywordInfo.put(
            Constants.PARENTHESIS_ENCLOSED_KEYWORD,
            new Keyword(
                Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                Arrays.asList(
                    Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                    Constants.NOT_KEYWORD,
                    Constants.EQUAL_SIGN_KEYWORD,
                    Constants.GREATER_THAN_SIGN_KEYWORD,
                    Constants.LESS_THAN_SIGN_KEYWORD,
                    Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.EXPECT_KEYWORD
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.TO_BE_KEYWORD,
                            Constants.EQUAL_SIGN_KEYWORD,
                            Constants.TO_EQUAL_KEYWORD,
                            Constants.NOT_KEYWORD,
                            Constants.LESS_THAN_SIGN_KEYWORD,
                            Constants.NOT_EQUAL_SIGN_KEYWORD,
                            Constants.LESS_THAN_SIGN_KEYWORD,
                            Constants.EQUAL_SIGN_KEYWORD,
                            Constants.GREATER_THAN_SIGN_KEYWORD,
                            Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                            Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                            Constants.FIELDNAME_KEYWORD,
                            Constants.QUALIFIED_FIELD_NAME,
                            Constants.PARENTHESIS_ENCLOSED_KEYWORD
                        )
                    );
                }},
                KeywordAction.COBOL_STATEMENT
            )
        );

        //--------------------------------ALPHANUMERIC LITERAL
        keywordInfo.put(
            Constants.ALPHANUMERIC_LITERAL_KEYWORD,
            new Keyword(
                Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Arrays.asList(
                        Constants.ALPHANUMERIC_LITERAL_KEYWORD,
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
                        Constants.FIELDNAME_KEYWORD,
                        Constants.BY_REFERENCE_TOKEN,
                        Constants.BY_CONTENT_TOKEN,
                        Constants.BY_VALUE_TOKEN,
                        Constants.USING_TOKEN
                    ),
                    new HashMap<String, List<String>>() {{
                        put(
                            Constants.MOCK_KEYWORD,
                            Arrays.asList(
                                Constants.ENDMOCK_KEYWORD,
                                Constants.FIELDNAME_KEYWORD,
                                Constants.BY_REFERENCE_TOKEN,
                                Constants.BY_CONTENT_TOKEN,
                                Constants.BY_VALUE_TOKEN,
                                Constants.USING_TOKEN
                            )
                        );
                        put(
                            Constants.VERIFY_KEYWORD,
                            Arrays.asList(
                                Constants.USING_TOKEN,
                                Constants.HAPPENED_KEYWORD,
                                Constants.NEVER_HAPPENED_KEYWORD
                            )
                        );
                    }},
                KeywordAction.FIELDNAME
            )
        );

        //--------------------------------NUMERIC LITERAL
        keywordInfo.put(
            Constants.NUMERIC_LITERAL_KEYWORD,
            new Keyword(
                Constants.NUMERIC_LITERAL_KEYWORD,
                Arrays.asList(
                    Constants.EXPECT_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.TIMES_KEYWORD
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.TIMES_KEYWORD,
                            Constants.TIME_KEYWORD
                        )
                    );
                }},
                KeywordAction.FIELDNAME
            )
        );

        //--------------------------------COBOL TOKEN
        keywordInfo.put(
            Constants.COBOL_TOKEN,
            new Keyword(
                Constants.COBOL_TOKEN,
                Arrays.asList(
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD,
                    Constants.NUMERIC_LITERAL_KEYWORD,
                    Constants.BOOLEAN_VALUE,
                    Constants.FIELDNAME_KEYWORD,
                    Constants.EXPECT_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.PARENTHESIS_ENCLOSED_KEYWORD,
                    Constants.GREATER_THAN_SIGN_KEYWORD,
                    Constants.LESS_THAN_SIGN_KEYWORD,
                    Constants.NOT_KEYWORD,
                    Constants.EQUAL_SIGN_KEYWORD,
                    Constants.GREATER_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.LESS_THAN_EQUAL_TO_SIGN_KEYWORD,
                    Constants.TIME_KEYWORD,
                    Constants.TIMES_KEYWORD,
                    Constants.QUALIFIED_FIELD_NAME
                ),
                KeywordAction.COBOL_STATEMENT
            )
        );

        //--------------------------------BOOLEAN VALUE
        keywordInfo.put(
            Constants.BOOLEAN_VALUE,
            new Keyword(
                Constants.BOOLEAN_VALUE,
                Arrays.asList(
                    Constants.EXPECT_KEYWORD,
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD
                ),
                KeywordAction.BOOLEAN_COMPARE
            )
        );

        //--------------------------------BEFORE EACH
        keywordInfo.put(
            Constants.BEFORE_EACH_TOKEN,
            new Keyword(
                Constants.BEFORE_EACH_TOKEN,
                Arrays.asList(
                    Constants.END_BEFORE_TOKEN
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------END BEFORE
        keywordInfo.put(
            Constants.END_BEFORE_TOKEN,
            new Keyword(
                Constants.END_BEFORE_TOKEN,
                Arrays.asList(
                    Constants.AFTER_EACH_TOKEN,
                    Constants.AFTER_EACH_TOKEN_HYPHEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------AFTER EACH
        keywordInfo.put(
            Constants.AFTER_EACH_TOKEN,
            new Keyword(
                Constants.AFTER_EACH_TOKEN,
                Arrays.asList(Constants.END_AFTER_TOKEN),
                KeywordAction.NONE
            )
        );

        //--------------------------------END AFTER
        keywordInfo.put(
            Constants.END_AFTER_TOKEN,
            new Keyword(
                Constants.END_AFTER_TOKEN,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.BEFORE_EACH_TOKEN,
                    Constants.BEFORE_EACH_TOKEN_HYPHEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------QUALIFIED FIELD NAME
        keywordInfo.put(
            Constants.QUALIFIED_FIELD_NAME,
            new Keyword(
                Constants.QUALIFIED_FIELD_NAME,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.FIELDNAME_KEYWORD
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.EXPECT_KEYWORD,
                        Arrays.asList(
                            Constants.FIELDNAME_KEYWORD
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //TODO: Remove hyphen keyword option? Backwards compatibility
        //--------------------------------BEFORE-EACH
        keywordInfo.put(
            Constants.BEFORE_EACH_TOKEN_HYPHEN,
            new Keyword(
                Constants.BEFORE_EACH_TOKEN_HYPHEN,
                Arrays.asList(Constants.END_BEFORE_TOKEN),
                KeywordAction.NONE
            )
        );

        //--------------------------------AFTER-EACH
        keywordInfo.put(
            Constants.AFTER_EACH_TOKEN_HYPHEN,
            new Keyword(
                Constants.AFTER_EACH_TOKEN_HYPHEN,
                Arrays.asList(Constants.END_AFTER_TOKEN),
                KeywordAction.NONE
            )
        );

        //--------------------------------MOCK
        keywordInfo.put(
            Constants.MOCK_KEYWORD,
            new Keyword(
                Constants.MOCK_KEYWORD,
                new ArrayList<>(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD,
                        Arrays.asList(
                            Constants.MOCK_TYPE
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------END MOCK
        keywordInfo.put(
            Constants.ENDMOCK_KEYWORD,
            new Keyword(
                Constants.ENDMOCK_KEYWORD,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.BEFORE_EACH_TOKEN,
                    Constants.BEFORE_EACH_TOKEN_HYPHEN,
                    Constants.AFTER_EACH_TOKEN,
                    Constants.AFTER_EACH_TOKEN_HYPHEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.EXPECT_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------MOCK TYPE
        keywordInfo.put(
            Constants.MOCK_TYPE,
            new Keyword(
                Constants.MOCK_TYPE,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.ALPHANUMERIC_LITERAL_KEYWORD
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD, Arrays.asList(
                            Constants.FIELDNAME_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD
                        )
                    );
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.FIELDNAME_KEYWORD,
                            Constants.ALPHANUMERIC_LITERAL_KEYWORD
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------USING
        keywordInfo.put(
            Constants.USING_TOKEN,
            new Keyword(
                Constants.USING_TOKEN,
                Arrays.asList(
                    Constants.FIELDNAME_KEYWORD,
                    Constants.BY_REFERENCE_TOKEN,
                    Constants.BY_CONTENT_TOKEN,
                    Constants.BY_VALUE_TOKEN
                ),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD,
                        Arrays.asList(
                            Constants.FIELDNAME_KEYWORD,
                            Constants.BY_REFERENCE_TOKEN,
                            Constants.BY_CONTENT_TOKEN,
                            Constants.BY_VALUE_TOKEN
                        )
                    );
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.FIELDNAME_KEYWORD,
                            Constants.BY_REFERENCE_TOKEN,
                            Constants.BY_CONTENT_TOKEN,
                            Constants.BY_VALUE_TOKEN
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------BY REFERENCE
        keywordInfo.put(
            Constants.BY_REFERENCE_TOKEN,
            new Keyword(
                Constants.BY_REFERENCE_TOKEN,
                Arrays.asList(Constants.FIELDNAME_KEYWORD),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------BY CONTENT
        keywordInfo.put(
            Constants.BY_CONTENT_TOKEN,
            new Keyword(
                Constants.BY_CONTENT_TOKEN,
                Arrays.asList(Constants.FIELDNAME_KEYWORD),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------BY VALUE
        keywordInfo.put(
            Constants.BY_VALUE_TOKEN,
            new Keyword(
                Constants.BY_VALUE_TOKEN,
                Arrays.asList(Constants.FIELDNAME_KEYWORD),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.MOCK_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(Constants.FIELDNAME_KEYWORD)
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------VERIFY
        keywordInfo.put(
            Constants.VERIFY_KEYWORD,
            new Keyword(
                Constants.VERIFY_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(Constants.MOCK_TYPE)
                    );
                }},
                KeywordAction.ACTUAL_FIELDNAME
            )
        );

        //--------------------------------NEVER HAPPENED
        keywordInfo.put(
            Constants.NEVER_HAPPENED_KEYWORD,
            new Keyword(
                Constants.NEVER_HAPPENED_KEYWORD,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.EXPECT_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------HAPPENED
        keywordInfo.put(
            Constants.HAPPENED_KEYWORD,
            new Keyword(
                Constants.HAPPENED_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.ONCE_KEYWORD,
                            Constants.ZERO_TOKEN,
                            Constants.AT_LEAST_KEYWORD,
                            Constants.NO_MORE_THAN_KEYWORD
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------ONCE
        keywordInfo.put(
            Constants.ONCE_KEYWORD,
            new Keyword(
                Constants.ONCE_KEYWORD,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.EXPECT_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------AT LEAST
        keywordInfo.put(
            Constants.AT_LEAST_KEYWORD,
            new Keyword(
                Constants.AT_LEAST_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.ONCE_KEYWORD
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------NO MORE THAN
        keywordInfo.put(
            Constants.NO_MORE_THAN_KEYWORD,
            new Keyword(
                Constants.NO_MORE_THAN_KEYWORD,
                Arrays.asList(),
                new HashMap<String, List<String>>() {{
                    put(
                        Constants.VERIFY_KEYWORD,
                        Arrays.asList(
                            Constants.NUMERIC_LITERAL_KEYWORD,
                            Constants.ONCE_KEYWORD
                        )
                    );
                }},
                KeywordAction.NONE
            )
        );

        //--------------------------------TIME
        keywordInfo.put(
            Constants.TIME_KEYWORD,
            new Keyword(
                Constants.TIME_KEYWORD,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.EXPECT_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        //--------------------------------TIMES
        keywordInfo.put(
            Constants.TIMES_KEYWORD,
            new Keyword(
                Constants.TIMES_KEYWORD,
                Arrays.asList(
                    Constants.COBOL_TOKEN,
                    Constants.TESTSUITE_KEYWORD,
                    Constants.TESTCASE_KEYWORD,
                    Constants.MOCK_KEYWORD,
                    Constants.VERIFY_KEYWORD,
                    Constants.EXPECT_KEYWORD
                ),
                KeywordAction.NONE
            )
        );

        mockTypes = Arrays.asList(Constants.SECTION_TOKEN, Constants.PARAGRAPH_TOKEN, Constants.PARA_TOKEN, Constants.CALL_TOKEN);
        qualifiedNameKeywords = Arrays.asList(Constants.IN_KEYWORD, Constants.OF_KEYWORD);
    }


    public static Keyword getKeywordFor(String key, boolean expectFieldName) {
        Keyword result = null;
        if (key != null && !key.isEmpty()) {
            Log.debug("Key: " + key);
            if (key.startsWith("\"") || key.startsWith("'")) {
                key = Constants.ALPHANUMERIC_LITERAL_KEYWORD;
            }
            if (key.startsWith("(")) {
                key = Constants.PARENTHESIS_ENCLOSED_KEYWORD;
            } else {
                if (Character.isDigit(key.charAt(0)) || key.charAt(0) == '-') {
                    boolean isNumeric = true;
                    for (char digit : key.toCharArray()) {
                        if (!Character.isDigit(digit) &&
                                digit != '.' &&
                                digit != ',' &&
                                digit != '-') {
                            isNumeric = false;
                            break;
                        }
                    }
                    if (isNumeric) {
                        key = Constants.NUMERIC_LITERAL_KEYWORD;
                    }
                } else if (key.equals(Constants.ZERO_TOKEN)) {
                    key = Constants.NUMERIC_LITERAL_KEYWORD;
                } else {
                    if (key.equals("TRUE") || key.equals("FALSE")) {
                        key = Constants.BOOLEAN_VALUE;
                    }
                }
                if (mockTypes.contains(key)) {
                    key = Constants.MOCK_TYPE;
                }
                if (qualifiedNameKeywords.contains(key)){
                    key = Constants.QUALIFIED_FIELD_NAME;
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
