package org.openmainframeproject.cobolcheck.features.testSuiteParser;

/**
 * Special handling pertaining to a cobol-check keyword. Used when parsing a testsuite and inserting the
 * corresponding Cobol code into the program under test. KeywordAction is a member of record type Keyword.
 *
 * NONE - no special action is associated with this keyword
 * ACTUAL_FIELDNAME - the next token will be the fieldname of the actual (result) value for an EXPECT
 * COBOL_STATEMENT_TOKEN - pass through user-written Cobol statements for test case setup or mock behavior
 * EXPECTED_VALUE - the next token will be the expected value for an EXPECT
 * FIELDNAME - this token is the name of a field in the Data Division of the program under test
 * IGNORE - the next token will be TESTCASE - bypass that test case
 * NEW_TESTCASE - the next token will be an alphanumeric literal representing the name of the TESTCASE
 * REVERSE_LOGIC - the comparison logic for this EXPECT is to be reversed (NOT logic)
 * TESTSUITE_NAME - the next token will be an alphanumeric literal representing the name of the TESTSUITE
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public enum KeywordAction {
    NONE,
    ACTUAL_FIELDNAME,
    BOOLEAN_COMPARE,
    COBOL_STATEMENT,
    EXPECTED_VALUE,
    FIELDNAME,
    IGNORE,
    NEW_TESTCASE,
    REVERSE_LOGIC,
    TESTSUITE_NAME
}
