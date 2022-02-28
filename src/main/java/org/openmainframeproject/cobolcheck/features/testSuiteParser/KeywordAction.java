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
