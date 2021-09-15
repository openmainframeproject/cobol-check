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

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

public final class Constants {

    Messages messages = new Messages();

    private Constants() {
        throw new PossibleInternalLogicErrorException(
                messages.get("ERR026"));
    }

    // File read status values
    public static final int STATUS_NORMAL = 0;
    public static final int STATUS_HALT = 8;

    // Cobol Check copybook location (not for users)
    public static final String COBOLCHECK_COPYBOOK_DIRECTORY = "/com/neopragma/cobolcheck/copybooks/";

    // Frequently-used string values
    public static final String EMPTY_STRING = "";
    public static final String NEWLINE = System.getProperty("line.separator");
    public static final String FILE_SEPARATOR = System.getProperty("file.separator");
    public static final String CURRENT_DIRECTORY = "./";
    public static final String PERIOD = ".";
    public static final String COMMENT_INDICATOR = "*";
    public static final String COLON = ":";
    public static final String SPACE = " ";
    public static final String COMMA = ",";
    public static final String QUOTE = "\"";
    public static final String APOSTROPHE = "'";

    // COPY REPLACING delimiters
    public static final String PSEUDO_TEXT_DELIMITER_EQUALS = "==";
    public static final String PSEUDO_TEXT_DELIMITER_COLON = "::";

    // Test suite keywords and values used in parsing test cases
    public static final String TRUE = "TRUE";
    public static final String FALSE = "FALSE";
    public static final String TESTSUITE_KEYWORD = "TESTSUITE";
    public static final String TESTCASE_KEYWORD = "TESTCASE";
    public static final String EXPECT_KEYWORD = "EXPECT";
    public static final String FIELDNAME_KEYWORD = "fieldname";
    public static final String TO_BE_KEYWORD = "TO BE";
    public static final String TO_EQUAL_KEYWORD = "TO EQUAL";
    public static final String NOT_KEYWORD = "NOT";
    public static final String ALPHANUMERIC_LITERAL_KEYWORD = "alphanumeric-literal";
    public static final String NUMERIC_LITERAL_KEYWORD = "numeric-literal";
    public static final String COBOL_TOKEN = "cobol-token";
    public static final String BOOLEAN_VALUE = "boolean-value";
    public static final String REPLACING_KEYWORD = "REPLACING";
    public static final String BY_KEYWORD = "BY";
    public static final String EQUAL_SIGN_KEYWORD = "=";
    public static final String NOT_EQUAL_SIGN_KEYWORD = "!=";
    public static final String GREATER_THAN_SIGN_KEYWORD = ">";
    public static final String LESS_THAN_SIGN_KEYWORD = "<";
    public static final String GREATER_THAN_EQUAL_TO_SIGN_KEYWORD = ">=";
    public static final String LESS_THAN_EQUAL_TO_SIGN_KEYWORD = "<=";

    // Configuration key values
    public static final String CONCATENATED_TEST_SUITES_CONFIG_KEY = "concatenated.test.suites";
    public static final String DEFAULT_CONCATENATED_TEST_SUITES_PATH = "./ALLTESTS";
    public static final String TEST_PROGRAM_NAME_CONFIG_KEY = "cobolcheck.test.program.name";
    public static final String DEFAULT_TEST_PROGRAM_NAME = "CC$$99.CBL";
    public static final String COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY = "cobolcheck.script.directory";
    public static final String DEFAULT_COBOLCHECK_SCRIPT_DIRECTORY = "./";
    public static final String PROCESS_CONFIG_KEY = ".process";
    public static final String COBOLCHECK_PREFIX_CONFIG_KEY = "cobolcheck.prefix";
    public static final String DEFAULT_COBOLCHECK_PREFIX = "UT-";
    public static final String TEST_CODE_PREFIX_PLACEHOLDER = "==UT==";

    // Command line option key values
    public static final String TESTS_OPTION = "tests";
    public static final String PROGRAMS_OPTION = "programs";

    // Special values the Generator looks for in the source of the program under test
    public static final String IDENTIFICATION_DIVISION = "IDENTIFICATION DIVISION";
    public static final String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION";
    public static final String CONFIGURATION_SECTION = "CONFIGURATION SECTION";
    public static final String INPUT_OUTPUT_SECTION = "INPUT-OUTPUT SECTION";
    public static final String FILE_CONTROL = "FILE-CONTROL";
    public static final String DATA_DIVISION = "DATA DIVISION";
    public static final String PROCEDURE_DIVISION = "PROCEDURE DIVISION";
    public static final String FILE_SECTION = "FILE SECTION";
    public static final String LOCAL_STORAGE_SECTION = "LOCAL-STORAGE SECTION";
    public static final String LINKAGE_SECTION = "LINKAGE SECTION";
    public static final String WORKING_STORAGE_SECTION = "WORKING-STORAGE SECTION";
    public static final String SELECT_TOKEN = "SELECT";
    public static final String FILE_STATUS_TOKEN = "FILE STATUS";
    public static final String IS_TOKEN = "IS";
    public static final String FD_TOKEN = "FD";
    public static final String LEVEL_01_TOKEN = "01";
    public static final String COPY_TOKEN = "COPY";
    public static final String SECTION_TOKEN = "SECTION";
    public static final String DECLARATIVES_TOKEN = "DECLARATIVES";

    //Keywords not in COBOL-Code
    public static final String PARAGRAPH_TOKEN = "PARAGRAPH";
}
