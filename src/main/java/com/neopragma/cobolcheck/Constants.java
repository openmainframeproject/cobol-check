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

public interface Constants {

    // File read status values
    int END_OF_STREAM = -1;
    int STATUS_NORMAL = 0;

    // Frequently-used string values
    String EMPTY_STRING = "";
    String NEWLINE = System.getProperty("line.separator");
    String FILE_SEPARATOR = System.getProperty("file.separator");
    String CURRENT_DIRECTORY = "./";
    String PERIOD = ".";
    String COMMENT_INDICATOR = "*";
    String COLON = ":";
    String SPACE = " ";
    String COMMA = ",";
    String QUOTE = "\"";
    String APOSTROPHE = "'";

    // COPY REPLACING delimiters
    String PSEUDO_TEXT_DELIMITER_EQUALS = "==";
    String PSEUDO_TEXT_DELIMITER_COLON = "::";

    // Test suite keywords and values used in parsing test cases
    String TRUE = "TRUE";
    String FALSE = "FALSE";
    String TESTSUITE_KEYWORD = "TESTSUITE";
    String TESTCASE_KEYWORD = "TESTCASE";
    String EXPECT_KEYWORD = "EXPECT";
    String FIELDNAME_KEYWORD = "fieldname";
    String TO_BE_KEYWORD = "TO BE";
    String NOT_KEYWORD = "NOT";
    String ALPHANUMERIC_LITERAL_KEYWORD = "alphanumeric-literal";
    String NUMERIC_LITERAL_KEYWORD = "numeric-literal";
    String COBOL_TOKEN = "cobol-token";
    String BOOLEAN_VALUE = "boolean-value";
    String REPLACING_KEYWORD = "REPLACING";
    String BY_KEYWORD = "BY";

    // Configuration key values
    String TEST_SUITE_DIRECTORY_CONFIG_KEY = "test.suite.directory";
    String CONCATENATED_TEST_SUITES_CONFIG_KEY = "concatenated.test.suites";
    String DEFAULT_CONCATENATED_TEST_SUITES_PATH = "./ALLTESTS";
    String APPLICATION_SOURCE_DIRECTORY_CONFIG_KEY = "application.source.directory";
    String DEFAULT_APPLICATION_SOURCE_DIRECTORY = "src/main/cobol";
    String APPLICATION_COPYBOOK_FILENAME_SUFFIX_KEY = "application.copybook.filename.suffix";
    String DEFAULT_APPLICATION_COPYBOOK_FILENAME_SUFFIX = ".CBL";
    String TEST_PROGRAM_SUFFIX_CONFIG_KEY = "cobolcheck.test.program.suffix";
    String DEFAULT_TEST_PROGRAM_SUFFIX = "T.CBL";
    String COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY = "cobolcheck.script.directory";
    String DEFAULT_COBOLCHECK_SCRIPT_DIRECTORY = "./";
    String PROCESS_CONFIG_KEY = ".process";
    String COBOLCHECK_PREFIX_CONFIG_KEY = "cobolcheck.prefix";
    String DEFAULT_COBOLCHECK_PREFIX = "UT-";
    String TEST_CODE_PREFIX_PLACEHOLDER = "==UT==";
    String RESOURCES_DIRECTORY_CONFIG_KEY = "resources.directory";
    String COBOLCHECK_COPYBOOK_DIRECTORY_CONFIG_KEY = "cobolcheck.copybook.directory";

    // Command line option key values
    String TESTS_OPTION = "tests";
    String PROGRAMS_OPTION = "programs";

    // Special values the Generator looks for in the source of the program under test
    String IDENTIFICATION_DIVISION = "IDENTIFICATION DIVISION";
    String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION";
    String CONFIGURATION_SECTION = "CONFIGURATION SECTION";
    String INPUT_OUTPUT_SECTION = "INPUT-OUTPUT SECTION";
    String FILE_CONTROL = "FILE-CONTROL";
    String DATA_DIVISION = "DATA DIVISION";
    String PROCEDURE_DIVISION = "PROCEDURE DIVISION";
    String FILE_SECTION = "FILE SECTION";
    String LOCAL_STORAGE_SECTION = "LOCAL-STORAGE SECTION";
    String LINKAGE_SECTION = "LINKAGE SECTION";
    String WORKING_STORAGE_SECTION = "WORKING-STORAGE SECTION";
    String SELECT_TOKEN = "SELECT";
    String FILE_STATUS_TOKEN = "FILE STATUS";
    String IS_TOKEN = "IS";
    String FD_TOKEN = "FD";
    String LEVEL_01_TOKEN = "01";
    String COPY_TOKEN = "COPY";

}
