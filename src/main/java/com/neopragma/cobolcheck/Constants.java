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
    String EMPTY_STRING = "";
    int END_OF_STREAM = -1;
    int STATUS_NORMAL = 0;
    String NEWLINE = System.getProperty("line.separator");
    String FILE_SEPARATOR = System.getProperty("file.separator");
    String CURRENT_DIRECTORY = "./";
    String PERIOD = ".";
    String COMMENT_INDICATOR = "*";
    String COLON = ":";
    String SPACE = " ";
    String COMMA = ",";
    String QUOTE = "\"";
    String PSEUDO_TEXT_DELIMITER_EQUALS = "==";
    String PSEUDO_TEXT_DELIMITER_COLON = "::";
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
    String TEST_SUITE_DIRECTORY_CONFIG_KEY = "test.suite.directory";
}
