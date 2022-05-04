package org.openmainframeproject.cobolcheck.services;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.util.Arrays;
import java.util.List;

public final class Constants {

    private Constants() {
        throw new PossibleInternalLogicErrorException(
                Messages.get("ERR026"));
    }

    // File read status values
    public static final int STATUS_NORMAL = 0;
    public static final int STATUS_HALT = 8;

    public static final int COMMENT_SPACE_OFFSET = 7;

    public static final String COBOLCHECK_PACKAGE_PATH = "/org/openmainframeproject/cobolcheck";

    // Cobol Check copybook location (not for users)
    public static final String COBOLCHECK_COPYBOOK_DIRECTORY = COBOLCHECK_PACKAGE_PATH + "/copybooks/";

    //Command line options
    public static final String COMMAND_lINE_OPTIONS = "c:l:p:t:g:a:e:vh --long config-file:,log-level:,programs:,tests:,generated-tests:,all-tests:,error-log:,version,help";

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
    public static final String PARENTHESIS_ENCLOSED_KEYWORD = "parenthesis-enclosed";
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
    public static final String MOCK_KEYWORD = "MOCK";
    public static final String MOCK_TYPE = "mock-type";
    public static final String ENDMOCK_KEYWORD = "END-MOCK";
    public static final String VERIFY_KEYWORD = "VERIFY";
    public static final String NEVER_HAPPENED_KEYWORD = "NEVER HAPPENED";
    public static final String HAPPENED_KEYWORD = "HAPPENED";
    public static final String ONCE_KEYWORD = "ONCE";
    public static final String AT_LEAST_KEYWORD = "AT LEAST";
    public static final String NO_MORE_THAN_KEYWORD = "NO MORE THAN";
    public static final String TIME_KEYWORD = "TIME";
    public static final String TIMES_KEYWORD = "TIMES";
    public static final String BEFORE_EACH_TOKEN = "BEFORE EACH";
    public static final String END_BEFORE_TOKEN = "END-BEFORE";
    public static final String AFTER_EACH_TOKEN = "AFTER EACH";
    public static final String END_AFTER_TOKEN = "END-AFTER";
    //BACKWARDS COMPATIBILITY TODO: remove values?
    public static final String BEFORE_EACH_TOKEN_HYPHEN = "BEFORE-EACH";
    public static final String AFTER_EACH_TOKEN_HYPHEN = "AFTER-EACH";
    public static final String PARA_TOKEN = "PARA";
    public static final List<String> IGNORED_TOKENS = Arrays.asList("END-CALL", "END-MOCK");

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

    // Special values the Interpreter looks for in the source of the program under test
    public static final String IDENTIFICATION_DIVISION = "IDENTIFICATION DIVISION";
    public static final String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION";
    public static final String CONFIGURATION_SECTION = "CONFIGURATION SECTION";
    public static final String SPECIAL_NAMES_PARAGRAPH = "SPECIAL-NAMES";
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
    public static final String DECIMAL_POINT_KEYWORD = "DECIMAL-POINT";
    public static final String COMMA_KEYWORD = "COMMA";
    public static final String FD_TOKEN = "FD";
    public static final String LEVEL_01_TOKEN = "01";
    public static final String COPY_TOKEN = "COPY";
    public static final String SECTION_TOKEN = "SECTION";
    public static final String CALL_TOKEN = "CALL";
    public static final String USING_TOKEN = "USING";
    public static final String BY_REFERENCE_TOKEN = "BY REFERENCE";
    public static final String BY_CONTENT_TOKEN = "BY CONTENT";
    public static final String BY_VALUE_TOKEN = "BY VALUE";
    public static final String DECLARATIVES_TOKEN = "DECLARATIVES";
    public static final String EXIT_TOKEN = "EXIT";
    public static final String END_SECTION_TOKEN = "END-SECTION";
    public static final String END_PARAGRAPH_TOKEN = "END-PARAGRAPH";
    public static final String END_CALL_TOKEN = "END-CALL";
    public static final String ZERO_TOKEN = "ZERO";

    public static final String COMP_3_VALUE = "COMP-3";
    public static final String COMP_VALUE = "COMP";
    public static final String PIC_VALUE = "PIC";
    public static final String PICTURE_VALUE = "PICTURE";
    public static final String NUMERIC_PICTURE_CLAUSE_PATTERN = "^[\\d\\(\\)SsVv]+$";


    //Keywords not in COBOL-Code
    public static final String PARAGRAPH_TOKEN = "PARAGRAPH";
}
