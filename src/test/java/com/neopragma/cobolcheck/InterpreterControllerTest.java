package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.interpreter.*;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class InterpreterControllerTest {
    InterpreterController interpreterController;
    BufferedReader mockedReader;

    @BeforeAll
    public static void setup(){
        Config.load();
    }

    @BeforeEach
    public void commonSetup(){
        mockedReader = Mockito.mock(BufferedReader.class);
        interpreterController = new InterpreterController(mockedReader);
    }

    @Test
    void cobol_source_cannot_be_null__probable_internal_logic_error() {
        assertThrows(PossibleInternalLogicErrorException.class, () -> new InterpreterController(null));
    }

    @Test
    void cobol_source_cannot_be_empty__probable_internal_logic_error() throws IOException {
        Mockito.when(mockedReader.readLine()).thenReturn(null);

        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                interpreterController.interpretNextLine());
        String message = ex.getMessage();
        assertTrue(message.contains("empty input stream"));
    }

    @Test
    public void it_sets_correct_flag_for_working_storage_section() throws IOException {
        String line = "       WORKING-STORAGE SECTION.";
        Mockito.when(mockedReader.readLine()).thenReturn(line);

        interpreterController.interpretNextLine();

        assertTrue(interpreterController.isReading(Constants.WORKING_STORAGE_SECTION));
    }

    @Test
    public void it_unsets_correct_flags_when_mutual_exclusive_flag_is_set() throws IOException {
        String str1 = "       DATA DIVISION.";
        String str2 = "       FILE SECTION.";
        String str3 = "       FD  INPUT-FILE";
        String str4 = "       01  INPUT-RECORD.";
        String str5 = "       WORKING-STORAGE SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.isReading(Constants.DATA_DIVISION));
        assertFalse(interpreterController.isReading(Constants.FILE_SECTION));
        assertFalse(interpreterController.isReading(Constants.FD_TOKEN));
        assertFalse(interpreterController.isReading(Constants.LEVEL_01_TOKEN));
        assertTrue(interpreterController.isReading(Constants.WORKING_STORAGE_SECTION));
    }

    @Test
    public void it_sets_decimal_point_is_comma() throws IOException {
        String str1 = "       ENVIRONMENT DIVISION. ";
        String str2 = "       CONFIGURATION SECTION.";
        String str3 = "       SPECIAL-NAMES.";
        String str4 = "           DECIMAL-POINT IS COMMA.";
        String str5 = "       INPUT-OUTPUT SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(Config.isDecimalPointComma());
        assertFalse(interpreterController.isReading(Constants.SPECIAL_NAMES_PARAGRAPH));
    }


    @Test
    public void it_recognizes_paragraph_header_format() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "       5400-WRITE-OUTPUT-RECORD.";
        String str3 = "           WRITE OUTPUT-RECORD";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.isReading(Constants.PARAGRAPH_TOKEN));
    }

    @Test
    public void it_recognizes_paragraph_header_format_with_extra_spaces() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "       5400-WRITE-OUTPUT-RECORD        .      ";
        String str3 = "           WRITE OUTPUT-RECORD";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.isReading(Constants.PARAGRAPH_TOKEN));
    }

    @Test
    public void it_recognizes_paragraph_header_format_with_period_on_next_line() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "       5400-WRITE-OUTPUT-RECORD";
        String str3 = "       .";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.isReading(Constants.PARAGRAPH_TOKEN));
    }

    @Test
    public void it_returns_false_if_not_a_paragraph_header_format() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "\"       000-START SECTION.\"";
        String str3 = "           PERFORM WITH TEST BEFORE";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertFalse(interpreterController.isReading(Constants.PARAGRAPH_TOKEN));
    }

    @Test
    public void it_returns_false_if_not_in_correct_area() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "           5400-WRITE-OUTPUT-RECORD.";
        String str3 = "           WRITE OUTPUT-RECORD";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertFalse(interpreterController.isReading(Constants.PARAGRAPH_TOKEN));
    }

    @Test
    public void it_sets_possible_mock_identifier_to_section_name() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "       5000-PROCESS SECTION.";
        String actual = "";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            actual = interpreterController.getPossibleMockIdentifier();
        }

        assertEquals("5000-PROCESS", actual);
    }

    @Test
    public void it_sets_possible_mock_identifier_to_call_program_name() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "           CALL 'PROG1'";
        String actual = "";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            actual = interpreterController.getPossibleMockIdentifier();
        }

        assertEquals("'PROG1'", actual);
    }

    @Test
    public void it_sets_possible_mock_args() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "           CALL 'PROG1' USING VALUE-1";
        List<String> actual = new ArrayList<>();

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            actual = interpreterController.getPossibleMockArgs();
        }

        assertEquals("REFERENCE VALUE-1", actual.get(0));
    }

    @Test
    public void it_sets_possible_mock_args_with_content_reference() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "           CALL 'PROG3' USING  BY CONTENT VALUE-1, BY REFERENCE VALUE-2";
        List<String> actual = new ArrayList<>();

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            actual = interpreterController.getPossibleMockArgs();
        }

        assertEquals("CONTENT VALUE-1", actual.get(0));
        assertEquals("REFERENCE VALUE-2", actual.get(1));
    }

    @Test
    public void it_sets_possible_mock_args_with_content_reference_multiline() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "           CALL 'PROG3' USING";
        String str3 = "             BY CONTENT VALUE-1,";
        String str4 = "             BY VALUE VALUE-2,";
        String str5 = "             VALUE-3";
        List<String> actual = new ArrayList<>();

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            actual = interpreterController.getPossibleMockArgs();
        }

        assertEquals("CONTENT VALUE-1", actual.get(0));
        assertEquals("VALUE VALUE-2", actual.get(1));
        assertEquals("REFERENCE VALUE-3", actual.get(2));
    }

    @Test
    public void it_ignores_lines_that_does_not_change_flags() throws IOException {

        boolean hasLine1Changed = false;
        boolean hasLine2Changed = false;
        boolean hasLine3Changed = false;

        String str1 = "                   PERFORM 9999-ABORT";
        String str2 = "           MOVE ZERO TO WS-COUNT";
        String str3 = "           IF MESSAGE-IS-FAREWELL";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3);

        interpreterController.interpretNextLine();
        hasLine1Changed = interpreterController.hasReaderStateChanged();
        interpreterController.interpretNextLine();
        hasLine2Changed = interpreterController.hasReaderStateChanged();
        interpreterController.interpretNextLine();
        hasLine3Changed = interpreterController.hasReaderStateChanged();

        assertFalse(hasLine1Changed);
        assertFalse(hasLine2Changed);
        assertFalse(hasLine3Changed);
    }

    @Test
    public void it_adds_file_control_statements_in_repo() throws IOException {

        String str1 = "       FILE-CONTROL.";
        String str2 = "           SELECT INPUT-FILE ASSIGN TO \"INFILE\"";
        String str3 = "               ORGANIZATION SEQUENTIAL";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.getFileControlStatements().contains(str2));
        assertTrue(interpreterController.getFileControlStatements().contains(str3));
    }

    @Test
    public void it_throws_when_token_list_has_fewer_than_2_entries() throws IOException {
        String str1 = "       FILE SECTION.";
        String str2 = "       FD  OUTPUT-FILE";
        String str3 = "           COPY.";
        String str4 = "       WORKING-STORAGE SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, null);
        interpreterController.interpretNextLine();
        interpreterController.interpretNextLine();

        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                interpreterController.interpretNextLine());
        assertTrue(ex.getMessage().contains("ERR024:"));
    }

    @Test
    public void it_adds_copy_tokens_in_repo_one_line() throws IOException {
        String str1 = "       FILE SECTION.";
        String str2 = "       FD  OUTPUT-FILE";
        String str3 = "           COPY OUTREC.";
        String str4 = "       WORKING-STORAGE SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.getCopyTokens().contains("COPY"));
        assertTrue(interpreterController.getCopyTokens().contains("OUTREC."));
    }

    @Test
    public void it_adds_copy_tokens_in_repo_multi_line() throws IOException {
        String str1 = "       FILE SECTION.";
        String str2 = "       FD  OUTPUT-FILE";
        String str3 = "           COPY";
        String str4 = "                         OUTREC";
        String str5 = "                         REPLACE";
        String str6 = "                         FILLER BY TEMP.";
        String str7 = "       WORKING-STORAGE SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.getCopyTokens().contains("COPY"));
        assertTrue(interpreterController.getCopyTokens().contains("OUTREC"));
        assertTrue(interpreterController.getCopyTokens().contains("REPLACE"));
        assertTrue(interpreterController.getCopyTokens().contains("FILLER"));
        assertTrue(interpreterController.getCopyTokens().contains("BY"));
        assertTrue(interpreterController.getCopyTokens().contains("TEMP."));
    }

    @Test
    public void it_ignores_comments_and_empty_lines_while_finding_copy_tokens() throws IOException {
        String str1 = "       FILE SECTION.";
        String str2 = "       FD  OUTPUT-FILE";
        String str3 = "           COPY";
        String str4 = "                ";
        String str5 = "      * This line is ignored!";
        String str6 = "                         OUTREC.";
        String str7 = "       WORKING-STORAGE SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertEquals(2,interpreterController.getCopyTokens().size());
        assertTrue(interpreterController.getCopyTokens().contains("COPY"));
        assertTrue(interpreterController.getCopyTokens().contains("OUTREC."));
    }

    @Test
    public void it_adds_file_section_statements() throws IOException {
        String str1 = "       FILE SECTION.";
        String str2 = "       FD  INPUT-FILE";
        String str3 = "       01  INPUT-RECORD.";
        String str4 = "           05  IN-FIELD-1         PIC X(10).";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertEquals(2,interpreterController.getFileSectionStatements().size());
        assertTrue(interpreterController.getFileSectionStatements().contains("       01  INPUT-RECORD."));
        assertTrue(interpreterController.getFileSectionStatements().contains("           05  IN-FIELD-1         PIC X(10)."));
    }

    @Test
    public void it_adds_file_section_statements_from_source_and_copybook() throws IOException {
        String str1 = "       FILE SECTION.";
        String str2 = "       FD  INPUT-FILE";
        String str3 = "       01  OUTPUT-RECORD.";
        String str4 = "           COPY OUTREC.";
        String str5 = "       WORKING-STORAGE SECTION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertEquals(6,interpreterController.getFileSectionStatements().size());
        assertTrue(interpreterController.getFileSectionStatements().contains("       01  OUTPUT-RECORD."));
        assertTrue(interpreterController.getFileSectionStatements().contains("           05  OUT-FIELD-1         PIC X(5)."));
        assertTrue(interpreterController.getFileSectionStatements().contains("           05  OUT-FIELD-2     PIC X(16)."));
        assertTrue(interpreterController.getFileSectionStatements().contains("           05  OUT-FIELD-3     PIC X(14)."));
        assertTrue(interpreterController.getFileSectionStatements().contains("      *    COPY OUTREC2."));
        assertTrue(interpreterController.getFileSectionStatements().contains("           05  FILLER              PIC X(5)."));
    }

    @Test
    public void it_adds_fileIdentifier_and_status() throws IOException {
        String str1 = "       ENVIRONMENT DIVISION.";
        String str2 = "       INPUT-OUTPUT SECTION.";
        String str3 = "       FILE-CONTROL.";
        String str4 = "           SELECT INPUT-FILE ASSIGN TO \"INFILE\"";
        String str5 = "               ORGANIZATION SEQUENTIAL";
        String str6 = "               ACCESS MODE SEQUENTIAL";
        String str7 = "               FILE STATUS INPUT-FILE-STATUS.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.getFileIdentifiersAndStatuses().containsKey("INPUT-FILE"));
        assertEquals("INPUT-FILE-STATUS", interpreterController.getFileIdentifiersAndStatuses().
                        get("INPUT-FILE"));
    }

    @Test
    public void it_adds_fileIdentifier_and_status_multi_line() throws IOException {
        String str1 = "       ENVIRONMENT DIVISION.";
        String str2 = "       INPUT-OUTPUT SECTION.";
        String str3 = "       FILE-CONTROL.";
        String str4 = "           SELECT";
        String str5 = "               OUTPUT-FILE";
        String str6 = "               ASSIGN TO \"OUTFILE\"";
        String str7 = "               ORGANIZATION SEQUENTIAL";
        String str8 = "               ACCESS MODE SEQUENTIAL";
        String str9 = "               FILE STATUS IS";
        String str10 = "                   OUTPUT-FILE-STATUS.";
        String str11 = "       DATA DIVISION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.getFileIdentifiersAndStatuses().containsKey("OUTPUT-FILE"));
        assertEquals("OUTPUT-FILE-STATUS", interpreterController.getFileIdentifiersAndStatuses().
                get("OUTPUT-FILE"));
    }

    @Test
    public void it_adds_fileIdentifier_and_status_while_ignoring_comments_and_empty_lines() throws IOException {
        String str1 = "       ENVIRONMENT DIVISION.";
        String str2 = "       INPUT-OUTPUT SECTION.";
        String str3 = "       FILE-CONTROL.";
        String str4 = "           SELECT";
        String str5 = "         ";
        String str6 = "      * This line is ignored";
        String str7 = "               OUTPUT-FILE";
        String str8 = "               ASSIGN TO \"OUTFILE\"";
        String str9 = "               ORGANIZATION SEQUENTIAL";
        String str10 = "               ACCESS MODE SEQUENTIAL";
        String str11 = "               FILE STATUS";
        String str12 = "         ";
        String str13 = "      * This line is ignored";
        String str14 = "                    IS OUTPUT-FILE-STATUS.";
        String str15 = "       DATA DIVISION.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6,
                str7, str8, str9, str10, str11, str12, str13, str14, str15, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertTrue(interpreterController.getFileIdentifiersAndStatuses().containsKey("OUTPUT-FILE"));
        assertEquals("OUTPUT-FILE-STATUS", interpreterController.getFileIdentifiersAndStatuses().
                get("OUTPUT-FILE"));
    }

    @Test
    public void it_reads_batch_file_io_statement() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "               READ INPUT-FILE";
        String str3 = "           END-PERFORM";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, null);

        List<String> statement = new ArrayList<>();

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            if (interpreterController.hasStatementBeenRead()){
                statement = interpreterController.getCurrentStatement();
            }
        }

        assertEquals("               READ INPUT-FILE", statement.get(0));
    }

    @Test
    public void it_reads_batch_file_io_statement_multi_line() throws IOException {
        String str1 = "       PROCEDURE DIVISION.";
        String str2 = "           OPEN";
        String str3 = "               OUTPUT";
        String str4 = "               OUTPUT-FILE";
        String str5 = "           EVALUATE TRUE";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, null);

        List<String> statement = new ArrayList<>();

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
            if (interpreterController.hasStatementBeenRead()){
                statement = interpreterController.getCurrentStatement();
            }
        }

        assertEquals(3, statement.size());
        assertEquals("               OUTPUT", statement.get(1));
        assertEquals("               OUTPUT-FILE", statement.get(2));
        assertEquals("           OPEN", statement.get(0));
    }

    @Test
    public void it_updates_numeric_fields() throws IOException {
        String str1 = "       DATA DIVISION.";
        String str2 = "       WORKING-STORAGE SECTION.";
        String str3 = "       01  FILLER.";
        String str4 = "           05  OUTPUT-FILE-STATUS PIC XX.";
        String str5 = "               88  OUTPUT-OK      VALUE '00'.";
        String str6 = "           05  WS-COUNT           PIC S9(5) COMP-3.";
        String str7 = "           05  WS-COUNT-FORMATTED PIC ZZ,ZZ9.";

        Mockito.when(mockedReader.readLine()).thenReturn(str1, str2, str3, str4, str5, str6, str7, null);

        while (interpreterController.interpretNextLine() != null){
            interpreterController.interpretNextLine();
        }

        assertEquals("PACKED_DECIMAL",
                interpreterController.getNumericFieldDataTypeFor("WS-COUNT").name());
    }








}
