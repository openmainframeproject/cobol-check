package org.openmainframeproject.cobolcheck.features.interpreter;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class CopybookExpanderTest {
    //                           1         2         3
    //                 012345678901234567890123456789012
    String execSlq1 = "EXEC SQL INCLUDE TRMCKP END-EXEC.";
    CopybookExpander ce;

    @BeforeAll
    static void oneTimeSetup() {
        Config.load("testconfig.properties");
    }


    @BeforeEach
    public void setup(){
        ce = new CopybookExpander();
    }

    @Test
    public void test_find_copybook_position_start(){
        int start = ce.findCopybookNamePositionInCopyStatement(execSlq1);
        assertEquals(17,start,"start found");
    }

    @Test
    public void test_find_copybook_position_end() {
        int endPos = ce.findCopyBookNameEndPositionInCopyStatement(execSlq1,17);
        assertEquals(23,endPos,"we found the end");
    }

    @Test
    public void test_we_actually_locate_the_copybook_name() {
        int start = ce.findCopybookNamePositionInCopyStatement(execSlq1);
        int endPos = ce.findCopyBookNameEndPositionInCopyStatement(execSlq1,start);
        assertEquals("TRMCKP",execSlq1.substring(start,endPos));
    }

    @Test
    public void test_extractCopybookNameFromCopyStatement(){
        assertEquals("onebookname",ce.extractCopybookNameFromCopyStatement("EXEC SQL INCLUDE onebookname END-EXEC."));
    }

    @Test
    public void commentOut_adds_asterisk_at_column_7() {
        String sourceLine = "123456 89";
        String result = ce.commentOut(sourceLine);
        assertEquals("123456*89", result);
    }

    @Test
    public void commentOut_handles_short_lines_without_exception() {
        String sourceLine = "12345";
        String result = ce.commentOut(sourceLine);
        assertEquals("12345", result);
    }

    @Test
    public void commentOut_handles_empty_string() {
        String sourceLine = "";
        String result = ce.commentOut(sourceLine);
        assertEquals("", result);
    }

    @Test
    public void commentOut_handles_null_input() {
        String sourceLine = null;
        String result = ce.commentOut(sourceLine);
        assertEquals(null, result);
    }

    @Test
    public void commentOut_handles_input_where_indicator_is_blocked() {
        String sourceLine = "123456-          'test continuation'.";
        assertThrows(PossibleInternalLogicErrorException.class, () -> ce.commentOut(sourceLine));
    }

    @Test
    public void commentOut_handles_input_where_indicator_is_comment() {
        String sourceLine = "123456*     if more < to-to-come  then";
        String result = ce.commentOut(sourceLine);
        assertEquals(sourceLine, result);
    }

}
