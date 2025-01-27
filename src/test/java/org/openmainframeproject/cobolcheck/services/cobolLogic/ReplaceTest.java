package org.openmainframeproject.cobolcheck.services.cobolLogic;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ReplaceTest {
    @BeforeEach
    public void setUp() {
        Replace.reset();
    }

    @Test
    public void it_accepts_an_empty_string_as_source() {
        String testLine = "";
        String result = Replace.replace(testLine);
        assertEquals("", result);

        Replace.setReplaceStatement(testLine);
    }

    @Test
    public void it_accepts_a_null_string_as_source() {
        String testLine = null;
        String result = Replace.replace(testLine);
        assertEquals(null, result);

        Replace.setReplaceStatement(testLine);
    }

    @Test
    public void it_will_preserve_the_source_string_when_no_replace_is_needed() {
        String testLine = " This is a test line. When no replace is needed. ";
        String result = Replace.replace(testLine);
        Replace.setReplaceStatement(testLine);
        assertEquals(testLine, result);
    }

    @Test
    public void it_will_recognize_replace_on_one_line() {
        String testLine = "        REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.setReplaceStatement(testLine);
        assertTrue(Replace.isReplaceOn());

        Replace.reset();
        testLine = " REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.setReplaceStatement(testLine);
        assertTrue(Replace.isReplaceOn());

        Replace.reset();
        testLine = " replace ==MODULE== by ==NEW-NAME==.";
        Replace.setReplaceStatement(testLine);
        assertTrue(Replace.isReplaceOn());

    }

    @Test
    public void it_will_recognize_a_comment_and_not_start_replace() {
        String testLine = "      *     REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.setReplaceStatement(testLine);
        assertFalse(Replace.isReplaceOn());

        testLine = " * REPLACE ==MODULE== BY ==NEW-NAME==.        ";
        Replace.setReplaceStatement(testLine);
        assertFalse(Replace.isReplaceOn());
    }

    @Test
    public void it_will_recognize_replace_off() {
        String testLine = " REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.setReplaceStatement(testLine);
        assertTrue(Replace.isReplaceOn());

        String replaceOffSentence = " REPLACE OFF.";
        Replace.setReplaceStatement(replaceOffSentence);
        assertFalse(Replace.isReplaceOn());

    }

    @Test
    public void it_will_recognize_a_comment_and_not_make_replace_off() {
        String testLine = " REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.setReplaceStatement(testLine);
        assertTrue(Replace.isReplaceOn());

        String replaceOffSentence = " * REPLACE OFF.";
        Replace.setReplaceStatement(replaceOffSentence);
        assertTrue(Replace.isReplaceOn());
    }

    @Test
    public void it_will_replace_the_string() {
        String replaceOn = " REPLACE ==:MODULE:== BY ==NEWNAME==.";
        Replace.setReplaceStatement(replaceOn);

        String sourceLine = "  EXPECT MAX-LENGTH          IN :MODULE:-PARM\n";
        String result = Replace.replace(sourceLine);
        assertEquals("  EXPECT MAX-LENGTH          IN NEWNAME-PARM\n", result);
    }


    @Test
    public void avoid_replacing_when_the_test_source_line_is_a_comment() {
        String replaceOn = " REPLACE ==:MODULE:== BY ==NEWNAME==.";
        Replace.setReplaceStatement(replaceOn);

        String sourceLine = "  EXPECT MAX-LENGTH          IN :BDSIXXX:-PARM\n";
        String result = Replace.replace(sourceLine);
        assertEquals("  EXPECT MAX-LENGTH          IN :BDSIXXX:-PARM\n", result);
    }
}
