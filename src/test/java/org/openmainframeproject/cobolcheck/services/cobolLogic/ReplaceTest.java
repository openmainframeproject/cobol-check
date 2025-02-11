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

        Replace.inspect(testLine);
    }

    @Test
    public void it_accepts_a_null_string_as_source() {
        String testLine = null;
        String result = Replace.replace(testLine);
        assertEquals(null, result);

        Replace.inspect(testLine);
    }

    @Test
    public void it_will_preserve_the_source_string_when_no_replace_is_needed() {
        String testLine = " This is a test line. When no replace is needed. ";
        String result = Replace.replace(testLine);
        Replace.inspect(testLine);
        assertEquals(testLine, result);
    }

    @Test
    public void it_will_recognize_replace_on_one_line() {
        String testLine = "        REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.inspect(testLine);
        assertTrue(Replace.isReplaceOn());

        Replace.reset();
        testLine = " REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.inspect(testLine);
        assertTrue(Replace.isReplaceOn());

        Replace.reset();
        testLine = " replace ==MODULE== by ==NEW-NAME==.";
        Replace.inspect(testLine);
        assertTrue(Replace.isReplaceOn());

    }

    @Test
    public void it_will_recognize_a_comment_and_not_start_replace() {
        String testLine = "      *     REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.inspect(testLine);
        assertFalse(Replace.isReplaceOn());

        testLine = " * REPLACE ==MODULE== BY ==NEW-NAME==.        ";
        Replace.inspect(testLine);
        assertFalse(Replace.isReplaceOn());
    }

    @Test
    public void it_will_recognize_replace_off() {
        String testLine = " REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.inspect(testLine);
        assertTrue(Replace.isReplaceOn());

        String replaceOffSentence = " REPLACE OFF.";
        Replace.inspect(replaceOffSentence);
        assertFalse(Replace.isReplaceOn());

    }

    @Test
    public void it_will_recognize_a_comment_and_not_make_replace_off() {
        String testLine = " REPLACE ==MODULE== BY ==NEW-NAME==.";
        Replace.inspect(testLine);
        assertTrue(Replace.isReplaceOn());

        String replaceOffSentence = " * REPLACE OFF.";
        Replace.inspect(replaceOffSentence);
        assertTrue(Replace.isReplaceOn());
    }

    @Test
    public void it_will_replace_the_string() {
        String replaceOn = " REPLACE ==:MODULE:== BY ==NEWNAME==.";
        Replace.inspect(replaceOn);

        String sourceLine = "  EXPECT MAX-LENGTH          IN :MODULE:-PARM\n";
        String result = Replace.replace(sourceLine);
        assertEquals("  EXPECT MAX-LENGTH          IN NEWNAME-PARM\n", result);
    }

    @Test
    public void avoid_replacing_when_the_test_source_line_is_a_comment() {
        String replaceOn = " REPLACE ==:MODULE:== BY ==NEWNAME==.";
        Replace.inspect(replaceOn);

        // Comment line with the replace from keyword. This must remain untouched
        String sourceLine = " *  EXPECT MAX-LENGTH          IN :MODULE:-PARM\n";
        String result = Replace.replace(sourceLine);
        assertEquals(" *  EXPECT MAX-LENGTH          IN :MODULE:-PARM\n", result);

        // Comment line without the replace to keyword. This must remain untouched
        sourceLine = "      *REM> READ            READ DATA\n";
        result = Replace.replace(sourceLine);
        assertEquals("      *REM> READ            READ DATA\n", result);

        // Numbered comment line with the replace to keyword. This must remain untouched
        sourceLine = "007100* INCLUDE :MODULE: FOR MAINTENANCE";
        result = Replace.replace(sourceLine);
        assertEquals("007100* INCLUDE :MODULE: FOR MAINTENANCE", result);
    }
}