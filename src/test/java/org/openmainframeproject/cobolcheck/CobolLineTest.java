package org.openmainframeproject.cobolcheck;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openmainframeproject.cobolcheck.features.interpreter.State;
import org.openmainframeproject.cobolcheck.features.interpreter.StringTokenizerExtractor;
import org.openmainframeproject.cobolcheck.services.cobolLogic.CobolLine;
import org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class CobolLineTest {

    TokenExtractor tokenExtractor;

    @BeforeEach
    public void commenSetup()
    {
        tokenExtractor = new StringTokenizerExtractor();
    }

    @Test
    public void it_builds_a_correct_cobol_line_with_sequence_number() {
        String originalLine = "123456 MOVE 6 TO WS-STORAGE";

        String expectedOriginalString = originalLine;
        String expectedUnNumberedString = "       MOVE 6 TO WS-STORAGE";
        String expectedTrimmedString = "MOVE 6 TO WS-STORAGE";
        List<String> expectedTokens = Arrays.asList("MOVE", "6", "TO", "WS-STORAGE");

        CobolLine line = new CobolLine(originalLine, tokenExtractor);
        assertEquals(line.getOriginalString(), expectedOriginalString);
        assertEquals(line.getUnNumberedString(), expectedUnNumberedString);
        assertEquals(line.getTrimmedString(), expectedTrimmedString);
        assertEquals(line.getTokens(), expectedTokens);
    }

    @Test
    public void it_builds_a_correct_cobol_line_without_sequence_number() {
        String originalLine = "       MOVE 6 TO WS-STORAGE";

        String expectedOriginalString = originalLine;
        String expectedUnNumberedString = originalLine;
        String expectedTrimmedString = "MOVE 6 TO WS-STORAGE";
        List<String> expectedTokens = Arrays.asList("MOVE", "6", "TO", "WS-STORAGE");

        CobolLine line = new CobolLine(originalLine, tokenExtractor);
        assertEquals(line.getOriginalString(), expectedOriginalString);
        assertEquals(line.getUnNumberedString(), expectedUnNumberedString);
        assertEquals(line.getTrimmedString(), expectedTrimmedString);
        assertEquals(line.getTokens(), expectedTokens);
    }

    @Test
    public void it_handles_empty_line() {
        String originalLine = "";

        String expectedOriginalString = originalLine;
        String expectedUnNumberedString = "";
        String expectedTrimmedString = "";
        List<String> expectedTokens = new ArrayList<>();

        CobolLine line = new CobolLine(originalLine, tokenExtractor);
        assertEquals(line.getOriginalString(), expectedOriginalString);
        assertEquals(line.getUnNumberedString(), expectedUnNumberedString);
        assertEquals(line.getTrimmedString(), expectedTrimmedString);
        assertEquals(line.getTokens(), expectedTokens);
    }

    @Test
    public void it_handles_five_spaces() {
        String originalLine = "     ";

        String expectedOriginalString = originalLine;
        String expectedUnNumberedString = originalLine;
        String expectedTrimmedString = "";
        List<String> expectedTokens = new ArrayList<>();

        CobolLine line = new CobolLine(originalLine, tokenExtractor);
        assertEquals(line.getOriginalString(), expectedOriginalString);
        assertEquals(line.getUnNumberedString(), expectedUnNumberedString);
        assertEquals(line.getTrimmedString(), expectedTrimmedString);
        assertEquals(line.getTokens(), expectedTokens);
    }

    @Test
    public void it_handles_six_spaces() {
        String originalLine = "      ";

        String expectedOriginalString = originalLine;
        String expectedUnNumberedString = originalLine;
        String expectedTrimmedString = "";
        List<String> expectedTokens = new ArrayList<>();

        CobolLine line = new CobolLine(originalLine, tokenExtractor);
        assertEquals(line.getOriginalString(), expectedOriginalString);
        assertEquals(line.getUnNumberedString(), expectedUnNumberedString);
        assertEquals(line.getTrimmedString(), expectedTrimmedString);
        assertEquals(line.getTokens(), expectedTokens);
    }

    @Test
    public void it_handles_seven_spaces() {
        String originalLine = "       ";

        String expectedOriginalString = originalLine;
        String expectedUnNumberedString = originalLine;
        String expectedTrimmedString = "";
        List<String> expectedTokens = new ArrayList<>();

        CobolLine line = new CobolLine(originalLine, tokenExtractor);
        assertEquals(line.getOriginalString(), expectedOriginalString);
        assertEquals(line.getUnNumberedString(), expectedUnNumberedString);
        assertEquals(line.getTrimmedString(), expectedTrimmedString);
        assertEquals(line.getTokens(), expectedTokens);
    }
}
