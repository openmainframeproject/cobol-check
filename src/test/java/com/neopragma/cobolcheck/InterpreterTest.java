package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.interpreter.*;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.cobolLogic.CobolLine;
import com.neopragma.cobolcheck.services.cobolLogic.Interpreter;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class InterpreterTest {

    State state;
    TokenExtractor tokenExtractor;

    @BeforeEach
    public void commenSetup()
    {
        state = new State();
        tokenExtractor = new StringTokenizerExtractor();
    }

    @Test
    public void it_recognizes_end_of_statement_when_ending_in_period(){
        CobolLine current = new CobolLine("               FILE STATUS INPUT-FILE-STATUS.", tokenExtractor);
        CobolLine next = new CobolLine(" ", tokenExtractor);

        boolean isEnd = Interpreter.isEndOfStatement(current, next);

        assertTrue(isEnd);
    }

    @Test
    public void it_recognizes_end_of_statement_when_next_line_is_verb(){
        CobolLine current = new CobolLine("               WHEN OUTPUT-OK", tokenExtractor);
        CobolLine next = new CobolLine("                   CONTINUE", tokenExtractor);

        boolean isEnd = Interpreter.isEndOfStatement(current, next);

        assertTrue(isEnd);
    }

    @Test
    public void it_finds_too_short_line(){
        CobolLine line = new CobolLine(" .", tokenExtractor);

        boolean isTooShort = Interpreter.isTooShortToBeMeaningful(line);

        assertTrue(isTooShort);
    }

    @Test
    public void it_accepts_normal_length_line(){
        CobolLine line = new CobolLine("               WHEN OUTPUT-OK", tokenExtractor);

        boolean isTooShort = Interpreter.isTooShortToBeMeaningful(line);

        assertFalse(isTooShort);
    }

    @Test
    public void it_recognizes_comment(){
        CobolLine line = new CobolLine("      * Layout of an input record", tokenExtractor);

        boolean isComment = Interpreter.isComment(line);

        assertTrue(isComment);
    }

    @Test
    public void non_comment_is_not_recognized_as_comment(){
        CobolLine line = new CobolLine("       Layout of an input record", tokenExtractor);

        boolean isComment = Interpreter.isComment(line);

        assertFalse(isComment);
    }

    @Test
    public void it_recognizes_empty_line(){
        CobolLine line1 = new CobolLine("", tokenExtractor);
        CobolLine line2 = new CobolLine("                               ", tokenExtractor);

        boolean isEmpty1 = Interpreter.isEmpty(line1);
        boolean isEmpty2 = Interpreter.isEmpty(line1);

        assertTrue(isEmpty1);
        assertTrue(isEmpty2);
    }

    @Test
    public void it_recognizes_non_empty_line(){
        CobolLine line1 = new CobolLine("               WHEN OUTPUT-OK", tokenExtractor);

        boolean isEmpty = Interpreter.isEmpty(line1);

        assertFalse(isEmpty);
    }

    @Test
    public void it_recognizes_lines_that_should_not_be_parsed(){
        CobolLine line1 = new CobolLine("  ", tokenExtractor);
        //In FILE SECTION
        CobolLine line2 = new CobolLine("       FD  INPUT-FILE", tokenExtractor);
        state.setFlagFor(Constants.FILE_SECTION);

        boolean shouldBeParsed1 = Interpreter.shouldLineBeParsed(line1, state);
        boolean shouldBeParsed2 = Interpreter.shouldLineBeParsed(line2, state);

        assertFalse(shouldBeParsed1);
        assertFalse(shouldBeParsed2);
    }

    @Test
    public void it_recognizes_lines_that_should_be_parsed(){
        CobolLine line1 = new CobolLine("           .", tokenExtractor);
        CobolLine line2 = new CobolLine("                   PERFORM 9999-ABORT", tokenExtractor);
        state.setFlagFor(Constants.PROCEDURE_DIVISION);

        boolean shouldBeParsed1 = Interpreter.shouldLineBeParsed(line1, state);
        boolean shouldBeParsed2 = Interpreter.shouldLineBeParsed(line2, state);

        assertTrue(shouldBeParsed1);
        assertTrue(shouldBeParsed2);
    }

    @Test
    public void it_recognizes_batch_file_io_statement(){
        CobolLine line = new CobolLine("           OPEN INPUT INPUT-FILE", tokenExtractor);

        boolean isBatchFileIOStatement = Interpreter.checkForBatchFileIOStatement(line);

        assertTrue(isBatchFileIOStatement);
    }

    @Test
    public void it_recognizes_non_batch_file_io_statement(){
        CobolLine line = new CobolLine("       5200-PREPARE-OUTPUT-RECORD.", tokenExtractor);

        boolean isBatchFileIOStatement = Interpreter.checkForBatchFileIOStatement(line);

        assertFalse(isBatchFileIOStatement);
    }

    @Test
    public void it_recognizes_sequence_number_area(){
        CobolLine line = new CobolLine ("001200 DATA DIVISION.", tokenExtractor);
        assertEquals(Area.SEQUENCE_NUMBER, Interpreter.getBeginningArea(line, false));
    }

    @Test
    public void it_recognizes_indicator_area_while_ignoring_sequence_number_area(){
        CobolLine line = new CobolLine ("001200-              \"World\"", tokenExtractor);
        assertEquals(Area.INDICATOR, Interpreter.getBeginningArea(line, true));
    }

    @Test
    public void it_recognizes_a_area(){
        CobolLine line = new CobolLine ("       000-START SECTION.", tokenExtractor);
        assertEquals(Area.A, Interpreter.getBeginningArea(line, false));
    }

    @Test
    public void it_recognizes_b_area(){
        CobolLine line = new CobolLine ("           PERFORM 003-DO-SOMETHING", tokenExtractor);
        assertEquals(Area.B, Interpreter.getBeginningArea(line, false));
    }

    @Test
    public void it_returns_none_if_outside_areas(){
        CobolLine line = new CobolLine ("                                                                     " +
                "    Too many spaces!", tokenExtractor);
        assertEquals(Area.NONE, Interpreter.getBeginningArea(line, false));
    }

    @Test
    public void it_returns_none_if_ignoring_sequence_area_but_string_is_too_short(){
        String none      = ("01");
        CobolLine line = new CobolLine ("01", tokenExtractor);
        assertEquals(Area.NONE, Interpreter.getBeginningArea(line, true));
    }


    private boolean mapsAreEqual(Map<String, State.Flag> first, Map<String, State.Flag> second) {
        if (first.size() != second.size()) {
            return false;
        }

        return first.entrySet().stream()
                .allMatch(e -> e.getValue().isSet() == (second.get(e.getKey()).isSet()));
    }

}
