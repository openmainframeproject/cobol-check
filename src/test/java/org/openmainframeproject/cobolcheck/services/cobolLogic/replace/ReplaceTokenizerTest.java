package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ReplaceTokenizerTest {

    @Test
    public void it_accepts_an_empty_string_as_source() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();
        tokenizer.tokenize("");
        assertFalse(tokenizer.hasMoreTokens());
    }

    @Test
    public void it_can_tokenize_more_strings_resetting_every_time() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();
        tokenizer.tokenize("first line.");

        assertTrue(tokenizer.hasMoreTokens());
        ReplaceToken rt = tokenizer.nextToken();
        assertEquals("first", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals("line", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals(".", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        tokenizer.tokenize("second line.");

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals("second", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals("line", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals(".", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        assertFalse(tokenizer.hasMoreTokens());

    }



    @Test
    public void it_tokenizes_a_single_line_replace_statement() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();
        tokenizer.tokenize("REPLACE ==MODULE== BY ==NEW-NAME==.");
        assertTrue(tokenizer.hasMoreTokens());
        ReplaceToken rt = tokenizer.nextToken();
        // Looking at the tokens and the ReplaceToken class,
        // we can see that the first token is "REPLACE" and the type is REPLACE
        assertSame(rt.getType(), ReplaceTokenType.REPLACE);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.BY);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        assertFalse(tokenizer.hasMoreTokens());
    }

    @Test
    public void it_tokenizes_various_lines() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();
        tokenizer.tokenize("Tokenize a line. Period is only terminator is followed by a whitespace char.");

        assertTrue(tokenizer.hasMoreTokens());
        ReplaceToken rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals("line", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals(".", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals("Period", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);


        tokenizer.tokenize("Period.is.terminator.if.followed.by.a.whitespace.char.");

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals("Period.is.terminator.if.followed.by.a.whitespace.char", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertEquals(".", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        assertFalse(tokenizer.hasMoreTokens());
    }

    @Test
    public void it_tokenizes_a_part_of_a_statement() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();
        tokenizer.tokenize("==something== BY ==NEW-NAME==.");

        assertTrue(tokenizer.hasMoreTokens());
        ReplaceToken rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.BY);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.OTHER);

        assertTrue(tokenizer.hasMoreTokens());
        rt = tokenizer.nextToken();
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        assertFalse(tokenizer.hasMoreTokens());
    }


    @Test
    public void it_tokenizes_a_single_period() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();
        tokenizer.tokenize(".");

        assertTrue(tokenizer.hasMoreTokens());
        ReplaceToken rt = tokenizer.nextToken();
        assertEquals(".", rt.getValue());
        assertSame(rt.getType(), ReplaceTokenType.TERMINATOR);

        assertFalse(tokenizer.hasMoreTokens());
    }

    @Test
    public void it_finds_a_comment() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();

        tokenizer.tokenize("      * This is a comment");
        assertTrue(tokenizer.isComment());

        tokenizer.tokenize("* 01 WS-NAME PIC X(10).");
        assertTrue(tokenizer.isComment());

        tokenizer.tokenize("123456* replace ==from== BY ==to==.");
        assertTrue(tokenizer.isComment());

        tokenizer.tokenize("12* 01 WS-NAME PIC X(10).");
        assertTrue(tokenizer.isComment());

        tokenizer.tokenize("123456- is not recognized as comment.");
        assertFalse(tokenizer.isComment());

        tokenizer.tokenize("123456D is not recognized as comment.");
        assertFalse(tokenizer.isComment());
    }

    @Test
    public void it_finds_sourcecode_lines() {
        ReplaceTokenizer tokenizer = new ReplaceTokenizer();

        tokenizer.tokenize("         01 WS-NAME PIC X(10).");
        assertFalse(tokenizer.isComment());

        tokenizer.tokenize("147258    01 WS-NAME PIC X(10).");
        assertFalse(tokenizer.isComment());

    }



}
