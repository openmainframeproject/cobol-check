package org.openmainframeproject.cobolcheck.features.interpreter;

import org.junit.jupiter.api.Test;
import org.openmainframeproject.cobolcheck.services.cobolLogic.CobolLine;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Simple integration tests for COBOL line filtering in CobolReader
 */
public class CobolReaderLineFilteringTest {

    @Test
    public void testEjectStatementIsCommentedOut() throws IOException {
        String cobolSource = String.join(System.lineSeparator(),
            "       IDENTIFICATION DIVISION.",
            "       EJECT",
            "       DATA DIVISION."
        );
        
        BufferedReader bufferedReader = new BufferedReader(new StringReader(cobolSource));
        CobolReader cobolReader = new CobolReader(bufferedReader);

        CobolLine line1 = cobolReader.readLine();
        assertEquals("       IDENTIFICATION DIVISION.", line1.getOriginalString());

        CobolLine line2 = cobolReader.readLine();
        assertEquals("      *EJECT", line2.getOriginalString()); // Should be commented out

        CobolLine line3 = cobolReader.readLine();
        assertEquals("       DATA DIVISION.", line3.getOriginalString());

        cobolReader.close();
    }

    @Test
    public void testSkipStatementIsCommentedOut() throws IOException {
        String cobolSource = String.join(System.lineSeparator(),
            "       PROCEDURE DIVISION.",
            "       SKIP1",
            "       DISPLAY 'HELLO'."
        );
        
        BufferedReader bufferedReader = new BufferedReader(new StringReader(cobolSource));
        CobolReader cobolReader = new CobolReader(bufferedReader);

        CobolLine line1 = cobolReader.readLine();
        assertEquals("       PROCEDURE DIVISION.", line1.getOriginalString());

        CobolLine line2 = cobolReader.readLine();
        assertEquals("      *SKIP1", line2.getOriginalString()); // Should be commented out

        CobolLine line3 = cobolReader.readLine();
        assertEquals("       DISPLAY 'HELLO'.", line3.getOriginalString());

        cobolReader.close();
    }

    @Test
    public void testSequenceNumbersAreRemoved() throws IOException {
        String cobolSource = String.join(System.lineSeparator(),
            "       MOVE 1 TO A.                                                     000010",
            "       DISPLAY 'TEST'.                                                  ABC123"
        );
        
        BufferedReader bufferedReader = new BufferedReader(new StringReader(cobolSource));
        CobolReader cobolReader = new CobolReader(bufferedReader);

        CobolLine line1 = cobolReader.readLine();
        assertEquals("       MOVE 1 TO A.                                                     ", line1.getOriginalString());

        CobolLine line2 = cobolReader.readLine();
        assertEquals("       DISPLAY 'TEST'.                                                  ", line2.getOriginalString());

        cobolReader.close();
    }
}
