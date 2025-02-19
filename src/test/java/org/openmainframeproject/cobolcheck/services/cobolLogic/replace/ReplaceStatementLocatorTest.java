package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.io.File;
import java.util.LinkedList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;

public class ReplaceStatementLocatorTest {

    @Test
    public void testReplaceStatementLocator() {
        // we test alle the lines in the file
        // are read and processed


        // Create a mock of ReplaceStatementLocator
        // TODO: FIX MOCK: ReplaceStatementLocator mockedLocator = Mockito.mock(ReplaceStatementLocator.class);

        // Define the behavior of the accumulateStatement method
        // TODO: FIX MOCK: Mockito.doNothing().when(mockedLocator).accumulateStatement(anyString());

        File cobolFile = new File("testfiles/replace.cbl");
        ReplaceStatementLocator locator = new ReplaceStatementLocator(cobolFile);

        // Verify that the accumulateStatement method was called
        // TODO: FIX MOCK: Mockito.verify(mockedLocator, times(8)).accumulateStatement(anyString());

        // TODO; remove the counters when the mock is working
        assertEquals(7, locator.commentLinesFound);
        assertEquals(25, locator.sourceLinesProcessed);

    }

    @Test
    public void testAccumulateStatement() {

        ReplaceStatementLocator locator = new ReplaceStatementLocator();
        locator.accumulateStatement("123245  REPLACE ==REPLACE== BY ==REPLACED==.");
        assertEquals(1, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());



    }

    @Test
    public void testAccumulateStatementWithMultipleLines() {
        ReplaceStatementLocator locator = new ReplaceStatementLocator();

        locator = new ReplaceStatementLocator();
        locator.accumulateStatement("123245  REPLACE ==REPLACE== BY ==REPLACED==   ");
        locator.accumulateStatement("123456          ==PETER== BY ==PHIL==.   ");
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());

    }

    @Test
    public void testAccumulateStatementWithMultipleLinesAndComments() {
        ReplaceStatementLocator locator = new ReplaceStatementLocator();

        locator = new ReplaceStatementLocator();
        locator.accumulateStatement("123456* REPLACE ==REPLACE== BY ==REPLACED==   ");
        locator.accumulateStatement("123456  REPLACE ==REPLACE== BY ==REPLACED==   ");
        locator.accumulateStatement("123456*  REPLACE ==BEEF== BY ==SALAD==   ");
        locator.accumulateStatement("123456          ==PETER== BY ==PHIL==.   ");
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());


        locator = new ReplaceStatementLocator();
        locator.accumulateStatement("* REPLACE ==REPLACE== BY ==REPLACED==   ");
        locator.accumulateStatement("  REPLACE ==REPLACE== BY ==REPLACED==   ");
        locator.accumulateStatement(" *  REPLACE ==BEEF== BY ==SALAD==   ");
        locator.accumulateStatement("          ==PETER== BY ==PHIL==.   ");
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());
    }

    @Test
    public void testCreateStatements() {

        //one set of keywords
        ReplaceStatementLocator locator = new ReplaceStatementLocator();
        locator.createStatements("REPLACE ==REPLACE== BY ==REPLACED==");
        assertEquals(1, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());

        //Two sets of keywords
        locator = new ReplaceStatementLocator();
        locator.createStatements("REPLACE ==REPLACE== BY ==REPLACED== ==PETER== BY ==PHIL==");
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());

        //Two sets of keywords with one leading and one trailing
        locator = new ReplaceStatementLocator();
        locator.createStatements("REPLACE TRAILING ==REPLACE== BY ==REPLACED== LEADING ==PETER== BY ==PHIL==");
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());
        assertTrue(locator.getReplaceSets().get(0).isTrailing());
        assertTrue(locator.getReplaceSets().get(1).isLeading());
    }
}
