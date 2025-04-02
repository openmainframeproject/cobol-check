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

        File cobolFile = new File("./testfiles/REPLACE.CBL");
        ReplaceStatementLocator locator = new ReplaceStatementLocator(cobolFile);

        // Verify that the accumulateStatement method was called
        // TODO: FIX MOCK: Mockito.verify(mockedLocator, times(8)).accumulateStatement(anyString());

        // TODO; remove the counters when the mock is working
        assertEquals(11, locator.commentLinesFound);
        assertEquals(56, locator.sourceLinesProcessed);

    }

    @Test
    public void testAccumulateStatement() {

        ReplaceStatementLocator locator = new ReplaceStatementLocator();
        locator.accumulateStatement("123245  REPLACE ==REPLACE== BY ==REPLACED==.",0);
        assertEquals(1, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
    }

    @Test
    public void testAccumulateStatementWithMultipleLines() {
        ReplaceStatementLocator locator = new ReplaceStatementLocator();

        locator.accumulateStatement("123245  REPLACE ==REPLACE== BY ==REPLACED==   ",0);
        locator.accumulateStatement("123456          ==PETER== BY ==PHIL==.   ",0);
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());
    }

    @Test
    public void testAccumulateStatementWithMultipleLinesAndComments() {
        ReplaceStatementLocator locator = new ReplaceStatementLocator();

        locator.accumulateStatement("123456* REPLACE ==REPLACE== BY ==REPLACED==   ",0);
        locator.accumulateStatement("123456  REPLACE ==REPLACE== BY ==REPLACED==   ",0);
        locator.accumulateStatement("123456*  REPLACE ==BEEF== BY ==SALAD==   ",0);
        locator.accumulateStatement("123456          ==PETER== BY ==PHIL==.   ",0);
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());


        locator = new ReplaceStatementLocator();
        locator.accumulateStatement("* REPLACE ==REPLACE== BY ==REPLACED==   ",0);
        locator.accumulateStatement("  REPLACE ==REPLACE== BY ==REPLACED==   ",0);
        locator.accumulateStatement(" *  REPLACE ==BEEF== BY ==SALAD==   ",0);
        locator.accumulateStatement("          ==PETER== BY ==PHIL==.   ",0);
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
        locator.createStatements("REPLACE ==REPLACE== BY ==REPLACED==",0);
        assertEquals(1, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());

        //Two sets of keywords
        locator = new ReplaceStatementLocator();
        locator.createStatements("REPLACE ==REPLACE== BY ==REPLACED== ==PETER== BY ==PHIL==",0);
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());

        //Two sets of keywords with one leading and one trailing
        locator = new ReplaceStatementLocator();
        locator.createStatements("REPLACE TRAILING ==REPLACE== BY ==REPLACED== LEADING ==PETER== BY ==PHIL==",0);
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals("REPLACE", locator.getReplaceSets().get(0).getFrom());
        assertEquals("REPLACED", locator.getReplaceSets().get(0).getTo());
        assertEquals("PETER", locator.getReplaceSets().get(1).getFrom());
        assertEquals("PHIL", locator.getReplaceSets().get(1).getTo());
        assertTrue(locator.getReplaceSets().get(0).isTrailing());
        assertTrue(locator.getReplaceSets().get(1).isLeading());
    }

    @Test
    public void testSourcelinesAreAddedToReplaceSets() {
        ReplaceStatementLocator locator = new ReplaceStatementLocator();
        locator.createStatements("REPLACE ==REPLACE== BY ==REPLACED==", 10);
        assertEquals(1, locator.getReplaceSets().size());
        assertEquals(11, locator.getReplaceSets().get(0).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(0).getUntilSourceLine());

        // add a second set of keywords, the previous set should have the untilSourceLine set
        locator.createStatements("REPLACE ==MONDAY== BY ==TUESDAY==",100);
        assertEquals(2, locator.getReplaceSets().size());
        assertEquals(11, locator.getReplaceSets().get(0).getFromSourceLine());
        assertEquals(99, locator.getReplaceSets().get(0).getUntilSourceLine());
        assertEquals(101, locator.getReplaceSets().get(1).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(1).getUntilSourceLine());

        // add a third set of keywords, the previous set should have the untilSourceLine set
        locator.createStatements("REPLACE ==THURSDAY== BY ==FRIDAY==",785);
        assertEquals(3, locator.getReplaceSets().size());
        assertEquals(11, locator.getReplaceSets().get(0).getFromSourceLine());
        assertEquals(99, locator.getReplaceSets().get(0).getUntilSourceLine());
        assertEquals(101, locator.getReplaceSets().get(1).getFromSourceLine());
        assertEquals(784, locator.getReplaceSets().get(1).getUntilSourceLine());
        assertEquals(786, locator.getReplaceSets().get(2).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(2).getUntilSourceLine());

        // add a fourth time - two sets, the previous set should have the untilSourceLine set
        locator.createStatements("REPLACE TRAILING ==REPLACE== BY ==REPLACED== LEADING ==PETER== BY ==PHIL==",950);
        assertEquals(5, locator.getReplaceSets().size());
        assertEquals(786, locator.getReplaceSets().get(2).getFromSourceLine());
        assertEquals(949, locator.getReplaceSets().get(2).getUntilSourceLine());
        assertEquals(951, locator.getReplaceSets().get(3).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(3).getUntilSourceLine());
        assertEquals(951, locator.getReplaceSets().get(4).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(4).getUntilSourceLine());

        // add a fifth time - two sets, the previous sets should have the untilSourceLine set
        locator.createStatements("REPLACE TRAILING ==REPLACE== BY ==NEW-WORD== LEADING ==PETER== BY ==Nicholas==",1000);
        assertEquals(7, locator.getReplaceSets().size());
        assertEquals(951, locator.getReplaceSets().get(3).getFromSourceLine());
        assertEquals(999, locator.getReplaceSets().get(3).getUntilSourceLine());
        assertEquals(951, locator.getReplaceSets().get(4).getFromSourceLine());
        assertEquals(999, locator.getReplaceSets().get(4).getUntilSourceLine());
        assertEquals(1001, locator.getReplaceSets().get(5).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(5).getUntilSourceLine());
        assertEquals(1001, locator.getReplaceSets().get(6).getFromSourceLine());
        assertEquals(0, locator.getReplaceSets().get(6).getUntilSourceLine());
    }

}
