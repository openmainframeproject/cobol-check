package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ReplaceSetTest {

    @Test
    public void testReplaceSet() {

        // Create a Replace set
        ReplaceSet replaceSet = new ReplaceSet("REPLACE", "REPLACED", false, false,0,0);
        assertEquals("REPLACE", replaceSet.getFrom());
        assertEquals("REPLACED", replaceSet.getTo());
        assertFalse(replaceSet.isTrailing());
        assertFalse(replaceSet.isLeading());

        replaceSet = new ReplaceSet("BRUCE", "CAITLYN", true, false,0,0);
        assertEquals("BRUCE", replaceSet.getFrom());
        assertEquals("CAITLYN", replaceSet.getTo());
        assertTrue(replaceSet.isTrailing());
        assertFalse(replaceSet.isLeading());

        replaceSet = new ReplaceSet("JOHNNY", "JAY", false, true,0,0);
        assertEquals("JOHNNY", replaceSet.getFrom());
        assertEquals("JAY", replaceSet.getTo());
        assertFalse(replaceSet.isTrailing());
        assertTrue(replaceSet.isLeading());

        // Having both trailing and leading set to true is not allowed
        // This should throw an exception
        // Not possible via constructor
        assertThrows(IllegalArgumentException.class, () -> new ReplaceSet("JOHNNY", "JAY", true, true,0,0));
        // not possible via setter
        ReplaceSet replaceSetLeading = new ReplaceSet("REPLACE", "REPLACED", false, true,0,0);
        assertThrows(IllegalArgumentException.class, () -> replaceSetLeading.setTrailing(true));
        ReplaceSet replaceSetTrailing = new ReplaceSet("REPLACE", "REPLACED", true, false,0,0);
        assertThrows(IllegalArgumentException.class, () -> replaceSetTrailing.setLeading(true));
    }

    @Test
    public void testReplaceInline() {
        ReplaceSet replaceSet = new ReplaceSet("JOHNNY", "JAY", false, false,0,0);
        String from = "Johnny is behind the iconic late-night desk.";
        String expected = "JAY is behind the iconic late-night desk.";
        assertEquals(expected, replaceSet.replaceInline(from));
    }

    @Test
    public void testReplaceInlineTrailing() {
        ReplaceSet replaceSet = new ReplaceSet("night", "day", true, false,0,0);
        String from = "Johnny is behind the iconic late-night desk.";
        String expected = "Johnny is behind the iconic late-day desk.";
        assertEquals(expected, replaceSet.replaceInline(from));

        from = "Johnny is working day and night behind the iconic late-night desk.";
        expected = "Johnny is working day and night behind the iconic late-day desk.";
        assertEquals(expected, replaceSet.replaceInline(from));

        from = "Johnny is working day and night behind the iconic desk.";
        expected = "Johnny is working day and night behind the iconic desk.";
        assertEquals(expected, replaceSet.replaceInline(from));
    }

    @Test
    public void testReplaceInlineLeading() {
        ReplaceSet replaceSet = new ReplaceSet("late", "early", false, true,0,0);
        String from = "Johnny is behind the iconic late-night desk.";
        String expected = "Johnny is behind the iconic early-night desk.";
        assertEquals(expected, replaceSet.replaceInline(from));

        from = "Johnny is working early and late behind the iconic late-night desk.";
        expected = "Johnny is working early and late behind the iconic early-night desk.";
        assertEquals(expected, replaceSet.replaceInline(from));

        from = "Johnny is working early and late behind the iconic desk.";
        expected = "Johnny is working early and late behind the iconic desk.";
        assertEquals(expected, replaceSet.replaceInline(from));
    }

    @Test
    public void testReplaceInlinewithlinenumbering() {
        ReplaceSet replaceSet = new ReplaceSet("late", "early", false, true,10,100);
        String from = "Johnny is behind the iconic late-night desk.";
        String expected = "Johnny is behind the iconic early-night desk.";
        // unchanged as line number is not in the range
        assertEquals(from, replaceSet.replaceInline(from,5));
        // changed as line number is in the range
        assertEquals(expected, replaceSet.replaceInline(from,15));
    }

    @Test
    public void testReplaceinLineWithLinenumberingLastReplaceSetHasZeroAsUntilSourceLine() {
        ReplaceSet replaceSet = new ReplaceSet("late", "early", false, true,10,0);
        String from = "Johnny is behind the iconic late-night desk.";
        String expected = "Johnny is behind the iconic early-night desk.";
        // unchanged as line number is not in the range
        assertEquals(from, replaceSet.replaceInline(from,5));
        // changed as line number is in the range
        assertEquals(expected, replaceSet.replaceInline(from,15));
    }
}
