package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ReplaceTest {

    @Test
    public void test_General_setup() {
        // test we can inspect multible times and still have the correct state
        // and number of replace sets

        Replace.inspectProgram(new File("./testfiles/replace.cbl"));
        assertTrue(Replace.isReplaceOn());
        assertEquals(5, Replace.getReplaceSetsSize());

        Replace.inspectProgram(new File("./testfiles/subprog-after.cbl"));
        assertFalse(Replace.isReplaceOn());
        assertEquals(0, Replace.getReplaceSetsSize());

        Replace.inspectProgram(new File("./testfiles/replace.cbl"));
        assertTrue(Replace.isReplaceOn());
        assertEquals(5, Replace.getReplaceSetsSize());
        assertEquals("In Genesys, the lead singer is Phil",Replace.replace("In Genesys, the lead singer is Peter"));
    }

    @Test
    public void test_replace() {
        Replace.inspectProgram(new File("./testfiles/replace.cbl"));
        assertEquals("In Genesys, the lead singer is Phil",Replace.replace("In Genesys, the lead singer is Peter"));
        assertEquals("            MOVE 'CAITLIN' TO WS-OMEGA",Replace.replace("            MOVE 'BRUCE' TO WS-OMEGA"));
        assertEquals("            MOVE 'SOFT' TO WS-OMEGA",Replace.replace("            MOVE 'SOFT' TO WS-OMEGA"));
        // When lines are comments, they should not be replaced
        assertEquals(" *          MOVE 'BRUCE' TO WS-OMEGA",Replace.replace(" *          MOVE 'BRUCE' TO WS-OMEGA"));
        assertEquals("  *         MOVE 'SOFT' TO WS-OMEGA",Replace.replace("  *         MOVE 'SOFT' TO WS-OMEGA"));

    }

    @Test
    public void test_replace_leading() {
        Replace.inspectProgram(new File("./testfiles/replace.cbl"));
        assertEquals("      *        USING WS-NOT-USED WS-OMEGA", Replace.replace("      *        USING WS-NOT-USED WS-OMEGA"));
        assertEquals("        MOVE 'B' TO UT-EXPECTED",Replace.replace("        MOVE 'B' TO :WS:-EXPECTED"));
        assertEquals("        MOVE 'Y' TO UT-OMEGA",Replace.replace("        MOVE 'Y' TO :WS:-OMEGA"));
        assertEquals("        MOVE 'Y' TO WS-:WS:",Replace.replace("        MOVE 'Y' TO WS-:WS:"));
    }

    @Test
    public void test_replace_trailing() {
        Replace.inspectProgram(new File("./testfiles/replace.cbl"));
        assertEquals("      *        USING WS-ALPHA WS-OMEGA", Replace.replace("      *        USING WS-ALPHA WS-OMEGA"));
        assertEquals("        MOVE 'B' TO UT-EXPECTED",Replace.replace("        MOVE 'B' TO UT-EXPECTED"));
        assertEquals("        MOVE 'Y' TO UT-GAMMA",Replace.replace("        MOVE 'Y' TO UT-ALPHA"));
        assertEquals("        MOVE 'Y' TO WS-EXPECTED",Replace.replace("        MOVE 'Y' TO WS-ACTUAL"));
    }





}