package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openmainframeproject.cobolcheck.services.Config;

import java.io.File;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ReplaceTest {

    @BeforeAll
    public static void setup(){
        Config.load();
    }


    @Test
    public void test_General_setup() {
        // test we can inspect multiple times and still have the correct state
        // and number of replace sets

        Replace.inspectProgram(new File("./testfiles/REPLACE.CBL"));
        assertTrue(Replace.isReplaceOn());
        assertEquals(5, Replace.getReplaceSetsSize());

        Replace.inspectProgram(new File("./testfiles/SUBPROG-AFTER.CBL"));
        assertFalse(Replace.isReplaceOn());
        assertEquals(0, Replace.getReplaceSetsSize());

        Replace.inspectProgram(new File("./testfiles/REPLACE.CBL"));
        assertTrue(Replace.isReplaceOn());
        assertEquals(5, Replace.getReplaceSetsSize());
        assertEquals("In Genesys, the lead singer is Phil",Replace.replace("In Genesys, the lead singer is Peter", 62));
    }

    @Test
    public void test_replace() {
        Replace.inspectProgram(new File("./testfiles/REPLACE.CBL"));
        // line    replaces in file:
        // Replace.showReplaceSets();
        // 23 REPLACE TRAILING ==ACTUAL== BY ==EXPECTED==.
        // 42 REPLACE TRAILING ==ALPHA== BY ==GAMMA==.
        // 52 REPLACE LEADING ==:WS:== BY ==UT==.
        // 61 REPLACE ==Bruce== BY ==CAITLIN== ==PETER== BY ==Phil==.
        assertEquals("In Genesys, the lead singer is Peter",Replace.replace("In Genesys, the lead singer is Peter", 50));
        assertEquals("In Genesys, the lead singer is Phil",Replace.replace("In Genesys, the lead singer is Peter", 64));
        assertEquals("            MOVE 'CAITLIN' TO WS-OMEGA",Replace.replace("            MOVE 'BRUCE' TO WS-OMEGA", 65));
        assertEquals("            MOVE 'SOFT' TO WS-OMEGA",Replace.replace("            MOVE 'SOFT' TO WS-OMEGA", 310));
        // When lines are comments, they should not be replaced
        assertEquals(" *          MOVE 'BRUCE' TO WS-OMEGA",Replace.replace(" *          MOVE 'BRUCE' TO WS-OMEGA", 65));
        assertEquals("  *         MOVE 'SOFT' TO WS-OMEGA",Replace.replace("  *         MOVE 'SOFT' TO WS-OMEGA", 310));

    }

    @Test
    public void test_replace_leading() {
        Replace.inspectProgram(new File("./testfiles/REPLACE.CBL"));
        assertEquals("      *        USING WS-NOT-USED WS-OMEGA", Replace.replace("      *        USING WS-NOT-USED WS-OMEGA", 57));
        assertEquals("        MOVE 'B' TO UT-EXPECTED",Replace.replace("        MOVE 'B' TO :WS:-EXPECTED", 58));
        assertEquals("        MOVE 'Y' TO UT-OMEGA",Replace.replace("        MOVE 'Y' TO :WS:-OMEGA", 59));
        assertEquals("        MOVE 'Y' TO WS-:WS:",Replace.replace("        MOVE 'Y' TO WS-:WS:", 69));
    }

    @Test
    public void test_replace_trailing() {
        Replace.inspectProgram(new File("./testfiles/REPLACE.CBL"));
        assertEquals("      *        USING WS-ALPHA WS-OMEGA", Replace.replace("      *        USING WS-ALPHA WS-OMEGA", 43));
        assertEquals("        MOVE 'B' TO UT-EXPECTED",Replace.replace("        MOVE 'B' TO UT-EXPECTED", 32));
        assertEquals("        MOVE 'Y' TO UT-GAMMA",Replace.replace("        MOVE 'Y' TO UT-ALPHA", 46));
        assertEquals("        MOVE 'Y' TO WS-EXPECTED",Replace.replace("        MOVE 'Y' TO WS-ACTUAL", 27));
    }


    @Test
    public void test_replace_FI01_only_when_the_line_is_correct() {
        Replace.inspectProgram(new File("./testfiles/REPLACE2.CBL"));
        Replace.showReplaceSets();
        assertEquals("               PERFORM INC-:XXXX:-SKRIV", Replace.replace("               PERFORM INC-:XXXX:-SKRIV", 69));
        assertEquals("       INC-FI01-SKRIV SECTION.", Replace.replace("       INC-:XXXX:-SKRIV SECTION.", 72));
    }

    @Test
    public void test_for_linenumber_0_every_replace_is_performed() {
        Replace.inspectProgram(new File("./testfiles/REPLACE.CBL"));
        assertEquals("        MOVE 'B' TO UT-EXPECTED",Replace.replace("        MOVE 'B' TO :WS:-EXPECTED", 0));
        assertEquals("        MOVE 'Y' TO UT-OMEGA",Replace.replace("        MOVE 'Y' TO :WS:-OMEGA", 0));
        assertEquals("        MOVE 'Y' TO WS-EXPECTED",Replace.replace("        MOVE 'Y' TO WS-ACTUAL", 0));
    }

    @Test
    public void test_what_happends_when_we_have_a_big_replace() {
        Replace.inspectProgram(new File("./testfiles/REPLACE3.CBL"));
        assertEquals("        MOVE 'B' TO REPDEMO3-PARAM",Replace.replace("        MOVE 'B' TO :PROGRAM:-PARAM", 0));
        assertEquals("123456     MOVE MY-DATA in ADVANCED-REQUEST-DATA TO UPDATE-MY-ADVANCED-REQUEST-DATA",
                Replace.replace("123456     MOVE MY-DATA in ADVANCED-REQUEST-DATA TO :REQUEST:-DATA", 0));

    }

    @Test
    public void test_output_file_name_is_set_to_path_for_test_elements() {
        //Config.getGeneratedTestCodePath() is where the modified files are written
        // regardless of where the input file is located

        String inputFileName = ".\\testfiles\\REPLACE.CBL";
        String expectedOutputFileName = Config.getGeneratedTestCodePath() + "\\REPLACE.replaced.CBL";
        assertEquals(expectedOutputFileName, Replace.getOutputFileName(inputFileName));

        inputFileName = ".\\test\\files\\REPLACE.CBL";
        expectedOutputFileName = Config.getGeneratedTestCodePath() + "\\REPLACE.replaced.CBL";
        assertEquals(expectedOutputFileName, Replace.getOutputFileName(inputFileName));

        inputFileName = ".\\test\\files\\REPLACE.xxx";
        expectedOutputFileName = Config.getGeneratedTestCodePath() + "\\REPLACE.replaced.xxx";
        assertEquals(expectedOutputFileName, Replace.getOutputFileName(inputFileName));
    }

    // test internal functions for filename manipulation
    @Test
    public void test_internal_functions_for_filename_manipulation() {
        String inputName = "REPLACE.CBL";
        assertEquals("REPLACE.CBL", Replace.getFilenameWithoutPath(inputName));
        assertEquals("CBL", Replace.getFileExtension(inputName));
        assertEquals("REPLACE", Replace.getFileNameWithoutExtension(inputName));

        // Default extension if none found
        inputName = "REPLACE";
        assertEquals("CBL", Replace.getFileExtension(inputName));
    }

    @Test
    public void test_internal_functions_for_filename_manipulation_path() {
        String input = ".\\test\\files\\REPLACE.CBL";
        assertEquals("REPLACE.CBL", Replace.getFilenameWithoutPath(input));
    }





}