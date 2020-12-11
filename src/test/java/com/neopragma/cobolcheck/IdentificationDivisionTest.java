package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.structure.*;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class IdentificationDivisionTest {

    private IdentificationDivision division = new IdentificationDivision();

    @Test
    public void division_knows_its_name() {
        assertEquals("IDENTIFICATION DIVISION", division.getName());
    }

    @Test
    public void division_knows_its_alternate_name() {
        List<String> alternateNames = division.getAlternateNames();
        assertEquals(1, alternateNames.size());
        assertEquals("ID DIVISION", alternateNames.get(0));
    }

    @Test
    public void division_knows_its_area() {
        assertEquals("A", division.getArea());
    }

    @Test
    public void division_has_no_subordinate_sections() {
        assertEquals(0, division.getSections().size());
    }

    @Test
    public void division_has_4_subordinate_paragraphs_in_a_specified_order() {
        assertEquals(4, division.getParagraphs().size());
        assertEquals(ProgramIdParagraph.class, division.getParagraphs().get(0).getClass());
        assertEquals(AuthorParagraph.class, division.getParagraphs().get(1).getClass());
        assertEquals(InstallationParagraph.class, division.getParagraphs().get(2).getClass());
        assertEquals(DateWrittenParagraph.class, division.getParagraphs().get(3).getClass());
    }

    @Test
    public void division_knows_which_paragraphs_are_mandatory() {
        assertEquals(4, division.getParagraphs().size());
        assertTrue(division.getParagraphs().get(0).isMandatory());
        assertFalse(division.getParagraphs().get(1).isMandatory());
        assertFalse(division.getParagraphs().get(2).isMandatory());
        assertFalse(division.getParagraphs().get(3).isMandatory());
    }
}
