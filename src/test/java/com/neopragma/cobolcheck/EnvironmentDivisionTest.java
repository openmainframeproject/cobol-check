package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.structure.ConfigurationSection;
import com.neopragma.cobolcheck.structure.EnvironmentDivision;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class EnvironmentDivisionTest {

    private EnvironmentDivision division = new EnvironmentDivision();

    @Test
    public void division_knows_its_name() {
        assertEquals("ENVIRONMENT DIVISION", division.getName());
    }

    @Test
    public void division_has_no_alternate_name() {
        List<String> alternateNames = division.getAlternateNames();
        assertEquals(0, alternateNames.size());
    }

    @Test
    public void division_knows_its_area() {
        assertEquals("A", division.getArea());
    }

    @Test
    public void division_knows_its_subordinate_sections() {
        assertEquals(2, division.getSections().size());
        assertEquals(ConfigurationSection.class, division.getSections().get(0).getClass());
//        assertEquals(InputOutputSection.class, division.getSections().get(0).getClass());
    }

    @Test
    public void division_has_no_subordinate_paragraphs() {
        assertEquals(0, division.getParagraphs().size());
    }

    @Test
    public void division_has_no_mandatory_sections() {
        assertEquals(0, division.getSections().size());
    }
}
