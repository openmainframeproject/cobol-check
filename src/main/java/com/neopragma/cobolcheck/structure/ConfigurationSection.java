package com.neopragma.cobolcheck.structure;

import java.util.ArrayList;
import java.util.List;

public class ConfigurationSection implements CobolSection {

    private static final String name = "CONFIGURATION SECTION";
    private final List<String> alternateNames;
    private final List<CobolClause> subordinateClauses;
    private final List<CobolParagraph> subordinateParagraphs;

    public ConfigurationSection() {
        alternateNames = new ArrayList(); // empty list
        subordinateClauses = new ArrayList();
        subordinateParagraphs = new ArrayList();
//        subordinateParagraphs.add(new SourceComputerParagraph());
//        subordinateParagraphs.add(new ObjectComputerParagraph());
//        subordinateParagraphs.add(new SpecialNamesParagraph());
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public boolean isMandatory() {
        return false;
    }

    @Override
    public List<CobolClause> getClauses() {
        return subordinateClauses;
    }

    @Override
    public List<CobolParagraph> getParagraphs() {
        return subordinateParagraphs;
    }
}
