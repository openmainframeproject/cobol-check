package com.neopragma.cobolcheck.structure;

import java.util.ArrayList;
import java.util.List;

public class EnvironmentDivision implements CobolDivision {

    private static final String name = "ENVIRONMENT DIVISION";
    private final List<String> alternateNames;
    private final List<CobolSection> subordinateSections;
    private final List<CobolParagraph> subordinateParagraphs;

    public EnvironmentDivision() {
        alternateNames = new ArrayList(); // empty list
        subordinateSections = new ArrayList();
        //subordinateSections.add(new ConfigurationSection());
        //subordinateSections.add(new InputOutputSection());
        subordinateParagraphs = new ArrayList(); // empty list
    }

    @Override
    /**
     * @return (String) The name of the division in upper case as it would appear in Cobol source code.
     */
    public String getName() {
        return name;
    }

    @Override
    /**
     * @return (List&lt;String&gt;) Any alternate division names that are valid in Cobol source code.
     */
    public List<String> getAlternateNames() {
        return alternateNames;
    }

    @Override
    /**
     * @return (List&lt;CobolSection&gt;) Sections belonging to this division, in coding order.
     */
    public List<CobolSection> getSections() {
        return subordinateSections;
    }

    @Override
    /**
     * @return (List&lt;CobolParagraph&gt;) Paragraphs belonging directly to this division,
     * not to subordinate sections, in coding order.
     */
    public List<CobolParagraph> getParagraphs() {
        return subordinateParagraphs;
    }
}
