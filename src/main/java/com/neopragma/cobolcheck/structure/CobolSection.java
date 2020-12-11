package com.neopragma.cobolcheck.structure;

import java.util.List;

public interface CobolSection {
    String getName();
    boolean isMandatory();
    List<CobolClause> getClauses();
    List<CobolParagraph> getParagraphs();
    default String getArea() { return "A"; }
}
