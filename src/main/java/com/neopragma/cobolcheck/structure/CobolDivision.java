package com.neopragma.cobolcheck.structure;

import java.util.List;

public interface CobolDivision {
    String getName();
    List<String> getAlternateNames();
    List<CobolSection> getSections();
    List<CobolParagraph> getParagraphs();
    default String getArea() { return "A"; }
}
