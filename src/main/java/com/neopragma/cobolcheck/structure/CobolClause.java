package com.neopragma.cobolcheck.structure;

public interface CobolClause {
    String getName();
    boolean isMandatory();
    default String getArea() { return "A"; }
}
