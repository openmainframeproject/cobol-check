package com.neopragma.cobolcheck.structure;

public class SpecialNamesParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "SPECIAL-NAMES";
    }

    @Override
    public boolean isMandatory() {
        return false;
    }
}
