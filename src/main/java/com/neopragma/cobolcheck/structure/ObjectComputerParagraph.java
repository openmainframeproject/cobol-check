package com.neopragma.cobolcheck.structure;

import com.neopragma.cobolcheck.structure.CobolParagraph;

public class ObjectComputerParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "OBJECT-COMPUTER";
    }

    @Override
    public boolean isMandatory() {
        return false;
    }
}
