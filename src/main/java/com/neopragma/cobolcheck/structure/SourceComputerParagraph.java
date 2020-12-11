package com.neopragma.cobolcheck.structure;

import com.neopragma.cobolcheck.structure.CobolParagraph;

public class SourceComputerParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "SOURCE-COMPUTER";
    }

    @Override
    public boolean isMandatory() {
        return false;
    }
}
