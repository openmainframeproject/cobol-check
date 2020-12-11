package com.neopragma.cobolcheck.structure;

import com.neopragma.cobolcheck.structure.CobolParagraph;

public class ProgramIdParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "PROGRAM-ID";
    }

    @Override
    public boolean isMandatory() {
        return true;
    }
}
