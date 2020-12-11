package com.neopragma.cobolcheck.structure;

public class AuthorParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "AUTHOR";
    }

    @Override
    public boolean isMandatory() {
        return false;
    }
}
