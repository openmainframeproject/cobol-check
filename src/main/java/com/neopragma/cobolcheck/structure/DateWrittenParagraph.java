package com.neopragma.cobolcheck.structure;

public class DateWrittenParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "DATE-WRITTEN";
    }

    @Override
    public boolean isMandatory() {
        return false;
    }
}
