package com.neopragma.cobolcheck.structure;

public class InstallationParagraph implements CobolParagraph {

    @Override
    public String getName() {
        return "INSTALLATION";
    }

    @Override
    public boolean isMandatory() {
        return false;
    }
}
