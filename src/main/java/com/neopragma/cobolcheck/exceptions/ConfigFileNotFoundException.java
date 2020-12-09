package com.neopragma.cobolcheck.exceptions;

public class ConfigFileNotFoundException extends RuntimeException {
    public ConfigFileNotFoundException(String filename) {
        super(filename);
    }
}
