package com.neopragma.cobolcheck.exceptions;

public class IOExceptionProcessingConfigFile extends RuntimeException {
    public IOExceptionProcessingConfigFile(String filename) {
        super(filename);
    }
}
