package com.neopragma.cobolcheck.exceptions;

public class IOExceptionProcessingTestResultFile extends RuntimeException{
    public IOExceptionProcessingTestResultFile(String filename) {
        super(filename);
    }
    public IOExceptionProcessingTestResultFile(Throwable ex) { super(ex); }
    public IOExceptionProcessingTestResultFile(String filename, Throwable ex) { super(filename, ex); }
}
