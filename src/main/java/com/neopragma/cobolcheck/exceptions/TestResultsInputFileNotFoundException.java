package com.neopragma.cobolcheck.exceptions;

public class TestResultsInputFileNotFoundException extends RuntimeException{
    public TestResultsInputFileNotFoundException(String message) {
        super(message);
    }
    public TestResultsInputFileNotFoundException(Throwable cause) {
        super(cause);
    }
    public TestResultsInputFileNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
