package org.openmainframeproject.cobolcheck.exceptions;

public class TestSuiteAlreadyExistsException extends RuntimeException {
    public TestSuiteAlreadyExistsException(String message) {
        super(message);
    }
    public TestSuiteAlreadyExistsException(Throwable cause) {
        super(cause);
    }
    public TestSuiteAlreadyExistsException(String message, Throwable cause) {
        super(message, cause);
    }
}