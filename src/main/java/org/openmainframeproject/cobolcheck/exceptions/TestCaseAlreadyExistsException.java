package org.openmainframeproject.cobolcheck.exceptions;

public class TestCaseAlreadyExistsException extends RuntimeException {
    public TestCaseAlreadyExistsException(String message) {
        super(message);
    }
    public TestCaseAlreadyExistsException(Throwable cause) {
        super(cause);
    }
    public TestCaseAlreadyExistsException(String message, Throwable cause) {
        super(message, cause);
    }
}
