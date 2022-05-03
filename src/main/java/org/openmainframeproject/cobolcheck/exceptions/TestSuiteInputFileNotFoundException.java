package org.openmainframeproject.cobolcheck.exceptions;

public class TestSuiteInputFileNotFoundException extends RuntimeException {
    public TestSuiteInputFileNotFoundException(String message) {
        super(message);
    }
    public TestSuiteInputFileNotFoundException(Throwable cause) {
        super(cause);
    }
    public TestSuiteInputFileNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
