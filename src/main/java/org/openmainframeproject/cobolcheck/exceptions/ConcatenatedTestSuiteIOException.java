package org.openmainframeproject.cobolcheck.exceptions;

public class ConcatenatedTestSuiteIOException extends RuntimeException {
    public ConcatenatedTestSuiteIOException(String message) {
        super(message);
    }
    public ConcatenatedTestSuiteIOException(Throwable cause) {
        super(cause);
    }
    public ConcatenatedTestSuiteIOException(String message, Throwable cause) {
        super(message, cause);
    }
}
