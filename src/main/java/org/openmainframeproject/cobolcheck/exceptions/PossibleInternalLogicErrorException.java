package org.openmainframeproject.cobolcheck.exceptions;

public class PossibleInternalLogicErrorException extends RuntimeException {
    public PossibleInternalLogicErrorException(String message) {
        super(message);
    }
    public PossibleInternalLogicErrorException(Throwable cause) {
        super(cause);
    }
    public PossibleInternalLogicErrorException(String message, Throwable cause) {
        super(message, cause);
    }
}
