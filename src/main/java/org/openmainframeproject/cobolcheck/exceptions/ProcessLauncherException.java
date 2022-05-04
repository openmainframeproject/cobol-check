package org.openmainframeproject.cobolcheck.exceptions;

public class ProcessLauncherException extends RuntimeException {
    public ProcessLauncherException(String message) {
        super(message);
    }
    public ProcessLauncherException(Throwable cause) {
        super(cause);
    }
    public ProcessLauncherException(String message, Throwable cause) {
        super(message, cause);
    }
}
