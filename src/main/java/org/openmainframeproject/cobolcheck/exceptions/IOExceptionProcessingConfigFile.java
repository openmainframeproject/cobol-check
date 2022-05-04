package org.openmainframeproject.cobolcheck.exceptions;

public class IOExceptionProcessingConfigFile extends RuntimeException {
    public IOExceptionProcessingConfigFile(String filename) {
        super(filename);
    }
    public IOExceptionProcessingConfigFile(Throwable ex) { super(ex); }
    public IOExceptionProcessingConfigFile(String filename, Throwable ex) { super(filename, ex); }
}
