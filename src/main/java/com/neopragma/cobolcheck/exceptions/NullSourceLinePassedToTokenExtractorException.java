package com.neopragma.cobolcheck.exceptions;

public class NullSourceLinePassedToTokenExtractorException extends RuntimeException {

    @Override
    public String getMessage() {
        return "Caller should not pass null reference in this context. Probable internal logic error.";
    };
}
