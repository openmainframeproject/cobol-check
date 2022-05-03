package org.openmainframeproject.cobolcheck.exceptions;

public class ExpectedConfigSettingNotFoundException extends RuntimeException {
    public ExpectedConfigSettingNotFoundException(String message) {
        super(message);
    }
}
