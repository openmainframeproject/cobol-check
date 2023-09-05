package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Constants;

/**
 * Keeps track of properties for Tokens during the parsing
 */
public class TokenTracker {

    String summizedTokensForArgument = Constants.EMPTY_STRING;
    String parseDataUsingForSummizedTokens = Constants.EMPTY_STRING;
    boolean lastAddedTokenToArgumentIsQualifier = false;

    /**
     * Resets the strings in the TokenTracker
     */
    public void reset() {
        this.summizedTokensForArgument = Constants.EMPTY_STRING;
        this.parseDataUsingForSummizedTokens = Constants.EMPTY_STRING;
    }
}
