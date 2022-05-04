package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import java.util.List;

/**
 * This class encapsulates information about a Cobol Check keyword.
 *
 * value = the value of the keyword as a string (note that some keywords have embedded spaces, like "TO BE")
 * validNextKeys = list of keys in Keywords for tokens that may follow the current keyword in the test suite.
 * keywordAction = special handling for this keyword, if any.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Keyword {
    private final String value;
    private final List<String> validNextKeys;
    private final KeywordAction keywordAction;

    public Keyword(String value,
                   List<String> validNextKeys,
                   KeywordAction keywordAction) {
        this.value = value;
        this.validNextKeys = validNextKeys;
        this.keywordAction = keywordAction;
    }

    public String value() {
        return value;
    }

    public List<String> getvalidNextKeys() {
        return validNextKeys;
    }

    public KeywordAction keywordAction() {
        return keywordAction;
    }
}
