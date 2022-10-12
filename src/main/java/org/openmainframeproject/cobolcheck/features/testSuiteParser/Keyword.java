package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
    private final Map<String, List<String>> validNextKeysInContext;
    private final KeywordAction keywordAction;

    public Keyword(String value, List<String> validNextKeys, KeywordAction keywordAction) {
        this.value = value;
        this.validNextKeys = validNextKeys;
        this.validNextKeysInContext = null;
        this.keywordAction = keywordAction;
    }

    public Keyword(String value, List<String> validNextKeys, Map<String, List<String>> validNextKeysInContext,
                   KeywordAction keywordAction) {
        this.value = value;
        this.validNextKeys = validNextKeys;
        this.validNextKeysInContext = validNextKeysInContext;
        this.keywordAction = keywordAction;
    }

    public String value() {
        return value;
    }

    public List<String> getvalidNextKeys(String context) {
        if (context == null || context.isEmpty()){
            return validNextKeys;
        }
        else if (validNextKeysInContext == null)
            return new ArrayList<>();
        else {
            if (validNextKeysInContext.containsKey(context))
                return validNextKeysInContext.get(context);
            else
                return new ArrayList<>();
        }
    }

    public KeywordAction keywordAction() {
        return keywordAction;
    }
}
