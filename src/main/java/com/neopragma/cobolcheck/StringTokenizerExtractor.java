package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.NullSourceLinePassedToTokenExtractorException;

import java.util.*;

/**
 * This class uses StringTokenizer to find tokens in the Cobol source line.
 *
 * @author Dave Nicolette (neopragma)
 * @since 1.4
 */
public class StringTokenizerExtractor implements TokenExtractor, Constants {

    private static final String delimiters = String.format(" .%s", Constants.NEWLINE);

    // Couldn't use Map.of because it has a limitation of 10 entries. (Dec 2020)
    private static Map<String, String> expectedTokens = new HashMap();
    static {
        expectedTokens.put("PROCEDURE", "DIVISION");
        expectedTokens.put("DATA", "DIVISION");
        expectedTokens.put("ENVIRONMENT", "DIVISION");
        expectedTokens.put("IDENTIFICATION", "DIVISION");
        expectedTokens.put("ID", "DIVISION");
        expectedTokens.put("CONFIGURATION", "SECTION");
        expectedTokens.put("INPUT-OUTPUT", "SECTION");
        expectedTokens.put("LINKAGE", "SECTION");
        expectedTokens.put("FILE", "SECTION");
        expectedTokens.put("WORKING-STORAGE", "SECTION");
        expectedTokens.put("LOCAL-STORAGE", "SECTION");
    }

    /**
     * Extracts tokens meaningful for processing Cobol source code from a Cobol source line.
     *
     * Such "meaningful" tokens may comprise more than one "plain" token,
     * such as "PROCEDURE DIVISION" or "INPUT-OUTPUT SECTION" - treated as a single token here.
     *
     * Commented lines are ignored.
     *
     * @param sourceLine (String) - passed by a component that reads Cobol source files, assumed to be upper case.
     * @return list of tokens (List&lt;String&gt;)
     */
    @Override
    public List<String> extractTokensFrom(String sourceLine) {
        if (sourceLine == null) {
            throw new NullSourceLinePassedToTokenExtractorException();
        }
        List<String> tokens = new ArrayList();
        String expectedNext = EMPTY_STRING;
        String saved = EMPTY_STRING;
        StringTokenizer tokenizer = new StringTokenizer(sourceLine, delimiters);
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            if (token.startsWith(COMMENT_INDICATOR)) {
                break;
            }
            if (expectedNext != EMPTY_STRING) {
                if (token.equals(expectedNext)) {
                    token = saved + " " + token;
                    expectedNext = EMPTY_STRING;
                    saved = EMPTY_STRING;
                }
            }
            if (expectedTokens.containsKey(token)) {
                expectedNext = expectedTokens.get(token);
                saved = token;
            } else {
                tokens.add(token);
            }
        }
        return tokens;
    }
}
