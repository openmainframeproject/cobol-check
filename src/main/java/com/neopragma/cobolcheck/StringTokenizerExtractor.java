/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.util.*;

/**
 * This class uses StringTokenizer to find tokens in the Cobol source line.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class StringTokenizerExtractor implements TokenExtractor, Constants {

    public StringTokenizerExtractor(Messages messages) {
        this.messages = messages;
    }

    private final Messages messages;
    private static final String delimiters = String.format(" .%s", Constants.NEWLINE);

    // Couldn't use Map.of because it has a limitation of 10 entries. (Dec 2020)
    private static final Map<String, String> expectedTokens = new HashMap();
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
            throw new PossibleInternalLogicErrorException(
                 messages.get("ERR001",
                         "sourceLine",
                         "StringTokenizerExtractor.extractTokensFrom(sourceLine)")
            );
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
            if (!expectedNext.equals(EMPTY_STRING)) {
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
