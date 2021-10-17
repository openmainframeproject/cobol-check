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
package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.*;

/**
 * This class uses StringTokenizer to find tokens in the Cobol source line. It is used by the Generator
 * when processing the source for the program under test.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class StringTokenizerExtractor implements TokenExtractor {

    public StringTokenizerExtractor() {

    }

    private static final String delimiters = String.format(" .%s", Constants.NEWLINE);

    // Some "logical tokens" meaningful in Cobol source consist of two words separated by whitespace.
    // Couldn't use Map.of because it has a limitation of 10 entries. (Dec 2020)
    private static final Map<String, List<String>> expectedTokens = new HashMap<>();
    static {
        expectedTokens.put("PROCEDURE", Arrays.asList("DIVISION"));
        expectedTokens.put("DATA", Arrays.asList("DIVISION"));
        expectedTokens.put("ENVIRONMENT", Arrays.asList("DIVISION"));
        expectedTokens.put("IDENTIFICATION", Arrays.asList("DIVISION"));
        expectedTokens.put("FILE", Arrays.asList("SECTION", "CONTROL", "STATUS"));
        expectedTokens.put("ID", Arrays.asList("DIVISION"));
        expectedTokens.put("CONFIGURATION", Arrays.asList("SECTION"));
        expectedTokens.put("INPUT-OUTPUT", Arrays.asList("SECTION"));
        expectedTokens.put("LINKAGE", Arrays.asList("SECTION"));
        expectedTokens.put("WORKING-STORAGE", Arrays.asList("SECTION"));
        expectedTokens.put("LOCAL-STORAGE", Arrays.asList("SECTION"));
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
                 Messages.get("ERR001",
                         "sourceLine",
                         "StringTokenizerExtractor.extractTokensFrom(sourceLine)")
            );
        }
        List<String> tokens = new ArrayList<>();
        List<String> expectedNext = new ArrayList<>();
        String saved = Constants.EMPTY_STRING;
        StringTokenizer tokenizer = new StringTokenizer(sourceLine, delimiters);
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            if (token.startsWith(Constants.COMMENT_INDICATOR)) {
                break;
            }
            if (!expectedNext.isEmpty()) {
                for (String expectedValue : expectedNext) {
                    if (token.equals(expectedValue)) {
                        token = saved + " " + token;
                        expectedNext = new ArrayList<>();
                        saved = Constants.EMPTY_STRING;
                    }
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
