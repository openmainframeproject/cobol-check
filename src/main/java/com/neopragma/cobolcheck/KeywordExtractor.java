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

import java.util.*;

public class KeywordExtractor implements TokenExtractor {

    private Map<String, List<String>> multiWordTokens;
    private StringBuilder buffer;
    private final char PERIOD = '.';
    private final char COMMA = ',';
    private final char DOUBLE_QUOTE = '"';
    private final char SINGLE_QUOTE = '\'';
    private final char SPACE = ' ';
    private List<String> nextExpectedTokens;
    private boolean openQuote = false;
    private char quoteDelimiter = '"';
    private boolean processingNumericLiteral = false;

    public KeywordExtractor() {
        nextExpectedTokens = new ArrayList<>();
        multiWordTokens = new HashMap<>();
        multiWordTokens.put("TO", Arrays.asList("BE", "EQUAL"));
    }

    @Override
    public List<String> extractTokensFrom(String sourceLine) {
        List<String> tokens = new ArrayList<>();
        buffer = new StringBuilder();
        int tokenOffset = 0;
        sourceLine = sourceLine.trim();
        while (tokenOffset < sourceLine.length()) {
            char currentCharacter = sourceLine.charAt(tokenOffset);
            if (isQuote(currentCharacter)) {
                if (openQuote) {
                    if (currentCharacter == quoteDelimiter) {
                        openQuote = false;
                        buffer.append(currentCharacter);
                        buffer = addTokenAndClearBuffer(buffer, tokens);
                    } else {
                        buffer.append(currentCharacter);
                    }
                } else {
                    openQuote = true;
                    quoteDelimiter = currentCharacter;
                    buffer.append(currentCharacter);
                }
            } else {

                if (processingNumericLiteral) {
                    if (!isDecimalPoint(buffer, currentCharacter, sourceLine, tokenOffset) 
                        && !Character.isDigit(currentCharacter)) {
                            processingNumericLiteral = false;      // prevent next period from being interpreted as end of sentence
                    }
                } else {
                    if (currentCharacter == PERIOD) {          // Cobol end of sentence
                        break;                                 // skip it
                    }
                    if (startNumericLiteral(buffer, currentCharacter)) {
                        processingNumericLiteral = true;
                    }
                }

                if (currentCharacter == SPACE) {
                    processingNumericLiteral = false;
                    if (openQuote) {
                        buffer.append(SPACE);
                    } else {
                        if (multiWordTokens.containsKey(buffer.toString().toUpperCase(Locale.ROOT))) {
                            nextExpectedTokens = multiWordTokens.get(buffer.toString().toUpperCase(Locale.ROOT));
                            buffer.append(SPACE);

                            int startOfLookahead = tokenOffset + 1;
                            for (String expectedToken : nextExpectedTokens) {
                                int endOfLookahead = startOfLookahead + expectedToken.length();
                                if (sourceLine.length() >= endOfLookahead) {
                                    if (expectedToken.equalsIgnoreCase(sourceLine.substring(startOfLookahead, endOfLookahead))
                                            && (endOfLookahead == sourceLine.length()
                                            || sourceLine.charAt(endOfLookahead) == SPACE)) {
                                        buffer.append(expectedToken);
                                        tokenOffset += expectedToken.length();
                                        break;
                                    }
                                }

                            }
                            buffer = addTokenAndClearBuffer(buffer, tokens);
                            nextExpectedTokens = new ArrayList<>();
                        } else {
                            nextExpectedTokens = new ArrayList<>();
                            if (buffer.length() > 0) {
                                buffer = addTokenAndClearBuffer(buffer, tokens);
                            }
                        }
                    }
                } else{
                    buffer.append(currentCharacter);
                }
            }
            tokenOffset += 1;
        }
        if (buffer.length() > 0) {
            buffer = addTokenAndClearBuffer(buffer, tokens);
        }
        return tokens;
    }


    /**
     * Quoted strings may be enclosed in apostrophes (single quotes) or quotation marks (double quotes).
     */
    private boolean isQuote(char character) {
        return character == DOUBLE_QUOTE || character == SINGLE_QUOTE;
    }

    /**
     * The start of a numeric literal is recognized when the previous character was a space and the current
     * character is a numerical digit. We care about this so we can distinguish between a period as a decimal point
     * and a period as a Cobol statement delimiter.
     */
    private boolean startNumericLiteral(StringBuilder buffer, char currentCharacter) {
        return ( getPreviousCharacterFromBuffer(buffer) == SPACE
                && ( Character.isDigit(currentCharacter)
                    || currentCharacter == '-' || currentCharacter == '+' || currentCharacter == '$') );
    }

    /**
     * Decimal point is recognized when a period or comma appears between two numeric digits.
     */
    private boolean isDecimalPoint(StringBuilder buffer, char currentCharacter, String sourceLine, int tokenOffset) {
        if (currentCharacter != PERIOD && currentCharacter != COMMA) {
            return false;
        }
        int lookahead = tokenOffset + 1;
        if (lookahead >= sourceLine.length()) {
            return false; // no digit after this character
        }
        return (Character.isDigit(getPreviousCharacterFromBuffer(buffer)) && Character.isDigit(sourceLine.charAt(lookahead)));
    }

    private char getPreviousCharacterFromBuffer(StringBuilder buffer) {
        char previousCharacter = SPACE;
        if (buffer.length() > 1) {
            previousCharacter = buffer.charAt(buffer.length()-1);
        }
        return previousCharacter;
    }

    private StringBuilder addTokenAndClearBuffer(StringBuilder buffer, List<String> tokens) {
        // this "if" is for a single case: a numeric literal is the last thing on a line and it's followed by a period.
        if (buffer.charAt(buffer.length()-1) == PERIOD) {
            buffer = new StringBuilder(buffer.substring(0, buffer.length()-1));
        }
        tokens.add(buffer.toString().trim());
        return new StringBuilder();
    }
}
