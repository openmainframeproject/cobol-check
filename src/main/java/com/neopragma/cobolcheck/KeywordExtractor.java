package com.neopragma.cobolcheck;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class KeywordExtractor implements TokenExtractor, Constants {

    private Map<String, String> twoWordTokens;
    private StringBuilder buffer;
    private String nextExpectedToken = EMPTY_STRING;

    public KeywordExtractor() {
        twoWordTokens = new HashMap<>();
        twoWordTokens.put("TO", "BE");
    }

    @Override
    public List<String> extractTokensFrom(String sourceLine) {
        List<String> tokens = new ArrayList<>();
        buffer = new StringBuilder();
        int tokenOffset = 0;
        boolean openQuote = false;
        sourceLine = sourceLine.trim();
        while (tokenOffset < sourceLine.length()) {
            if (sourceLine.charAt(tokenOffset) == '"') {
                if (openQuote) {
                    openQuote = false;
                    buffer.append("\"");
                    buffer = addTokenAndClearBuffer(buffer, tokens);
                } else {
                    openQuote = true;
                    buffer.append("\"");
                }
            } else {
                if (sourceLine.charAt(tokenOffset) == ' ') {
                    if (openQuote) {
                        buffer.append(SPACE);
                    } else {
                        if (nextExpectedToken.length() > 0) {
                            buffer.append(SPACE);
                            buffer.append(sourceLine.charAt(tokenOffset));
                            buffer = addTokenAndClearBuffer(buffer, tokens);
                            nextExpectedToken = EMPTY_STRING;
                        } else {
                            if (twoWordTokens.containsKey(buffer.toString())) {
                                nextExpectedToken = twoWordTokens.get(buffer.toString());
                                buffer.append(SPACE);
                            } else {
                                nextExpectedToken = EMPTY_STRING;
                                if (buffer.length() > 0) {
                                    buffer = addTokenAndClearBuffer(buffer, tokens);
                                }
                            }
                        }
                    }
                } else{
                    buffer.append(sourceLine.charAt(tokenOffset));
                }
            }
            tokenOffset += 1;
        }
        if (buffer.length() > 0) {
            buffer = addTokenAndClearBuffer(buffer, tokens);
        }
        return tokens;
    }

    private StringBuilder addTokenAndClearBuffer(StringBuilder buffer, List<String> tokens) {
        tokens.add(buffer.toString().trim());
        return new StringBuilder();
    }
}
