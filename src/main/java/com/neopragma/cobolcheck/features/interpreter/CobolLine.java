package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.List;
import java.util.Locale;

public class CobolLine {

    private String originalString;

    private String trimmedString;
    private List<String> tokens;

    public CobolLine(String line, TokenExtractor tokenExtractor){
        originalString = line;
        trimmedString = line.trim();
        tokens = tokenExtractor.extractTokensFrom(line);
    }

    String getOriginalString() {
        return originalString;
    }
    public String getTrimmedString() { return trimmedString; }
    List<String> getTokens() {
        return tokens;
    }
    String getToken(int index) { return tokens.get(index); }

    int tokensSize() { return tokens.size(); }

    /**
     * Checks if this line contains the specified string - not case-sensitive. The string
     * has to be a single token.
     *
     * @param tokenValue - The string token, to look for.
     *
     * @return (boolean) true if this line contains the token
     */
    boolean containsToken(String tokenValue) {
        return tokens.size() > 0 && tokens.contains(tokenValue.toUpperCase(Locale.ROOT));
    }
}
