package com.neopragma.cobolcheck.services.cobolLogic;

import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.Collection;
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

    public String getOriginalString() {
        return originalString;
    }
    public String getTrimmedString() { return trimmedString; }
    public List<String> getTokens() {
        return tokens;
    }
    public String getToken(int index) { return tokens.get(index); }

    public int tokensSize() { return tokens.size(); }

    /**
     * Checks if this line contains the specified string - not case-sensitive. The string
     * has to be a single token.
     *
     * @param tokenValue - The string token, to look for.
     *
     * @return (boolean) true if this line contains the token
     */
    public boolean containsToken(String tokenValue) {
        return tokens.size() > 0 && tokens.contains(tokenValue.toUpperCase(Locale.ROOT));
    }

    /**
     * Checks if this line contains all the specified strings - not case-sensitive. Each string
     * has to be a single token.
     *
     * @param tokenValues - The string tokens, to look for.
     *
     * @return (boolean) true if this line contains all the given token values
     */
    public boolean containsAllTokens(Collection<String> tokenValues) {
        for (String token : tokenValues){
            if (!containsToken(token))
                return false;
        }
        return true;
    }

    /**
     * Checks if this line contains all the specified strings - not case-sensitive. Each string
     * has to be a single token.
     *
     * @param tokenValues - The string tokens, to look for.
     *
     * @return (boolean) true if this line contains all the given token values
     */
    public boolean containsAllTokensInConsecutiveOrder(Collection<String> tokenValues) {
        int tokenIndex = 0;
        int lastTokenIndex = 0;
        for (String token : tokenValues){
            tokenIndex = getTokenIndexOf(token);
            if (tokenIndex != 0 && tokenIndex != lastTokenIndex + 1)
                return false;
            lastTokenIndex = tokenIndex;
        }
        return true;
    }

    /**
     * Looks through the list of tokens for the specified string and finds the index. - not case-sensitive. The string
     * has to be a single token.
     *
     * @param tokenValue - The string token, to look for. Not case-sensitive. Has to be a single token.
     *
     * @return (int) index of the string. If the string is not found, -1 is returned.
     */
    public int getTokenIndexOf(String tokenValue) {
        for (int i = 0; i < tokens.size(); i++){
            if (tokens.get(i).equals(tokenValue.toUpperCase(Locale.ROOT)))
                return i;
        }
        return -1;
    }
}
