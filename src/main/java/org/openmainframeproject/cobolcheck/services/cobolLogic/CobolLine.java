package org.openmainframeproject.cobolcheck.services.cobolLogic;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

public class CobolLine {

    private String originalString;
    private String unNumberedString;
    private String trimmedString;
    private List<String> tokens;

    public CobolLine(String line, TokenExtractor tokenExtractor){
        originalString = line;
        unNumberedString = removeSequenceNumberArea(line);
        trimmedString = unNumberedString.trim();
        tokens = tokenExtractor.extractTokensFrom(unNumberedString);
    }

    public String getOriginalString() { return originalString; }
    public String getUnNumberedString() { return unNumberedString; }
    public String getTrimmedString() { return trimmedString; }
    public List<String> getTokens() {
        return tokens;
    }
    public String getToken(int index) { return tokens.get(index); }
    
    public void setUnNumberedString(String s) {unNumberedString = s;}

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

    /**
     * Checks if this line ends with the specified string - not case-sensitive. The string
     * has to be a single token.
     *
     * @param tokenValue - The string token, to look for.
     *
     * @return (boolean) true if this line ends with the token
     */
    public boolean endsWithToken(String tokenValue) {
        return tokens.size() > 0 && tokens.get(tokensSize() - 1) == tokenValue.toUpperCase(Locale.ROOT);
    }

    private String removeSequenceNumberArea(String originalLine){
        int index = Interpreter.getSequenceNumberAreaIndex();
        if (originalLine.length() < index)
            return originalLine;
        else
            return "      " + originalLine.substring(index);
    }
}
