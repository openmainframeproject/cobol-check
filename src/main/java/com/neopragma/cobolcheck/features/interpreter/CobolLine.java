package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.List;
import java.util.Locale;

public class CobolLine {

    private String originalString;

    private String trimmedString;
    private String fixedLengthString;
    private List<String> tokens;

    //Line interpretation variables



    TokenExtractor tokenExtractor;

    public CobolLine(String line, TokenExtractor tokenExtractor){
        originalString = line;
        trimmedString = line.trim();
        fixedLengthString = StringHelper.fixedLength(line);
        tokens = tokenExtractor.extractTokensFrom(line);
    }

    String getOriginalString() {
        return originalString;
    }

    public String getTrimmedString() { return trimmedString; }

    String getFixedLengthString() {
        return fixedLengthString;
    }

    List<String> getTokens() {
        return tokens;
    }
    int tokensSize() { return tokens.size(); }
    String getToken(int index) { return tokens.get(index); }

    boolean contains(String tokenValue) {
        return tokens.size() > 0 && tokens.contains(tokenValue.toUpperCase(Locale.ROOT));
    }
}
