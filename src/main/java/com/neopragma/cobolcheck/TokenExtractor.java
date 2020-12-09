package com.neopragma.cobolcheck;

import java.util.List;

public interface TokenExtractor {
    List<String> extractTokensFrom(String sourceLine);
}
