package org.openmainframeproject.cobolcheck.services.cobolLogic;

import java.util.List;

public interface TokenExtractor {
    List<String> extractTokensFrom(String sourceLine);
}
