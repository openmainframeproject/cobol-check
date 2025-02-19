package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.junit.jupiter.api.Test;

public class ReplaceTokenTest {
    @Test
    public void it_identifies_the_replace_tokens() {
        ReplaceToken token = new ReplaceToken("REPLACE");
        assert(token.getType() == ReplaceTokenType.REPLACE);

        token = new ReplaceToken("LEADING");
        assert(token.getType() == ReplaceTokenType.LEADING);

        token = new ReplaceToken("TRAILING");
        assert(token.getType() == ReplaceTokenType.TRAILING);

        token = new ReplaceToken("from-value");
        assert(token.getType() == ReplaceTokenType.OTHER);

        token = new ReplaceToken("BY");
        assert(token.getType() == ReplaceTokenType.BY);

        token = new ReplaceToken("TO-value");
        assert(token.getType() == ReplaceTokenType.OTHER);

        token = new ReplaceToken(".");
        assert(token.getType() == ReplaceTokenType.TERMINATOR);

        token = new ReplaceToken("OTHER");
        assert(token.getType() == ReplaceTokenType.OTHER);

        token = new ReplaceToken("*");
        assert(token.getType() == ReplaceTokenType.OTHER);


    }
}
