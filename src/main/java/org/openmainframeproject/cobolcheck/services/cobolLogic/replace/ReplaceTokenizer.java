package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tokenizes a line and create tokens. All tokens are evaluated to a REPLACE statement
 */
public class ReplaceTokenizer {
    private int position;
    private final LinkedList<ReplaceToken> tokens = new LinkedList<>();
    private boolean isComment;
    private static final int GROUP_COMMENT_INDICATOR = 2;
    private static final String COMMENT_INDICATOR = "*";


    /**
     * Pattern to match all tokens in a line
     * <p>
     * This pattern will match all non-space characters  or a dot.
     * The lookBack (?<!\\.) is used to avoid matching a dot. We want the bot to be a token later.
     */
    private static final Pattern replacePattern = Pattern.compile("(\\S+)(?<!\\.)|(\\.)", Pattern.CASE_INSENSITIVE);

    /**
     * Pattern to match a comment line
     * <p>
     * This pattern will match a line that starts with 6 spaces or digits, followed by a space or a star.
     * The check is lenient, so it will also match lines that start with 0 to 5 spaces or digits.
     */
    private static final Pattern isCommentPattern = Pattern.compile("^([\\s|\\d]{0,6})([\\*|\\s])(\\s*)", Pattern.CASE_INSENSITIVE);

    /**
     * Tokenize a line. Every time a line is tokenized, the tokens are stored in a list, replacing the previous list.
     * Use method <i>hasMoreTokens()</i> and <i>nextToken()</i> to get the tokens.
     * @param line The line to tokenize
     */
    public void tokenize(String line) {
        this.position = 0;
        tokens.clear();
        isComment = false;
        Matcher isCommentMatcher = isCommentPattern.matcher(line);
        if (isCommentMatcher.find()) {
            // Is the line a comment? then stop processing
            isComment = isCommentMatcher.group(GROUP_COMMENT_INDICATOR).equals(COMMENT_INDICATOR);
            if (isComment) {
                return;
            }
        }
        Matcher allTokens = replacePattern.matcher(line);

        while (allTokens.find()) {
            String token = allTokens.group();
            tokens.add(new ReplaceToken(token));
        }
    }

    /**
     * Check if there are more tokens
     * @return true if there are more tokens
     */
    public boolean hasMoreTokens() {
        return (position < tokens.size());
    }

    /**
     * Get the next token. Tokens are read in order and can only be read once.
     * @return The next token. It is the type of ReplaceToken
     */
    public ReplaceToken nextToken() {
        return tokens.get(position++);
    }

    /**
     * Check if the line is a comment
     * @return true if the line is a comment
     */
    public boolean isComment() {
        return isComment;
    }
}
