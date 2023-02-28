package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;

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
    private boolean openParenthesis = false;
    private char quoteDelimiter = '"';
    private boolean processingNumericLiteral = false;

    public KeywordExtractor() {
        nextExpectedTokens = new ArrayList<>();
        multiWordTokens = new HashMap<>();
        multiWordTokens.put("TO", Arrays.asList("BE", "EQUAL"));
        multiWordTokens.put("BEFORE", Arrays.asList("EACH"));
        multiWordTokens.put("AFTER", Arrays.asList("EACH"));
        multiWordTokens.put("NEVER", Arrays.asList("HAPPENED"));
        multiWordTokens.put("AT", Arrays.asList("LEAST"));
        multiWordTokens.put("NO", Arrays.asList("MORE", "THAN"));
        multiWordTokens.put("BY", Arrays.asList("REFERENCE", "CONTENT", "VALUE"));

    }

    @Override
    public List<String> extractTokensFrom(String sourceLine) {
        List<String> tokens = new ArrayList<>();
        buffer = new StringBuilder();
        int tokenOffset = 0;
        sourceLine = sourceLine.trim();
        char previousCharacter = SPACE;
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
            }
            else if (currentCharacter == '(' && !openQuote){
                if (previousCharacter != SPACE){
                    buffer = addTokenAndClearBuffer(buffer, tokens);
                }
                buffer.append(currentCharacter);
                openParenthesis = true;
            }
            else if (currentCharacter == ')' && !openQuote){
                openParenthesis = false;
                buffer.append(currentCharacter);
                buffer = addTokenAndClearBuffer(buffer, tokens);
            }
            else if (openParenthesis && !openQuote)
                buffer.append(currentCharacter);

            else {

                if (processingNumericLiteral) {
                    if (!isDecimalPoint(buffer, currentCharacter, sourceLine, tokenOffset) 
                        && !Character.isDigit(currentCharacter)) {
                            processingNumericLiteral = false;  // prevent next period from being interpreted as end of sentence
                    }
                } else {
                    if (currentCharacter == PERIOD && !openQuote) {          // Cobol end of sentence
                        break;                                 // skip it
                    }
                    if (startNumericLiteral(buffer, currentCharacter)) {
                        processingNumericLiteral = true;
                    }
                }

                if (currentCharacter == SPACE) {
                    tokenOffset = handleEndOfWord(sourceLine, tokens, tokenOffset);
                } else{
                    buffer.append(currentCharacter);
                }
            }
            previousCharacter = currentCharacter;
            tokenOffset += 1;
        }
        if (buffer.length() > 0) {
            buffer = addTokenAndClearBuffer(buffer, tokens);
        }
        return tokens;
    }

    private int handleEndOfWord(String sourceLine, List<String> tokens, int tokenOffset) {
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
                        String temp = sourceLine.substring(startOfLookahead, endOfLookahead);
                        if (expectedToken.equalsIgnoreCase(temp)
                                && (endOfLookahead == sourceLine.length()
                                || sourceLine.charAt(endOfLookahead) == SPACE)) {
                            buffer.append(expectedToken);
                            tokenOffset += expectedToken.length() + 1;
                            buffer.append(SPACE);
                            startOfLookahead = tokenOffset + 1;
                        }
                    }

                }
                buffer.deleteCharAt(buffer.length() - 1);
                buffer = addTokenAndClearBuffer(buffer, tokens);
                nextExpectedTokens = new ArrayList<>();
            } else {
                nextExpectedTokens = new ArrayList<>();
                if (buffer.length() > 0) {
                    buffer = addTokenAndClearBuffer(buffer, tokens);
                }
            }
        }
        return tokenOffset;
    }

    public boolean tokenListEndsDuringMultiToken(List<String> tokens){
        if (tokens == null || tokens.isEmpty())
            return false;

        if (StringHelper.equalsAny(tokens.get(tokens.size() - 1), multiWordTokens.keySet()))
            return true;

        return false;
    }
    public void handleEndOfWord(){

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
