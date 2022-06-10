package org.openmainframeproject.cobolcheck.features.interpreter;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.*;

/**
 * This class uses StringTokenizer to find tokens in the Cobol source line. It is used by the Generator
 * when processing the source for the program under test.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class StringTokenizerExtractor implements TokenExtractor {

    public StringTokenizerExtractor() {

    }

    private static final String delimiters = String.format(" .%s", Constants.NEWLINE);

    // Some "logical tokens" meaningful in Cobol source consist of two words separated by whitespace.
    // Couldn't use Map.of because it has a limitation of 10 entries. (Dec 2020)
    private static final Map<String, List<String>> expectedTokens = new HashMap<>();
    static {
        expectedTokens.put("PROCEDURE", Arrays.asList("DIVISION"));
        expectedTokens.put("DATA", Arrays.asList("DIVISION"));
        expectedTokens.put("ENVIRONMENT", Arrays.asList("DIVISION"));
        expectedTokens.put("IDENTIFICATION", Arrays.asList("DIVISION"));
        expectedTokens.put("FILE", Arrays.asList("SECTION", "CONTROL", "STATUS"));
        expectedTokens.put("ID", Arrays.asList("DIVISION"));
        expectedTokens.put("CONFIGURATION", Arrays.asList("SECTION"));
        expectedTokens.put("INPUT-OUTPUT", Arrays.asList("SECTION"));
        expectedTokens.put("LINKAGE", Arrays.asList("SECTION"));
        expectedTokens.put("WORKING-STORAGE", Arrays.asList("SECTION"));
        expectedTokens.put("LOCAL-STORAGE", Arrays.asList("SECTION"));
        expectedTokens.put("BY", Arrays.asList("REFERENCE", "CONTENT", "VALUE"));
        expectedTokens.put("EXEC", Arrays.asList("SQL", "CICS"));
    }

    /**
     * Extracts tokens meaningful for processing Cobol source code from a Cobol source line.
     *
     * Such "meaningful" tokens may comprise more than one "plain" token,
     * such as "PROCEDURE DIVISION" or "INPUT-OUTPUT SECTION" - treated as a single token here.
     *
     * Strings are treated as a single token.
     * Commented lines are ignored.
     *
     * @param sourceLine (String) - passed by a component that reads Cobol source files, assumed to be upper case.
     * @return list of tokens (List&lt;String&gt;)
     */
    @Override
    public List<String> extractTokensFrom(String sourceLine) {
        if (sourceLine == null) {
            throw new PossibleInternalLogicErrorException(
                 Messages.get("ERR001",
                         "sourceLine",
                         "StringTokenizerExtractor.extractTokensFrom(sourceLine)")
            );
        }
        List<String> tokens = new ArrayList<>();
        List<String> expectedNext = new ArrayList<>();
        String saved = Constants.EMPTY_STRING;
        Map<String, String> stringTokensToString = new HashMap<>();
        sourceLine = swapStringsOutWithMappedTokens(sourceLine, stringTokensToString);
        StringTokenizer tokenizer = new StringTokenizer(sourceLine, delimiters);
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken().toUpperCase(Locale.ROOT);
            if (token.startsWith(Constants.COMMENT_INDICATOR)) {
                break;
            }
            if (!expectedNext.isEmpty()) {
                for (String expectedValue : expectedNext) {
                    if (token.equals(expectedValue)) {
                        token = saved + " " + token;
                        expectedNext = new ArrayList<>();
                        saved = Constants.EMPTY_STRING;
                    }
                }
            }
            if (expectedTokens.containsKey(token)) {
                expectedNext = expectedTokens.get(token);
                saved = token;
            } else {
                tokens.add(token);
            }
        }
        swapMappedTokensOutWithSavedStrings(tokens, stringTokensToString);
        return tokens;
    }

    /**
     * Swaps out strings in the given line, with tags, so each string will be seen as a single
     * token, when extracting tokens. Additionally, spaces are added to each side of string-signs.
     *
     * Each tag is mapped to the original string, so that the original values can be placed again
     * after the extraction.
     *
     * @param line - Line for which to swap string values
     * @param stringTokensToString - Map of tags to original string values
     * @return The new line, with the strings swapped for tags
     */
    private String swapStringsOutWithMappedTokens(String line, Map<String, String> stringTokensToString){
        int stringCount = 0;
        boolean insideString = false;
        char stringSign = ' ';
        String newString = "";
        String currentRealString = "";
        String mapValue = "";
        for (char c : line.toCharArray()){
            if (c == '\''){
                if (insideString){
                    currentRealString += c;
                    if (stringSign == '\''){
                        insideString = false;
                        stringSign = ' ';
                        stringTokensToString.put(mapValue, currentRealString);
                    }
                }
                else if (!insideString){
                    insideString = true;
                    stringSign = '\'';
                    currentRealString = String.valueOf(c);
                    mapValue = "" + c + "STRING" + stringCount + c;
                    stringCount++;
                    newString += " " + mapValue + " ";
                }
            }
            else if (c == '"'){
                if (insideString){
                    currentRealString += c;
                    if (stringSign == '"'){
                        insideString = false;
                        stringSign = ' ';
                        stringTokensToString.put(mapValue, currentRealString);
                    }
                }
                else if (!insideString){
                    insideString = true;
                    stringSign = '"';
                    currentRealString = String.valueOf(c);
                    mapValue = "" + c + "STRING" + stringCount + c;
                    stringCount++;
                    newString += " " + mapValue + " ";
                }
            }
            else {
                if (insideString)
                    currentRealString += c;
                else
                    newString += c;
            }
        }
        return newString;
    }

    private void swapMappedTokensOutWithSavedStrings(List<String> tokens, Map<String, String> stringTokensToString){
        for(int i = 0; i < tokens.size(); i++){
            if (stringTokensToString.containsKey(tokens.get(i))){
                tokens.set(i, stringTokensToString.get(tokens.get(i)));
            }
        }
    }
}
