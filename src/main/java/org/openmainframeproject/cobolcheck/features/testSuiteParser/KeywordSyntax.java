package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public class KeywordSyntax {
    private Keyword lastKeyword;
    private String syntaxErrorFileMessage = "SYNTAX ERROR in file: %1s";
    private String lineIndexMessage = "Unexpected token on line %1s, index %2s:";

    private String followingExpectedGotMessage = "Following <%1s> Expected %2s, got <%3s>";
    private String keywordInBlock = "Cannot have Cobol Check keyword <%1s> inside a %2s block";

    private final List<String> cobolCheckStartingAndEndingKeywords = Arrays.asList(Constants.TESTSUITE_KEYWORD,
            Constants.TESTCASE_KEYWORD, Constants.EXPECT_KEYWORD, Constants.MOCK_KEYWORD, Constants.ENDMOCK_KEYWORD,
            Constants.VERIFY_KEYWORD, Constants.BEFORE_EACH_TOKEN, Constants.END_BEFORE_TOKEN, Constants.AFTER_EACH_TOKEN,
            Constants.END_AFTER_TOKEN);

    public boolean checkSyntax(Keyword currentKeyword, String currentFile, int lineNumber, int lineIndex){
        if (lastKeyword != null){
            if (!lastKeyword.getvalidNextKeys().contains(currentKeyword.value())){
                System.out.println(String.format(syntaxErrorFileMessage, currentFile));
                System.out.println(String.format(lineIndexMessage, lineNumber, lineIndex));
                System.out.println(String.format(followingExpectedGotMessage, lastKeyword.value(),
                        Arrays.toString(lastKeyword.getvalidNextKeys().toArray()), currentKeyword.value()));
                System.out.println("");
            }
        }
        lastKeyword = currentKeyword;
        return true;
    }

    public void checkSyntaxInsideBlock(String blockKeyword, List<String> cobolLines, TokenExtractor tokenExtractor, String currentFile, int lineNumber) {
        int revertedCount = cobolLines.size();
        for (String line : cobolLines){
            List<String> keywords = tokenExtractor.extractTokensFrom(line);
            for(String keyword : keywords){
                if (cobolCheckStartingAndEndingKeywords.contains(keyword.toUpperCase(Locale.ROOT))){
                    int index = line.indexOf(keyword) + 1;
                    System.out.println(String.format(syntaxErrorFileMessage, currentFile));
                    System.out.println(String.format(lineIndexMessage, lineNumber - revertedCount, index));
                    System.out.println(String.format(keywordInBlock, keyword, blockKeyword));
                    System.out.println("");
                }
            }
            revertedCount -=1;
        }
    }
}
