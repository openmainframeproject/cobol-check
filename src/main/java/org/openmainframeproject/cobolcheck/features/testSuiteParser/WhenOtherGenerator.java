package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import java.util.ArrayList;
import java.util.List;

import org.openmainframeproject.cobolcheck.services.StringHelper;

public class WhenOtherGenerator {
    private final String performFormat = "                    PERFORM %s";

    
    String generateWhenOtherCall(WhenOther whenOther) {
        return String.format(performFormat, whenOther.getGeneratedWhenOtherIdentifier());
    }

    List<String> generateWhenOther(WhenOther whenOther, boolean withComments){
        List<String> lines = new ArrayList<>();
        lines.addAll(CobolGenerator.generateCommentBlock("WhenOther block called"));
        lines.addAll(generateParagraphsForWhenOther(whenOther, withComments));
        lines.add("");
        return lines;
    }

    private List<String> generateParagraphsForWhenOther(WhenOther whenOther, boolean withComments){
        List<String> comments = new ArrayList<>();
        if (withComments){
            for (String line : whenOther.getCommentText()){
                comments.add(StringHelper.commentOutLine(line));
            }
        }
        List<String> body = new ArrayList<>();
        body.addAll(whenOther.getLines());
        return CobolGenerator.generateWhenOtherLines(whenOther.getGeneratedWhenOtherIdentifier(), whenOther.getType(), comments, body);
    }

    
}
