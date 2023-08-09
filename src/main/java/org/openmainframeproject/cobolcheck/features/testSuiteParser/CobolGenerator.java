package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.StringHelper;

import java.util.ArrayList;
import java.util.List;

public class CobolGenerator {
    private final static String SECTION_HEADER_FORMAT = "       SECTION %s.";    
    private final static String WHEN_OTHER_SECTION_HEADER_FORMAT = "       %s SECTION.";
    private final static String PARAGRAPH_HEADER_FORMAT = "       %s.";
    private final static String ENDING_PERIOD = "           .";




    static List<String> generateSectionLines(String identifier, List<String> commentLines, List<String> bodyLines){
        List<String> lines = new ArrayList<>();
        lines.add(String.format(SECTION_HEADER_FORMAT, identifier));
        if (commentLines != null)
            lines.addAll(commentLines);
        if (bodyLines != null)
            lines.addAll(bodyLines);
        lines.add(ENDING_PERIOD);
        return lines;
    }

    static List<String> generateParagraphLines(String identifier, List<String> commentLines, List<String> bodyLines){
        List<String> lines = new ArrayList<>();
        lines.add(String.format(PARAGRAPH_HEADER_FORMAT, identifier));
        if (commentLines != null)
            lines.addAll(commentLines);
        if (bodyLines != null)
            lines.addAll(bodyLines);
        lines.add(ENDING_PERIOD);
        return lines;
    }

    static List<String> generateCommentBlock(String... body){
        List<String> commentLines = new ArrayList<>();
        commentLines.add("      *****************************************************************");
        for (String line: body){
            commentLines.add(StringHelper.commentOutLine(line));
        }
        commentLines.add("      *****************************************************************");
        return commentLines;
    }

    static String getInjectStartTagComment(){
        String tag = Config.getInjectStartTag();
        if (tag.equalsIgnoreCase("NULL"))
            return "";
        else
            return StringHelper.commentOutLine(tag);
    }

    static String getInjectEndTagComment(){
        String tag = Config.getInjectEndTag();
        if (tag.equalsIgnoreCase("NULL"))
            return "";
        else
            return StringHelper.commentOutLine(tag);
    }

    static String getContinueStatement(){
        return "           CONTINUE";
    }

    static void addStartAndEndTags(List<String> lines){
        if (!getInjectStartTagComment().isEmpty())
            lines.add(0, getInjectStartTagComment());
        if (!getInjectEndTagComment().isEmpty())
            lines.add(getInjectEndTagComment());
    }

    static List<String> generateWhenOtherLines(String identifier, String type, List<String> commentLines, List<String> bodyLines){
        List<String> lines = new ArrayList<>();
        if(type.equals(Constants.SECTION_TOKEN)) 
            lines.add(String.format(WHEN_OTHER_SECTION_HEADER_FORMAT, identifier));
        else lines.add(String.format(PARAGRAPH_HEADER_FORMAT, identifier));
        if (commentLines != null)
            lines.addAll(commentLines);
        if (bodyLines != null)
            lines.addAll(bodyLines);
        lines.add(ENDING_PERIOD);
        return lines;
    }

}
