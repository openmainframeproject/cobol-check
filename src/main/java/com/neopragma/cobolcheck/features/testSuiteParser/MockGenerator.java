package com.neopragma.cobolcheck.features.testSuiteParser;

import com.neopragma.cobolcheck.services.StringHelper;

import java.util.ArrayList;
import java.util.List;

public class MockGenerator {

    private String evaluateStartLine = "            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME";
    private String whenFormat = "                WHEN %1$s ALSO %2$s";
    private String performFormat = "                    PERFORM %s";
    private String whenOtherLine = "           WHEN OTHER";
    private String endEvaluateLine = "            END-EVALUATE";

    List<String> generateMockSections(List<Mock> mocks){

        List<String> lines = new ArrayList<>(getMockSectionCommentHeader());
        for (Mock mock : mocks){
            lines.addAll(generateSectionForMock(mock));
            lines.add("");
        }
        return lines;
    }

   List<String> generateMockPerformCalls(String identifier, List<Mock> mocks){
        List<String> lines = new ArrayList<>();
        lines.add(evaluateStartLine);

        for (Mock mock: mocks) {
            if (mock.getScope() == MockScope.Local){
                if (mock.getIdentifier().equals(identifier)){
                    lines.add(String.format(whenFormat, mock.getTestSuiteName(), mock.getTestCaseName()));
                    lines.add(String.format(performFormat, mock.getGeneratedMockIdentifier()));
                }
            }
        }
        lines.add(whenOtherLine);

        return lines;
    }

    String getEndEvaluateLine() {
        return endEvaluateLine;
    }

    private List<String> getMockSectionCommentHeader(){
        List<String> lines = new ArrayList<>();
        lines.add("      *****************************************************************");
        lines.add(StringHelper.commentOutLine("       Sections called when mocking"));
        lines.add("      *****************************************************************");
        return lines;
    }

    //TODO: Add pre-spaces
    private List<String> generateSectionForMock(Mock mock){
        List<String> lines = new ArrayList<>();
//        lines.add(StringHelper.commentOutLine("       " + mock.getCommentText()));
        lines.add("       " + mock.getGeneratedMockIdentifier() + " SECTION.");
        lines.addAll(mock.getLines());
        lines.add("       .");
        return lines;
    }


}
