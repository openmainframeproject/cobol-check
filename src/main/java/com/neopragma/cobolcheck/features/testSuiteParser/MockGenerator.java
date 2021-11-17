package com.neopragma.cobolcheck.features.testSuiteParser;

import com.neopragma.cobolcheck.services.StringHelper;

import java.util.ArrayList;
import java.util.List;

public class MockGenerator {

    private final String evaluateStartLine = "            EVALUATE UT-TEST-SUITE-NAME ALSO UT-TEST-CASE-NAME";
    private final String whenFormat1 = "                WHEN %s";
    private final String whenFormat2 = "                ALSO %s";
    private final String performFormat = "                    PERFORM %s";
    private final String whenOtherLine = "           WHEN OTHER";
    private final String endEvaluateLine = "            END-EVALUATE";
    private final String anyKeyword = "ANY";


    /**Generates the lines for keeping track of mock counting
     * For each mock a variable are created for:
     * - Current count
     * - Expected count
     * - Mock operation (string showing type and identity)
     * @param mocks - The mocks to generate lines for
     * @return The generated lines
     */
    List<String> generateWorkingStorageMockCountLines(List<Mock> mocks){
        List<String> lines = new ArrayList<>();
        lines.add("       01  UT-MOCKS-GENERATED.");
        lines.addAll(generateMockCountValues(mocks));

        return lines;
    }

    /**Generates the lines for the initializer section.
     * This resets the current count and expected count of all global mocks in cobol.
     * @param mocks - The mocks to generate lines for
     * @return The generated lines
     */
    List<String> generateMockCountInitializer(List<Mock> mocks){
        List<String> lines = new ArrayList<>();
        lines.add("       UT-INITIALIZE-MOCK-COUNT SECTION.");
        lines.add("      *****************************************************************");
        lines.add(StringHelper.commentOutLine("       Sets all global mock counters and expected count to 0"));
        lines.add("      *****************************************************************");
        for (Mock mock : mocks){
            if (mock.getScope() == MockScope.Global){
                lines.add("           MOVE 0 TO " + mock.getGeneratedMockCountIdentifier());
                lines.add("           MOVE 0 TO " + mock.getGeneratedMockCountExpectedIdentifier());
            }
        }
        lines.add("       .");
        return lines;
    }

    /**Generates the lines for SECTIONs based on mocks,
     * for each mock in a given list.
     * @param mocks - The mocks to generate SECTIONs for
     * @return The generated lines
     */
    List<String> generateMockSections(List<Mock> mocks, boolean withComments){

        List<String> lines = new ArrayList<>(getMockSectionCommentHeader());
        for (Mock mock : mocks){
            lines.addAll(generateSectionForMock(mock, withComments));
            lines.add("");
        }
        return lines;
    }

    /**Generates the lines for 'Evaluate when' to perform the correct generated SECTION
     * for a specific identifier.
     * @param identifier - The identifier of the SECTION, PARAGRAPH etc. that is mocked.
     * @param mocks - All mocks in all tests
     * @return The generated lines
     */
    List<String> generateMockPerformCalls(String identifier, String type, List<Mock> mocks){
        List<String> resultLines = new ArrayList<>();
        List<String> localLines = new ArrayList<>();
        List<String> globalLines = new ArrayList<>();
        resultLines.add(evaluateStartLine);

        for (Mock mock: mocks) {
            if (mock.getIdentifier().equals(identifier) && mock.getType().equals(type)){
                if (mock.getScope() == MockScope.Local){
                    localLines.add(String.format(whenFormat1, mock.getTestSuiteName()));
                    localLines.add(String.format(whenFormat2, mock.getTestCaseName()));
                    localLines.add(String.format(performFormat, mock.getGeneratedMockIdentifier()));
                    mock.markAsUsed();
                }
                if (mock.getScope() == MockScope.Global){
                    globalLines.add(String.format(whenFormat1, mock.getTestSuiteName()));
                    globalLines.add(String.format(whenFormat2, anyKeyword));
                    globalLines.add(String.format(performFormat, mock.getGeneratedMockIdentifier()));
                    mock.markAsUsed();
                }
            }
        }
        //If a local mock exists, that mock should apply
        // Thus we need to have local mocks on top of global mocks
        resultLines.addAll(localLines);
        resultLines.addAll(globalLines);
        resultLines.add(whenOtherLine);

        return resultLines;
    }

    String getEndEvaluateLine() {
        return endEvaluateLine;
    }

    private List<String> generateMockCountValues(List<Mock> mocks) {
        List<String> lines = new ArrayList<>();
        for (Mock mock : mocks){
            lines.add("           05  " + mock.getGeneratedMockCountIdentifier() + "       PIC 9(02) VALUE ZERO.");
            lines.add("           05  " + mock.getGeneratedMockCountExpectedIdentifier() + "    PIC 9(02) VALUE ZERO.");
            lines.add("           05  " + mock.getGeneratedMockStringIdentifierName() + "        PIC X(40)");
            lines.add("                   VALUE \"" + mock.getMockDisplayString() + "\".");
        }
        return lines;
    }

    private List<String> getMockSectionCommentHeader(){
        List<String> lines = new ArrayList<>();
        lines.add("      *****************************************************************");
        lines.add(StringHelper.commentOutLine("       Sections called when mocking"));
        lines.add("      *****************************************************************");
        return lines;
    }

    private List<String> generateSectionForMock(Mock mock, boolean withComment){
        List<String> lines = new ArrayList<>();
        lines.add("       " + mock.getGeneratedMockIdentifier() + " SECTION.");
        if (withComment){
            for (String line : mock.getCommentText()){
                lines.add(StringHelper.commentOutLine(line));
            }
        }
        lines.add("           ADD 1 TO " + mock.getGeneratedMockCountIdentifier());
        lines.addAll(mock.getLines());
        lines.add("       .");
        return lines;
    }


}
