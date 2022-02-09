package com.neopragma.cobolcheck.features.testSuiteParser;

import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.StringHelper;

import java.util.ArrayList;
import java.util.List;

public class MockGenerator {

    private final String testSuiteIdentifier = "%sTEST-SUITE-NAME";
    private final String testCaseIdentifier = "%sTEST-CASE-NAME";
    private final String performFormat = "                    PERFORM %s";
    private final String endEvaluateLine = "            END-EVALUATE";

    private final String countMockInitialWSHeader = "       01  %sMOCKS-GENERATED.";
    private final String initializeMockCountParagraphHeader = "       %sINITIALIZE-MOCK-COUNT.";




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
        if (mocks.isEmpty())
            return lines;

        lines.add(String.format(countMockInitialWSHeader, Config.getTestCodePrefix()));
        lines.addAll(generateMockCountValues(mocks));

        return lines;
    }

    /**Generates the lines for the initializer paragraph.
     * This resets the current count and expected count of all global mocks in cobol.
     * @param mocks - The mocks to generate lines for
     * @return The generated lines
     */
    List<String> generateMockCountInitializer(List<Mock> mocks){
        List<String> lines = new ArrayList<>();
        lines.add(String.format(initializeMockCountParagraphHeader, Config.getTestCodePrefix()));
        lines.addAll(CobolGenerator.generateCommentBlock("Sets all global mock counters and expected count to 0"));

        if (mocks.isEmpty()){
            lines.add("           .");
            return lines;
        }

        for (Mock mock : mocks){
            if (mock.getScope() == MockScope.Global){
                lines.add("           MOVE 0 TO " + mock.getGeneratedMockCountIdentifier());
                lines.add("           MOVE 0 TO " + mock.getGeneratedMockCountExpectedIdentifier());
            }
        }
        lines.add("           .");
        return lines;
    }

    /**Generates the lines for Paragraphs based on mocks,
     * for each mock in a given list.
     * @param mocks - The mocks to generate Paragraphs for
     * @return The generated lines
     */
    List<String> generateMockParagraphs(List<Mock> mocks, boolean withComments){
        List<String> lines = new ArrayList<>();
        if (mocks.isEmpty())
            return lines;

        lines.addAll(CobolGenerator.generateCommentBlock("Paragraphs called when mocking"));
        for (Mock mock : mocks){
            lines.addAll(generateParagraphsForMock(mock, withComments));
            lines.add("");
        }
        return lines;
    }

    /**Generates the lines for 'Evaluate when' to perform the correct generated Paragraphs
     * for a specific identifier.
     * @param identifier - The identifier of the SECTION, PARAGRAPH etc. that is mocked.
     * @param mocks - All mocks in all tests
     * @return The generated lines
     */
    List<String> generateMockPerformCalls(String identifier, String type, List<String> arguments, List<Mock> mocks){
        EvaluationGenerator evaluationGenerator = new EvaluationGenerator(String.format(testSuiteIdentifier, Config.getTestCodePrefix()),
                String.format(testCaseIdentifier, Config.getTestCodePrefix()));
        List<Mock> globalMocks = new ArrayList<>();

        if (mocks.isEmpty())
            return new ArrayList<>();

        for (Mock mock: mocks) {
            if (mock.getIdentifier().equals(identifier) && mock.getType().equals(type)
                    && mock.getArguments().equals(arguments)){
                if (mock.getScope() == MockScope.Local){
                    String line = String.format(performFormat, mock.getGeneratedMockIdentifier());
                    evaluationGenerator.addEvaluationItem(line, mock.getTestSuiteName(), mock.getTestCaseName());
                    mock.markAsUsed();
                }
                if (mock.getScope() == MockScope.Global){
                    globalMocks.add(mock);
                }
            }
        }
        //Global mocks should be at the bottom
        for (Mock mock : globalMocks){
            String line = String.format(performFormat, mock.getGeneratedMockIdentifier());
            evaluationGenerator.addEvaluationItem(line, mock.getTestSuiteName(), "ANY");
            mock.markAsUsed();
        }
        if (type.equals(Constants.SECTION_TOKEN) || type.equals(Constants.PARAGRAPH_TOKEN))
            return evaluationGenerator.getEvaluationLines(true, new ArrayList<>(), false);

        else
            return evaluationGenerator.getEvaluationLines(false, null, true);
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

    private List<String> generateParagraphsForMock(Mock mock, boolean withComment){
        List<String> comments = new ArrayList<>();
        if (withComment){
            for (String line : mock.getCommentText()){
                comments.add(StringHelper.commentOutLine(line));
            }
        }

        List<String> body = new ArrayList<>();
        body.add("           ADD 1 TO " + mock.getGeneratedMockCountIdentifier());
        body.addAll(mock.getLines());
        return CobolGenerator.generateParagraphLines(mock.getGeneratedMockIdentifier(), comments, body);
    }
}
