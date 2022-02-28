package org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats;

import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObject;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObjectStyle;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.JUnitDto;
import org.openmainframeproject.cobolcheck.features.interpreter.StringTokenizerExtractor;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public abstract class Formatter {
    private final String testSuiteKeyword = "TESTSUITE:";
    private final String passKeyword = "PASS:";
    private final String failPrefixKeyword = "****";
    private final String failKeyword = "FAIL:";
    private final String expectedKeyword = "EXPECTED";
    private final List<String> verifyCompareKeywords = Arrays.asList("EXACT", "AT LEAST", "NO MORE THAN");
    private final String expectKeyword = "EXPECT";
    private final String verifyKeyword = "VERIFY";
    private final String testCasesExecutedText = "TEST CASES WERE EXECUTED";



    private TokenExtractor tokenExtractor = new StringTokenizerExtractor();
    private boolean expectTestSuiteName;
    private String currentTestSuite;
    private boolean expectFailMessage;

    protected DataTransferObject dataTransferObject;
    private boolean expectNumberPassed;
    private boolean expectNumberFailed;

    /**
     * Writes text in a specific format to a file. The format is written in the style of the Formatter's
     * DataTransferObject. This method is expected to be called after text has been parsed to the
     * DataTransferObject.
     * @param path The name of the source program.
     */
    public abstract String writeInFormat(String path) throws Exception;

    public Formatter(DataTransferObjectStyle dataTransferObjectStyle){
        dataTransferObject = instantiateBasedOnStyle(dataTransferObjectStyle);
    }

    /**
     * Parses text from text results to a data transfer object style. The style is given
     * in the constructor of the Formmatter.
     * @param text Test results returned from test program.
     * @param testSuitePackage The package(program) for the testsuite(s) in the text.
     */
    public void parseText(String text, String testSuitePackage){
        String[] lines = text.split(Constants.NEWLINE);

        for (String line : lines){
            if (line.trim().isEmpty() || line.startsWith("==="))
                continue;


            //Getting Test Suite name
            if (line.trim().equalsIgnoreCase(testSuiteKeyword))
                expectTestSuiteName = true;

            else if (expectTestSuiteName)
            {
                dataTransferObject.moveToNextTestSuite();
                dataTransferObject.setCurrentTestSuitePackage(testSuitePackage);
                dataTransferObject.setCurrentTestSuiteName(line.trim());
                expectTestSuiteName = false;
            }

            else if (expectFailMessage){
                dataTransferObject.setCurrentTestCaseFailure(line.trim(), getFailureType(line));
                expectFailMessage = false;
            }

            else if (line.toUpperCase(Locale.ROOT).contains(testCasesExecutedText)){
                String[] tokens = line.trim().split(" ");
                if (tokens.length == 5){
                    dataTransferObject.setNumberOfAllTests(tokens[0]);
                    expectNumberPassed = true;
                }
            }

            else if (expectNumberPassed){
                expectNumberPassed = false;
                expectNumberFailed = true;
            }

            else if (expectNumberFailed){
                expectNumberFailed = false;
                String[] tokens = line.trim().split(" ");
                dataTransferObject.setNumberOffAllFailures(tokens[0]);
            }

            else
                setTestCaseValues(line);
        }
    }

    private void setTestCaseValues(String line){
        String[] partedLine = line.split("\\.");
        String[] tokens = partedLine[0].split(" ");
        if (tokens.length > 0){
            dataTransferObject.moveToNextTestCase();
            dataTransferObject.setCurrentTestCaseName(partedLine[1].trim());
            if (tokens[0].equalsIgnoreCase(passKeyword)){
                //Test passed
            }
            if (tokens[0].equalsIgnoreCase(failPrefixKeyword) && tokens[1].equalsIgnoreCase(failKeyword)){
                //Test failed
                expectFailMessage = true;
            }
        }
    }

    private String getFailureType(String line){
        List<String> tokens = tokenExtractor.extractTokensFrom(line);
        if (!tokens.isEmpty()) {
            if (tokens.get(0).equalsIgnoreCase(expectedKeyword)){
                for (String token : tokens){
                    if (verifyCompareKeywords.contains(token.toUpperCase(Locale.ROOT))){
                        return verifyKeyword;
                    }
                }
                return expectKeyword;
            }
        }
        return "";
    }

    private DataTransferObject instantiateBasedOnStyle(DataTransferObjectStyle style){
        switch (style){
            case JUnit:
                return new JUnitDto();
            default:
                return new JUnitDto();
        }
    }

}
