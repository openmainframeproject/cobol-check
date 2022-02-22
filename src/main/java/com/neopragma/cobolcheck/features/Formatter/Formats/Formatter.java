package com.neopragma.cobolcheck.features.Formatter.Formats;

import com.neopragma.cobolcheck.features.Formatter.DataTransferObjects.DataTransferObjectAdapter;
import com.neopragma.cobolcheck.features.Formatter.DataTransferObjects.DataTransferObjectStyle;
import com.neopragma.cobolcheck.features.interpreter.StringTokenizerExtractor;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;

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

    protected DataTransferObjectAdapter dataTransferObjectAdapter;
    private boolean expectNumberPassed;
    private boolean expectNumberFailed;


    public abstract String getFormattedString(String path) throws Exception;

    public Formatter(DataTransferObjectStyle dataTransferObjectStyle){
        dataTransferObjectAdapter = new DataTransferObjectAdapter(dataTransferObjectStyle);
    }

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
                dataTransferObjectAdapter.moveToNextTestSuite();
                dataTransferObjectAdapter.setCurrentTestSuitePackage(testSuitePackage);
                dataTransferObjectAdapter.setCurrentTestSuiteName(line.trim());
                expectTestSuiteName = false;
            }

            else if (expectFailMessage){
                dataTransferObjectAdapter.setCurrentTestCaseFailure(line.trim(), getFailureType(line));
                expectFailMessage = false;
            }

            else if (line.toUpperCase(Locale.ROOT).contains(testCasesExecutedText)){
                String[] tokens = line.trim().split(" ");
                if (tokens.length == 5){
                    dataTransferObjectAdapter.setNumberOfAllTests(tokens[0]);
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
                dataTransferObjectAdapter.setNumberOffAllFailures(tokens[0]);
            }

            else
                setTestCaseValues(line);
        }
    }

    private void setTestCaseValues(String line){
        String[] partedLine = line.split("\\.");
        String[] tokens = partedLine[0].split(" ");
        if (tokens.length > 0){
            dataTransferObjectAdapter.moveToNextTestCase();
            dataTransferObjectAdapter.setCurrentTestCaseName(partedLine[1].trim());
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

}
