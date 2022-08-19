package org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.*;
import org.openmainframeproject.cobolcheck.features.interpreter.StringTokenizerExtractor;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.text.ParseException;
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
    private String failData;
    private boolean expectFailMessage;
    private boolean expectWasMessage;
    private boolean expectExpectedMessage;


    protected DataTransferObject dataTransferObject;
    private boolean expectNumberPassed;
    private boolean expectNumberFailed;

    /**
     * Writes text in a specific format to a file. The format is written in the style of the Formatter's
     * DataTransferObject. This method is expected to be called after text has been parsed to the
     * DataTransferObject.
     * @param path The path for the test results to be written.
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

            else if (expectFailMessage & expectExpectedMessage){
                failData = line.trim();
                expectExpectedMessage = false;
                expectWasMessage = true;
            }

            else if (expectFailMessage & expectWasMessage){
                failData.concat(",");
                failData.concat(line.trim());
                dataTransferObject.setCurrentTestCaseFailure(failData, getFailureType(failData));
                expectFailMessage = false;
                expectWasMessage = false;
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

            else{
                try{
                    setTestCaseValues(line);
                } catch (PossibleInternalLogicErrorException e){
                    Log.debug(e.getMessage());
                }

            }

        }
    }

    private void setTestCaseValues(String line){
        line = line.trim();
        String[] partedLine = line.split("\\.");
        if (partedLine.length < 2)
            throw new PossibleInternalLogicErrorException("Following line is expected to be output from source and will not be formatted: " + line);

        String[] tokens = partedLine[0].split(" ");
        if (tokens.length > 0){
            dataTransferObject.moveToNextTestCase();
            dataTransferObject.setCurrentTestCaseName(partedLine[1].trim());
            if (tokens[0].equalsIgnoreCase(passKeyword)){
                //Test passed
                return;
            }
            else if (tokens[0].equalsIgnoreCase(failPrefixKeyword) && tokens.length > 1 &&
                    tokens[1].equalsIgnoreCase(failKeyword)){
                //Test failed
                expectFailMessage = true;
                expectExpectedMessage = true;
                return;
            }
        }
        throw new PossibleInternalLogicErrorException("Following line is expected to be output from source and will not be formatted: " + line);
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
            case tableDocument:
                return new TableDocumentDto();
            case tableEmbed:
                return new TableEmbedDto();
            default:
                return new JUnitDto();
        }
    }

}
