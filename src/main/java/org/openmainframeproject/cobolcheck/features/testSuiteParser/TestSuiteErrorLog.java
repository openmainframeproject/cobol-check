package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;
import org.openmainframeproject.cobolcheck.services.filehelpers.EncodingIO;
import org.openmainframeproject.cobolcheck.services.filehelpers.FilePermission;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.*;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public class TestSuiteErrorLog {



    public enum ErrorTypes {SYNTAX_ERROR, RUNTIME_ERROR, WARNING}
    private Keyword lastKeyword;
    private String lastToken;

    private String fileMessage = "%1s in file: %2s";
    private String lineIndexMessage = "Unexpected token on line %1s, index %2s:";

    private String followingExpectedGotMessage = "Following <%1s> classified as <%2s>" + Constants.NEWLINE +
            "Expected classification: %3s" + Constants.NEWLINE + "Got <%4s> classified as <%5s>";
    private String followingExpectedInContextGotMessage = "Following <%1s> classified as <%2s>" + Constants.NEWLINE +
            "Expected classification in the context of %3s: %4s" + Constants.NEWLINE + "Got <%5s> classified as <%6s>";
    private String keywordInBlock = "Cannot have Cobol Check keyword <%1s> inside a %2s block";

    private boolean errorOccured = false;

    private final List<String> cobolCheckStartingAndEndingKeywords = Arrays.asList(Constants.TESTSUITE_KEYWORD,
            Constants.TESTCASE_KEYWORD, Constants.EXPECT_KEYWORD, Constants.MOCK_KEYWORD, Constants.ENDMOCK_KEYWORD,
            Constants.VERIFY_KEYWORD, Constants.BEFORE_EACH_TOKEN, Constants.END_BEFORE_TOKEN,
            Constants.AFTER_EACH_TOKEN,
            Constants.END_AFTER_TOKEN, Constants.HAPPENED_KEYWORD, Constants.TO_BE_KEYWORD, Constants.TO_EQUAL_KEYWORD,
            Constants.BEFORE_EACH_TOKEN_HYPHEN, Constants.AFTER_EACH_TOKEN_HYPHEN, Constants.NEVER_HAPPENED_KEYWORD,
            Constants.ONCE_KEYWORD, Constants.AT_LEAST_KEYWORD, Constants.NO_MORE_THAN_KEYWORD);

    private String errorLogPath;

    private String errorLogMessages = "";

    public TestSuiteErrorLog(){
        errorLogPath = getTestSuiteParserErrorLogPath();
        initializeTestSuiteErrorLogWriter(errorLogPath);
    }

    public boolean hasErrorOccured() {
        return errorOccured;
    }

    public String getErrorMessages(){ return errorLogMessages; }

    public String getLastKeywordValue() { return lastKeyword.value(); }

    public boolean checkExpectedTokenSyntax(Keyword currentKeyword, String currentToken, String currentFile, int lineNumber, int lineIndex){
        String error = "";
        if (lastKeyword != null){
            if (!lastKeyword.getValidNextKeys(ContextHandler.getCurrentContext()).contains(currentKeyword.value())){
                errorOccured = true;
                String expectedKeywords = Arrays.toString(lastKeyword.getValidNextKeys(ContextHandler.getCurrentContext()).toArray());
                error += String.format(fileMessage, displayErrorType(ErrorTypes.SYNTAX_ERROR), currentFile) + ":" + lineNumber + ":" + lineIndex + ":" + Constants.NEWLINE;
                error += String.format(lineIndexMessage, lineNumber, lineIndex) + Constants.NEWLINE;
                if (ContextHandler.insideOfContext()){
                    error += String.format(followingExpectedInContextGotMessage, lastToken, lastKeyword.value(), ContextHandler.getCurrentContext() , expectedKeywords,
                            currentToken, currentKeyword.value()) +
                            Constants.NEWLINE + Constants.NEWLINE;
                }
                else {
                    error += String.format(followingExpectedGotMessage, lastToken, lastKeyword.value(), expectedKeywords,
                            currentToken, currentKeyword.value()) +
                            Constants.NEWLINE + Constants.NEWLINE;
                }
                outputError(error);
            }
        }
        ContextHandler.tryEnterContext(currentKeyword.value());
        ContextHandler.tryExitingContext(currentKeyword.value());
        lastKeyword = currentKeyword;
        lastToken = currentToken;
        return error.isEmpty();
    }

    public void checkSyntaxInsideBlock(String blockKeyword, List<String> cobolLines, TokenExtractor tokenExtractor, String currentFile, int lineNumber) {
        int revertedCount = cobolLines.size();
        for (String line : cobolLines){
            List<String> keywords = tokenExtractor.extractTokensFrom(line);
            for(String keyword : keywords){
                if (cobolCheckStartingAndEndingKeywords.contains(keyword.toUpperCase(Locale.ROOT))){
                    errorOccured = true;
                    String error = "";
                    lineNumber = lineNumber - revertedCount;
                    int index = line.indexOf(keyword) + 1;
                    error += String.format(fileMessage, displayErrorType(ErrorTypes.SYNTAX_ERROR), currentFile) + ":" + lineNumber + ":" + index + ":" + Constants.NEWLINE;
                    error += String.format(lineIndexMessage, lineNumber, index) + Constants.NEWLINE;
                    error += String.format(keywordInBlock, keyword, blockKeyword) + Constants.NEWLINE + Constants.NEWLINE;
                    outputError(error);
                }
            }
            revertedCount -=1;
        }
    }

    public void logIdenticalMocks(Mock mock){
        String error = "";
        errorOccured = true;
        int lineNumber = mock.getDeclarationLineNumberInOriginalFile();
        int lineIndex = mock.getDeclarationIndexNumberInOriginalFile();
        error += String.format(fileMessage, displayErrorType(ErrorTypes.RUNTIME_ERROR), mock.getTestSuiteFileName()) + ":" + lineNumber + ":" + lineIndex + ":" + Constants.NEWLINE;
        error += String.format(lineIndexMessage, lineNumber, lineIndex) + Constants.NEWLINE;
        String message = "Mock <" + mock.getIdentifier() + "> already exists " +
                (mock.getArguments().isEmpty() ? "" : "with the given arguments ") + "in this " +
                mock.getScope().name() + ((mock.getScope() == MockScope.Local) ? " testcase " : " testsuite ") +
                "scope";
        error += message + Constants.NEWLINE + Constants.NEWLINE;
        outputError(error);
    }

    public void logVerifyReferencesNonExistentMock(VerifyMockCount verify) {
        String error = "";
        errorOccured = true;
        int lineNumber = verify.getDeclarationLineNumberInOriginalFile();
        int lineIndex = verify.getDeclarationIndexNumberInOriginalFile();
        error += String.format(fileMessage, displayErrorType(ErrorTypes.RUNTIME_ERROR), verify.getTestSuiteFileName()) + ":" + lineNumber + ":" + lineIndex + ":" + Constants.NEWLINE;
        error += String.format(lineIndexMessage, lineNumber, lineIndex) + Constants.NEWLINE;
        String message = "Verify references non existent mock. Mock does not exist for:  " + verify.getType() + " " + verify.getIdentifier() +
                ((verify.getArguments().isEmpty()) ? " with no arguments" : " with the given arguments");
        error += message + Constants.NEWLINE + Constants.NEWLINE;
        outputError(error);
    }

    public void logUnusedMocks(List<Mock> mocks){
        for (Mock mock : mocks){
            if (!mock.isUsed()){
                String error = "";
                int lineNumber = mock.getDeclarationLineNumberInOriginalFile();
                int lineIndex = mock.getDeclarationIndexNumberInOriginalFile();
                error += String.format(fileMessage, displayErrorType(ErrorTypes.WARNING), mock.getTestSuiteFileName()) + ":" + lineNumber + ":" + lineIndex + ":" + Constants.NEWLINE;
                error += String.format(lineIndexMessage, lineNumber, lineIndex) + Constants.NEWLINE;
                error += "Mock <" +  mock.getType() + "> <" + mock.getIdentifier() + "> does not reference " +
                        "any construct in the source code" + Constants.NEWLINE + Constants.NEWLINE;
                outputError(error);
            }
        }
    }

    public void logVariableTypeMismatch(String expectedType, String actualType, String currentFile, int lineNumber, int lineIndex){
        String error = "";
        error += String.format(fileMessage, displayErrorType(ErrorTypes.WARNING), currentFile) + ":" + lineNumber + ":" + lineIndex + ":" + Constants.NEWLINE;
        error += String.format(lineIndexMessage, lineNumber, lineIndex) + Constants.NEWLINE;
        error += "Expected compare to be of type <" + expectedType +">, but the variable was classified as the type <" + actualType + ">" + Constants.NEWLINE;
        error += "The test was carried out with the compare type <" + actualType + ">" + Constants.NEWLINE + Constants.NEWLINE;
        outputError(error);
    }

    private void outputError(String error) {
        errorLogMessages += error;
        System.out.println(error);
        BufferedWriter errorLogWriter = null;
        try {
            errorLogWriter = (BufferedWriter) EncodingIO.getWriterWithCorrectEncoding(errorLogPath, true);
            errorLogWriter.write(error);
            errorLogWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                errorLogWriter.close();
            } catch (Exception e) {
                System.out.println("TestSuite error log file could not be closed: " + e.getMessage());
            }
        }
    }

    private String displayErrorType(ErrorTypes errorType){
        return errorType.name().replace("_", " ");
    }

    /**
     * Gets the path for the TestSuite Parser error log
     */
    private String getTestSuiteParserErrorLogPath(){
        StringBuilder testSuiteParserErrorLogPath = new StringBuilder();
        String configPath = Config.getTestsuiteparserErrorLogPath();
        if (configPath == null)
            configPath = "." + Constants.FILE_SEPARATOR;
        if (configPath.equals("." + Constants.FILE_SEPARATOR))
            testSuiteParserErrorLogPath.append(new File(Constants.EMPTY_STRING).getAbsolutePath());
        else
            testSuiteParserErrorLogPath.append(new File(configPath));

        testSuiteParserErrorLogPath.append(Constants.FILE_SEPARATOR);
        testSuiteParserErrorLogPath.append(Config.getTestsuiteparserErrorLogName());
        return testSuiteParserErrorLogPath.toString();
    }

    /**
     * Returns a Writer for the TestSuite Parser error log.
     */
    private void initializeTestSuiteErrorLogWriter(String path){
        String testSuiteParserErrorLogPath = path;
        Writer testSourceWriter = null;
        try {
            File testSuiteParserErrorLogFile = new File(testSuiteParserErrorLogPath);
            if (testSuiteParserErrorLogFile.exists())
                testSuiteParserErrorLogFile.delete();

            Writer filecreator = new FileWriter(testSuiteParserErrorLogPath);
            filecreator.close();
            testSourceWriter = EncodingIO.getWriterWithCorrectEncoding(testSuiteParserErrorLogPath);
            FilePermission.setFilePermissionForAllUsers(testSuiteParserErrorLogPath, Config.getGeneratedFilesPermissionAll());
            Log.info(Messages.get("INF014", testSuiteParserErrorLogPath));
            testSourceWriter.close();
        } catch (IOException testSourceOutException) {
            System.out.println(testSourceOutException.getMessage());
        }
    }
}
