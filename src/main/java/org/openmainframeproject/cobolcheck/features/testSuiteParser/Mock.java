package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.StringHelper;

import java.util.ArrayList;
import java.util.List;

public class Mock {
    private String identifier;
    private String type;
    private List<String> lines;
    private List<String> arguments;
    private MockScope scope;
    private boolean isUsed;
    private String testSuiteName;
    private String testCaseName;
    private int testSuiteNumber;
    private int testCaseNumber;
    private int mockNumber;

    private String testSuiteFileName;
    private int declarationLineNumberInOriginalFile;
    private int declarationIndexNumberInOriginalFile;

    public Mock(String testSuiteName, String testCaseName, int testSuiteNumber, int testCaseNumber, int mockNumber) {
        this.testSuiteName = testSuiteName;
        this.testCaseName = testCaseName;
        this.testSuiteNumber = testSuiteNumber;
        this.testCaseNumber = testCaseNumber;
        this.mockNumber = mockNumber;
        lines = new ArrayList<>();
        arguments = new ArrayList<>();
    }

    public String getGeneratedMockIdentifierRoot(){
        return testSuiteNumber + "-" + testCaseNumber + "-" + mockNumber + "-MOCK";
    }

    public String getGeneratedMockIdentifier(){
        return Config.getTestCodePrefix() + getGeneratedMockIdentifierRoot();
    }

    public String getGeneratedMockCountIdentifier(){
        return Config.getTestCodePrefix() + getGeneratedMockIdentifierRoot() + "-COUNT";
    }

    public String getGeneratedMockCountExpectedIdentifier(){
        return Config.getTestCodePrefix() + getGeneratedMockIdentifierRoot() + "-EXPECTED";
    }

    public String getGeneratedMockStringIdentifierName(){
        return Config.getTestCodePrefix() + getGeneratedMockIdentifierRoot() + "-NAME";
    }

    public String getMockDisplayString(){
        return type.toUpperCase() + " " + identifier;
    }

    public String getMockDescription(){
        return type + " " + identifier + " in testsuite: " + testSuiteName +
                (testCaseName.equals("") ? "" : ", testcase: " + testCaseName);
    }

    public List<String> getCommentText(){
        List<String> lines = new ArrayList<>();
        lines.add("      *****************************************************************");
        lines.add(scope.name() + " mock of: " + type + ": " + identifier);
        if (!arguments.isEmpty()){
            lines.add("With args: " + getArgumentText());
        }
        lines.add("In testsuite: " + testSuiteName);
        if (scope == MockScope.Local){
            lines.add("In testcase: " + testCaseName);
        }
        lines.add("      *****************************************************************");
        return lines;

    }

    public String getIdentifier() {
        return identifier;
    }

    public String getType() {
        return type;
    }

    public List<String> getLines() {
        return lines;
    }

    public List<String> getArguments() {return arguments;}

    public MockScope getScope() {
        return scope;
    }

    public boolean isUsed() {
        return isUsed;
    }

    public int getTestSuiteNumber() {
        return testSuiteNumber;
    }

    public int getTestCaseNumber() {
        return testCaseNumber;
    }
    
    public String getTestSuiteName() {
        return testSuiteName;
    }

    public String getTestCaseName() {
        return testCaseName;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setScope(MockScope scope) {
        this.scope = scope;
    }

    public void markAsUsed() {
        isUsed = true;
    }

    public void addLine(String line) {
        lines.add(StringHelper.moveToAreaB(line));
    }

    public void addLines(List<String> lines) {
        this.lines.addAll(StringHelper.moveToAreaB(lines));
    }

    public void addArgument(String argument) {arguments.add(argument);}

    public String getTestSuiteFileName() {
        return testSuiteFileName;
    }

    public void setTestSuiteFileName(String testSuiteFileName) {
        this.testSuiteFileName = testSuiteFileName;
    }

    public int getDeclarationLineNumberInOriginalFile() {
        return declarationLineNumberInOriginalFile;
    }

    public void setDeclarationLineNumberInOriginalFile(int declarationLineNumberInOriginalFile) {
        this.declarationLineNumberInOriginalFile = declarationLineNumberInOriginalFile;
    }

    public int getDeclarationIndexNumberInOriginalFile() {
        return declarationIndexNumberInOriginalFile;
    }

    public void setDeclarationIndexNumberInOriginalFile(int declarationIndexNumberInOriginalFile) {
        this.declarationIndexNumberInOriginalFile = declarationIndexNumberInOriginalFile;
    }

    private String getArgumentText(){
        String combinedArgs = "";
        for (String arg : arguments){
            combinedArgs += arg + ", ";
        }
        return combinedArgs.substring(0, combinedArgs.length() - 2);
    }

}
