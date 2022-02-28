package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Config;

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
        lines.add(line);
    }

    public void addLines(List<String> lines) {
        this.lines.addAll(lines);
    }

    public void addArgument(String argument) {arguments.add(argument);}

    private String getArgumentText(){
        String combinedArgs = "";
        for (String arg : arguments){
            combinedArgs += arg + ", ";
        }
        return combinedArgs.substring(0, combinedArgs.length() - 2);
    }
}
