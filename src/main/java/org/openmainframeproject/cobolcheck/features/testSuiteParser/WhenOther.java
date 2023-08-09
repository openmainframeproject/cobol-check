package org.openmainframeproject.cobolcheck.features.testSuiteParser;


import java.util.ArrayList;
import java.util.List;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.StringHelper;

public class WhenOther {

    private String identifier;
    private String type;
    private List<String> lines;
    private MockScope scope;
    private String testSuiteName;
    private String testCaseName;
    private int testSuiteNumber;
    private int testCaseNumber;
    private int mockNumber;


    public WhenOther(String testSuiteName, String testCaseName, int testSuiteNumber, int testCaseNumber, int mockNumber) {
        this.testSuiteName = testSuiteName;
        this.testCaseName = testCaseName;
        this.testSuiteNumber = testSuiteNumber;
        this.testCaseNumber = testCaseNumber;
        this.mockNumber = mockNumber;
        lines = new ArrayList<>();
    }
    
    public String getGeneratedWhenOtherIdentifier(){
        return Config.getTestCodePrefix() + getGeneratedWhenOtherIdentifierRoot();
    }

    public String getGeneratedWhenOtherIdentifierRoot(){
        return testSuiteNumber + "-" + testCaseNumber + "-" + mockNumber + "-WO";
    }

    public void addLines(List<String> lines) {
        this.lines.addAll(lines);
    }

    public void setScope(MockScope scope) {
        this.scope = scope;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getType() {
        return this.type;
    }

    public List<String> getLines() {
        return this.lines;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }


    public List<String> getCommentText(){
        List<String> newLines = new ArrayList<>();
        newLines.add("      *****************************************************************");
        newLines.add( "When other block of: " + type + ": " + identifier);
        newLines.add("In testsuite: " + testSuiteName);
        if (scope == MockScope.Local){
            newLines.add("In testcase: " + testCaseName);
        }
        newLines.add("      *****************************************************************");
        return newLines;

    }

}
