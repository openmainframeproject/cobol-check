package com.neopragma.cobolcheck.features.testSuiteParser;

import java.util.ArrayList;
import java.util.List;

public class Mock {
    private String identifier;
    private String type;
    private List<String> lines;
    private MockScope scope;
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
    }

    public String getGeneratedMockIdentifier(){
        return testSuiteNumber + "-" + testCaseNumber + "-" + mockNumber + "-MOCK";
    }

    public String getCommentText(){
        return scope.name() + "mock created from test case: " + testCaseName + " in test suite: " + testSuiteName +
                "for the " + type + ": " + identifier;
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

    public MockScope getScope() {
        return scope;
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

    public void addLine(String line) {
        lines.add(line);
    }

    public void addLines(List<String> lines) {
        this.lines.addAll(lines);
    }
}
