package org.openmainframeproject.cobolcheck.features.testSuiteParser;

public class Test {
    private String testSuiteName;
    private String testCaseName;
    private int testSuiteNumber;
    private int testCaseNumber;
    private MockScope scope;

    public Test(int testSuiteNumber, String testSuiteName, int testCaseNumber, String testCaseName) {
        this.testSuiteName = testSuiteName;
        this.testCaseName = testCaseName;
        this.testSuiteNumber = testSuiteNumber;
        this.testCaseNumber = testCaseNumber;
        if (testCaseNumber == 0) {
            scope = MockScope.Global;
        } else {
            scope = MockScope.Local;
        }
    }

    public String getTestSuiteName() {
        return testSuiteName;
    }

    public String getTestCaseName() {
        return testCaseName;
    }

    public int getTestSuiteNumber() {
        return testSuiteNumber;
    }

    public int getTestCaseNumber() {
        return testCaseNumber;
    }

    public MockScope getScope() {
        return scope;
    }
}
