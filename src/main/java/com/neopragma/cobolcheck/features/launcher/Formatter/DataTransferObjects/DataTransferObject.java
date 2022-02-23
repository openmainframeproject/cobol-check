package com.neopragma.cobolcheck.features.launcher.Formatter.DataTransferObjects;

public abstract class DataTransferObject {
    int testSuiteIndex = -1;
    int testCaseIndex = -1;

    public void moveToNextTestSuite(){
        testSuiteIndex++;
        testCaseIndex = -1;
    }

    public void moveToNextTestCase(){
        testCaseIndex++;
    }

    public abstract Object getDataTransferObject();

    public abstract void setNumberOfAllTests(String numberofTests);

    public abstract void setNumberOffAllFailures(String numberOfFailures);

    public abstract void setCurrentTestSuiteName(String name);

    public abstract void setCurrentTestSuiteTests(String numberofTests);

    public abstract void setCurrentTestSuiteFailures(String numberOfFailures);

    public abstract void setCurrentTestSuitePackage(String testSuitePackage);

    public abstract void setCurrentTestCaseName(String name);

    public abstract void setCurrentTestCaseFailure(String message, String type);

    public abstract void setCurrentTestCaseErrorMessage(String message, String type);
}
