package org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects;

import org.openmainframeproject.cobolcheck.services.RunInfo;

import java.util.ArrayList;
import java.util.List;

public abstract class DataTransferObject {
    int testSuiteIndex = -1;
    int testCaseIndex = -1;

    TestInstance testInstance = new TestInstance();

    public void moveToNextTestSuite() {
        testSuiteIndex++;
        testCaseIndex = -1;
        testInstance.addTestSuite(new TestSuite());
    }

    public void moveToNextTestCase() {
        testCaseIndex++;
        testInstance.getTestSuites().get(testSuiteIndex).addTestCase(new TestCase());
    }

    public Object getDataTransferObject() {
        setCounts();
        return testInstance;
    }

    public void setNumberOfAllTests(String numberofTests) {
        testInstance.testCaseNumber += Integer.valueOf(numberofTests);
    }

    public void setNumberOffAllFailures(String numberOfFailures) {
        testInstance.testCaseNumberFailed += Integer.valueOf(numberOfFailures);
    }

    public void setCurrentTestSuiteName(String name)
    {
        testInstance.getTestSuites().get(testSuiteIndex).setName(name);
        testInstance.getTestSuites().get(testSuiteIndex).setProgramName(RunInfo.getCurrentProgramName());
        testInstance.getTestSuites().get(testSuiteIndex).setProgramPath(RunInfo.getCurrentProgramPath());
        testInstance.getTestSuites().get(testSuiteIndex).setPath(RunInfo.getTestSuitePath(name));
    }

    public void setCurrentTestSuiteTests(String numberofTests) {
        //Gotten from testcase count
    }

    public void setCurrentTestSuiteFailures(String numberOfFailures) {
        //cannot get value here
    }

    public void setCurrentTestSuitePackage(String testSuitePackage) {
        //Not used in this context
    }

    public void setCurrentTestCaseName(String name) {
        testInstance.getTestSuites().get(testSuiteIndex).getTestCases().get(testCaseIndex).setName(name);
    }

    public void setCurrentTestCaseFailure(String message, String type) {
        testInstance.getTestSuites().get(testSuiteIndex).getTestCases().get(testCaseIndex).setPassed(false);
        testInstance.getTestSuites().get(testSuiteIndex).getTestCases().get(testCaseIndex).setFailMessage(message);
    }

    String lastProgram = "";
    public void setCounts(){
        for (TestSuite testSuite : testInstance.getTestSuites()){
            if (!testSuite.programName.equals(lastProgram)){
                testInstance.testingPrograms += testSuite.getProgramName() + " & ";
                lastProgram = testSuite.programName;
            }
            for (TestCase testCase : testSuite.getTestCases()){
                if (!testCase.didPass()){
                    if (testSuite.getFailedTests() == 0){
                        testInstance.incrementFailedTestSuites();
                    }
                    testSuite.incrementFailedTests();
                }

            }
        }
        if (testInstance.testingPrograms.length() > 2){
            testInstance.testingPrograms = testInstance.testingPrograms.
                    substring(0, testInstance.testingPrograms.length() - 3);
        }
    }

    public void setCurrentTestCaseErrorMessage(String message, String type) {
        //Not used here
    }

    class TestInstance{
        private List<TestSuite> testSuites = new ArrayList<>();
        private String testingPrograms = "";
        private int testSuiteNumber = 0;
        private int testSuiteNumberFailed = 0;
        private int testCaseNumber = 0;
        private int testCaseNumberFailed = 0;

        public List<TestSuite> getTestSuites() {
            return testSuites;
        }

        public void addTestSuite(TestSuite testSuite) {
            testSuites.add(testSuite);
        }

        public String getTestingPrograms() { return testingPrograms; }

        public int getTestSuiteNumber() {
            return testSuites.size();
        }

        public int getTestSuiteNumberPassed(){
            return getTestSuiteNumber() - testSuiteNumberFailed;
        }

        public int getTestSuiteNumberFailed() {
            return testSuiteNumberFailed;
        }

        public void incrementFailedTestSuites() {
            testSuiteNumberFailed++;
        }

        public int getTestCaseNumber() {
            return testCaseNumber;
        }

        public int getTestCaseNumberPassed(){
            return getTestCaseNumber() - testCaseNumberFailed;
        }

        public int getTestCaseNumberFailed() {
            return testCaseNumberFailed;
        }

    }

    class TestSuite{
        private String name;
        private int failedTests = 0;
        private List<TestCase> testCases = new ArrayList<>();
        private String programName;
        private String programPath;
        private String path;


        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public int getTests() {
            return testCases.size();
        }

        public int getPassedTests(){
            return getTests() - failedTests;
        }

        public int getFailedTests() {
            return failedTests;
        }

        public void incrementFailedTests() {
            failedTests++;
        }

        public List<TestCase> getTestCases() {
            return testCases;
        }

        public void addTestCase(TestCase testCase) {
            testCases.add(testCase);
        }

        public String getProgramName() {
            return programName;
        }

        public void setProgramName(String programName) {
            this.programName = programName;
        }

        public String getProgramPath() {
            return programPath;
        }

        public void setProgramPath(String programPath) {
            this.programPath = programPath;
        }

        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }
    }

    class TestCase{
        String name;
        boolean passed = true;
        String failMessage;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public boolean didPass() {
            return passed;
        }

        public void setPassed(boolean passed) {
            this.passed = passed;
        }

        public String getFailMessage() {
            return failMessage;
        }

        public void setFailMessage(String failMessage) {
            this.failMessage = failMessage;
        }
    }
}
