package com.neopragma.cobolcheck.features.Formatter.DataTransferObjects;

import com.neopragma.cobolcheck.services.cobolLogic.Interpreter;

public class DataTransferObjectAdapter {
    private DataTransferObjectStyle dataTransferObjectStyle;
    private JUnitDto jUnitDto;

    private int testSuiteIndex = -1;
    private int testCaseIndex = -1;

    public DataTransferObjectAdapter(DataTransferObjectStyle dataTransferObjectStyle){
        this.dataTransferObjectStyle = dataTransferObjectStyle;
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto = new JUnitDto();
        }
    }

    public Object getDataTransferObject(){
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.setTestCounts();
                return jUnitDto.getTestsuites();
        }
        return null;
    }

    public void moveToNextTestSuite(){
        testSuiteIndex++;
        testCaseIndex = -1;
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.getTestsuites().addTestSuite(new Testsuite());
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).setID(Integer.toString(testSuiteIndex));
        }
    }

    public void moveToNextTestCase(){
        testCaseIndex++;
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).addTestCase(new Testcase());
        }
    }

    public void setNumberOfAllTests(String numberofTests){
        switch (dataTransferObjectStyle){
//            case JUnit:
//                jUnitDto.getTestsuites().setTests(numberofTests);
        }
    }

    public void setNumberOffAllFailures(String numberOfFailures){
        switch (dataTransferObjectStyle){
//            case JUnit:
//                jUnitDto.getTestsuites().setFailures(numberOfFailures);
        }
    }

    public void setCurrentTestSuiteName(String name){
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).setName(name);
        }
    }

    public void setCurrentTestSuiteTests(String numberofTests){
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).setTests(numberofTests);
        }
    }

    public void setCurrentTestSuiteFailures(String numberOfFailures){
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).setFailures(numberOfFailures);
        }
    }

    public void setCurrentTestSuitePackage(String testSuitePackage) {
        jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).setTestsuitePackage(testSuitePackage);
    }

    public void setCurrentTestCaseName(String name){
        switch (dataTransferObjectStyle){
            case JUnit:
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex)
                        .setName(name);
        }
    }

    public void setCurrentTestCaseFailure(String message, String type){
        switch (dataTransferObjectStyle){
            case JUnit:
                Error failure = new Error();
                failure.setMessage(message);
                failure.setType(type);
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex)
                        .setFailure(failure);
        }
    }

//    public void setCurrentTestCaseResult(String result){
//        switch (dataTransferObjectStyle){
//            case JUnit:
//                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex)
//                        .setSystemOut(result);
//        }
//    }



    public void setCurrentTestCaseErrorMessage(String message, String type){
        switch (dataTransferObjectStyle){
            case JUnit:
                Error error = new Error();
                error.setMessage(message);
                error.setType(type);
                jUnitDto.getTestsuites().getTestsuites().get(testSuiteIndex).getTestcase().get(testCaseIndex)
                        .setError(error);
        }
    }
}
