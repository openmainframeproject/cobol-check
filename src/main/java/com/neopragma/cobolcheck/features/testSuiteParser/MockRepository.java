package com.neopragma.cobolcheck.features.testSuiteParser;

import com.neopragma.cobolcheck.exceptions.ComponentMockedTwiceInSameScopeException;

import java.util.ArrayList;
import java.util.List;

public class MockRepository {

    private List<Mock> mocks = new ArrayList<>();

    public void addMock(Mock mock){

        if (mockAlreadyExist(mock)){
            throw new ComponentMockedTwiceInSameScopeException("Mock for " + mock.getIdentifier() + " in " +
                    "testsuite: " + mock.getTestSuiteName() + ", testcase: " +
                    ((mock.getTestCaseName().equals("")) ? "N/A" : mock.getTestCaseName()) +
                    " already exists in this "+ mock.getScope().name() +
                    ((mock.getScope() == MockScope.Local) ? " testcase " : " testsuite ") + "scope.");
        }
        mocks.add(mock);
    }

    public List<Mock> getMocks(){
        return mocks;
    }

    public boolean mockExistsFor(String identifier){
        for (Mock mock: mocks) {
            if (mock.getIdentifier().equals(identifier)){
                return true;
            }
        }
        return false;
    }

    public Mock getMockFor(String identifier, String testSuite, String testCase){
        List<Mock> globalMocks = new ArrayList<>();
        for (Mock mock: mocks) {
            if (mock.getIdentifier().equals(identifier)){
                if (mock.getScope() == MockScope.Local){
                    if (mock.getTestSuiteName().equals(testSuite) && mock.getTestCaseName().equals(testCase)){
                        return mock;
                    }
                }
                if (mock.getScope() == MockScope.Global){
                    //We need to check all local mocks first, and save global mocks for later
                    globalMocks.add(mock);
                }
            }
        }
        for (Mock mock: globalMocks) {
            if (mock.getTestSuiteName().equals(testSuite)){
                return mock;
            }
        }


        return null;
    }

    private boolean mockAlreadyExist(Mock mock){
        for(Mock m : mocks){
            if (mock.getIdentifier().equals(m.getIdentifier())
                    && mock.getScope() == m.getScope()
                    && mock.getTestSuiteName().equals(m.getTestSuiteName())
                    && mock.getTestCaseName().equals(m.getTestCaseName())){
                return true;
            }
        }
        return false;
    }


}
