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

    public boolean mockExistsFor(String identifier){
        for (Mock mock: mocks) {
            if (mock.getIdentifier().equals(identifier)){
                return true;
            }
        }
        return false;
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

    public List<Mock> getMocks(){
        return mocks;
    }
}
