package com.neopragma.cobolcheck.features.testSuiteParser;

import java.util.ArrayList;
import java.util.List;

public class MockRepository {

    private List<Mock> mocks = new ArrayList<>();

    public void addMock(Mock mock){
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


}
