package com.neopragma.cobolcheck.features.testSuiteParser;

import java.util.ArrayList;
import java.util.List;

public class MockRepository {

    private List<Mock> mocks = new ArrayList<>();

    public void addMock(Mock mock){
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

    public List<Mock> getMocks(){
        return mocks;
    }
}
