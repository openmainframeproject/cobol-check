package com.neopragma.cobolcheck.features.concatenator;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Messages;

import java.io.BufferedReader;
import java.io.Reader;

public class ConcatenatorController {

    TestSuiteConcatenator testSuiteConcatenator;

    public ConcatenatorController(String testFileNames){
        testSuiteConcatenator = new TestSuiteConcatenator(testFileNames);
    }

    public Reader concatenateTestSuites(String programTestSuiteSubdirectory) {
        Reader testSuiteReader = testSuiteConcatenator.concatenateTestSuites(programTestSuiteSubdirectory);
        if (testSuiteReader == null) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        return testSuiteReader;
    }
}

