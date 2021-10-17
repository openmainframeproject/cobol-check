package com.neopragma.cobolcheck.features.prepareMerge;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.TestSuiteConcatenator;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.filehelpers.PathHelper;

import java.io.*;

public class IO_FileGetter {

    /**
     * Returns a reader for the cobol source file.
     *
     * @param programName - The name of the file its reading from.
     */
    static Reader getSourceReader(String programName){
        String cobolSourceInPath = PathHelper.getCobolSourceDirectory() + programName;
        cobolSourceInPath = PathHelper.appendMatchingFileSuffix(cobolSourceInPath, Config.getApplicationFilenameSuffixes());
        cobolSourceInPath = StringHelper.adjustPathString(cobolSourceInPath);

        Reader sourceReader;
        try {
            sourceReader = new FileReader(cobolSourceInPath);
        } catch (IOException cobolSourceInException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR018", programName));
        }
        return sourceReader;
    }

    /**
     * Returns a reader for the testSuites in a directory
     *
     * @param testDirectory - The directory of the test files.
     */
    static Reader getTestSuiteReader(String testDirectory, String testFileNames){
        TestSuiteConcatenator concatenator = new TestSuiteConcatenator(testFileNames);
        return concatenator.concatenateTestSuites(testDirectory);
    }


    /**
     * Returns a Writer for the output file.
     *
     * @param sourceFile - The name of the cobol source.
     */
    static Writer getTestSourceWriter(String sourceFile){
        String testSourceOutPath = PathHelper.getTestSourceOutPath();
        Writer testSourceWriter;
        try {
            testSourceWriter = new FileWriter(testSourceOutPath);
        } catch (IOException testSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR016", sourceFile));
        }
        return testSourceWriter;
    }
}
