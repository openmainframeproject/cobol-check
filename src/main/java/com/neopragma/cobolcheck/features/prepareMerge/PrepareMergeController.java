package com.neopragma.cobolcheck.features.prepareMerge;

import com.neopragma.cobolcheck.services.filehelpers.PathHelper;

import java.io.Reader;
import java.io.Writer;

public class PrepareMergeController {


    public static Reader getSourceReader(String programName){
        return IO_FileGetter.getSourceReader(programName);
    }

    public static Reader getTestSuiteReader(String testDirectory, String testFileNames){
        return IO_FileGetter.getTestSuiteReader(testDirectory, testFileNames);
    }

    public static Writer getTestSourceWriter(String sourceFile){
        return IO_FileGetter.getTestSourceWriter(sourceFile);
    }

    public static String getTestSourceOutPath(){
        return PathHelper.getTestSourceOutPath();
    }
}
