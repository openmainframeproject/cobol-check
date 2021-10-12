package com.neopragma.cobolcheck.features.prepareMerge;

import com.neopragma.cobolcheck.services.filehelpers.PathHelper;

import java.io.Reader;
import java.io.Writer;
import java.util.List;

public class PrepareMergeController {


    public static Reader getSourceReader(String programName){
        return IO_FileGetter.getSourceReader(programName);
    }

    public static Writer getTestSourceWriter(String sourceFile){
        return IO_FileGetter.getTestSourceWriter(sourceFile);
    }

    public static String getTestSourceOutPath(){
        return PathHelper.getTestSourceOutPath();
    }

    public static List<String> getMatchingTestDirectoriesForProgram(String programName) {
        return IO_FileGetter.getMatchingTestDirectoriesForProgram(programName);
    }
}
