package org.openmainframeproject.cobolcheck.features.prepareMerge;

import org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper;

import java.io.Reader;
import java.io.Writer;
import java.util.List;

public class PrepareMergeController {

    /**
     * @param programName - The name of the file its reading from.
     * @return a reader for the cobol source file.
     */
    public static Reader getSourceReader(String programName){
        return IO_FileGetter.getSourceReader(programName);
    }

    /**
     * @param sourceFile - The name of the cobol source.
     * @return a Writer for the output file.
     */
    public static Writer getTestSourceWriter(String sourceFile){
        return IO_FileGetter.getTestSourceWriter(sourceFile);
    }

    /**
     * @return the path for the output test source
     */
    public static String getTestSourceOutPath(){
        return PathHelper.getTestSourceOutPath();
    }

    /**
     * Looks through the test directory, to find folders, that matches the name of the
     * source program name given as a parameter.
     *
     * @param programName - The name of the source cobol program
     * @return A list of the found folder paths
     */
    public static List<String> getMatchingTestDirectoriesForProgram(String programName) {
        return IO_FileGetter.getMatchingTestDirectoriesForProgram(programName);
    }
}
