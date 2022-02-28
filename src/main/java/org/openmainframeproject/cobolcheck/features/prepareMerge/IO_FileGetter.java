package org.openmainframeproject.cobolcheck.features.prepareMerge;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper.endWithFileSeparator;
import static org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper.getMatchingDirectories;

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

    /**
     * Looks through the test directory, to find folders, that matches the name of the
     * source program name given as a parameter.
     *
     * @param programName - The name of the source cobol program
     * @return A list of the found folder paths
     */
    static List<String> getMatchingTestDirectoriesForProgram(String programName){
        // all test suites are located under this directory
        String testSuiteDirectory = Config.getTestSuiteDirectoryPathString();
        testSuiteDirectory = endWithFileSeparator(testSuiteDirectory);

        // Find test subdirectories that match program names
        List<String> matchingDirectories;
        Path path = Paths.get(programName);
        programName = path.getFileName().toString();
        try{
            matchingDirectories = getMatchingDirectories(programName, testSuiteDirectory);
        } catch (IOException ioException) {
            throw new PossibleInternalLogicErrorException(Messages.get("ERR019", programName));
        }
        return matchingDirectories;
    }
}
