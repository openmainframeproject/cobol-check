package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.exceptions.ConcatenatedTestSuiteIOException;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.exceptions.TestSuiteInputFileNotFoundException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.cobolLogic.replace.Replace;
import org.openmainframeproject.cobolcheck.services.filehelpers.EncodingIO;
import org.openmainframeproject.cobolcheck.services.filehelpers.FileNameMatcher;
import org.openmainframeproject.cobolcheck.services.filehelpers.FilePermission;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * This class is responsible for resolving command-line specifications for -t or --test-suite-path
 * and config test.suite.path and for concatenating the test suite input files into a single source
 * for the Generator to consume.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class TestSuiteConcatenator {

    private String testFileNames;

    public TestSuiteConcatenator(String testFileNames) {
        this.testFileNames = testFileNames;
    }

    /**
     * Multiple test suite files may be input to Cobol Check. They are located in the directory specified in config
     * setting test.suite.directory. Under that directory Cobol Check expects to find one subdirectory for each program
     * to be tested. Test suite files are located under those subdirectories.
     *
     * The various test suite input files are concatenated into a single file for the Generator to process.
     * In case of an IOException on this file, we throw ConcatenatedTestSuiteIOException and provide info to
     * help correct the error.
     *
     * This:
     *
     * cobolcheck --programs ALPHA BETA --tests *-test
     *
     * Comes in on separate calls as:
     *
     * [testSuiteDirectory]/ALPHA/
     * [testSuiteDirectory]/BETA/
     *
     * And means:
     *
     * Run all testsuites under [testSuiteDirectory]/ALPHA whose names match *-test against program ALPHA.CBL
     *
     * and (separately)
     *
     * Run all testsuites under [testSuiteDirectory]/BETA whose names match *-test against program BETA.CBL.
     *
     *
     * @return Reader for the concatenated test suites
     * @throws ConcatenatedTestSuiteIOException
     * @throws TestSuiteInputFileNotFoundException
     * @throws PossibleInternalLogicErrorException
     */
    Reader concatenateTestSuites(String programTestSuiteSubdirectory) {
        String[] testFileNamesSeparated = testFileNames.split(Constants.COLON);

        // find files under subdirectory
        List<String> matchingFiles = new ArrayList<>();
        for (String testFileNameGlob : testFileNamesSeparated) {
            FileNameMatcher fileFinder = new FileNameMatcher(testFileNameGlob);
            try {
                Files.walkFileTree(Paths.get(programTestSuiteSubdirectory), fileFinder);
                matchingFiles = fileFinder.getMatchingFiles();
                if (matchingFiles.isEmpty()) {
                    Log.warn(Messages.get("WRN002", programTestSuiteSubdirectory));
                }
            } catch (IOException exceptionWalkingFiles) {
                throw new PossibleInternalLogicErrorException(
                        Messages.get("ERR013", testFileNameGlob),
                        exceptionWalkingFiles);
            }
        }

        // concatenate matching test suite files into a single test input file for the Generator to consume
        String concatenatedTestSuiteFileName = Config.getConcatenatedTestSuitesPath();
        Writer concatenatedTestSuitesWriter = null;
        try {
            File concattenatedTestSuiteFile = new File(concatenatedTestSuiteFileName);
            if (concattenatedTestSuiteFile.exists())
                concattenatedTestSuiteFile.delete();
            concatenatedTestSuitesWriter = EncodingIO.getWriterWithCorrectEncoding(concatenatedTestSuiteFileName);
        } catch (IOException concatenatedTestSuitesException) {
            throw new ConcatenatedTestSuiteIOException(
                    Messages.get("ERR012", concatenatedTestSuiteFileName),
                    concatenatedTestSuitesException);
        }
        FilePermission.setFilePermissionForAllUsers(concatenatedTestSuiteFileName, Config.getGeneratedFilesPermissionAll());

        try {
            for (String matchingFile : matchingFiles) {
                BufferedReader testFileReader = new BufferedReader(EncodingIO.getReaderWithCorrectEncoding(matchingFile));
                String line = Constants.EMPTY_STRING;
                concatenatedTestSuitesWriter.write(StringHelper.commentOutLine("From file: " + matchingFile) + Constants.NEWLINE);
                while((line = testFileReader.readLine()) != null) {
                    concatenatedTestSuitesWriter.write(Replace.replace(line) + Constants.NEWLINE);
                }
                testFileReader.close();
            }
            concatenatedTestSuitesWriter.close();
        } catch (IOException testFileReaderException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR014", programTestSuiteSubdirectory),
                    testFileReaderException);
        }

        // return the concatenated test suite file as a Reader
        Reader testSuite = null;
        try {
            testSuite = EncodingIO.getReaderWithCorrectEncoding(concatenatedTestSuiteFileName);
        } catch (IOException exceptionCreatingTestSuiteReader) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR015"));
        }

        return testSuite;
    }
}
