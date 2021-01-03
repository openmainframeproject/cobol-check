/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.ConcatenatedTestSuiteIOException;
import com.neopragma.cobolcheck.exceptions.TestSuiteInputFileNotFoundException;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
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
public class TestSuiteConcatenator implements Constants, StringHelper {
    private static final String PROGRAMS_OPTION = "programs";
    private static final String TESTS_OPTION = "tests";
    private static final String CONCATENATED_TEST_SUITES_CONFIG_KEY = "concatenated.test.suites";
    private static final String DEFAULT_CONCATENATED_TEST_SUITES_PATH = "./ALLTESTS";

    private Config config;
    private Messages messages;
    private GetOpt options;
    private BufferedReader testSuite;

    public TestSuiteConcatenator(Config config, GetOpt options) {
        this.config = config;
        this.messages = config.getMessages();
        this.options = options;
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
     * cobolcheck --tests ALPHA/*-test ALPHA/*-unit BETA DELTA/first DELTA/second
     *
     * Comes in as:
     *
     * ALPHA/*-test:ALPHA/*-unit:BETA:DELTA/first:DELTA/second
     *
     * And means:
     *
     * Run all testsuites under ALPHA whose names match *-test or *-unit
     * plus all test suites under BETA
     * plus all the first and second test suites under DELTA
     *
     * @return Reader for the concatenated test suites
     * @throws ConcatenatedTestSuiteIOException
     * @throws TestSuiteInputFileNotFoundException
     */
    Reader concatenateTestSuites() {
        String concatenatedTestSuiteFileName =
                config.getString(CONCATENATED_TEST_SUITES_CONFIG_KEY,
                        DEFAULT_CONCATENATED_TEST_SUITES_PATH);
        FileWriter concatenatedTestSuitesWriter;
        try {
            concatenatedTestSuitesWriter = new FileWriter(concatenatedTestSuiteFileName);
        } catch (IOException concatenatedTestSuitesException) {
            throw new ConcatenatedTestSuiteIOException(
                    messages.get("ERR012", concatenatedTestSuiteFileName),
                    concatenatedTestSuitesException);
        }

        String testDirectory =
                config.getString(TEST_SUITE_DIRECTORY_CONFIG_KEY, Constants.CURRENT_DIRECTORY);
        if (!testDirectory.endsWith(FILE_SEPARATOR)) {
            testDirectory += FILE_SEPARATOR;
        }

        System.out.println("testDirectory: " + testDirectory);

        String programNames = options.getValueFor(PROGRAMS_OPTION);
        String testFileNames = options.getValueFor(TESTS_OPTION);

        System.out.println(NEWLINE);
        System.out.println("programNames: " + programNames);
        System.out.println("testFileNames: " + testFileNames);

        String[] programNamesSeparated = programNames.split(COLON);

        System.out.println(NEWLINE);
        for (String name : programNamesSeparated) {
            System.out.println("programName: " + name);
        }


        String[] testFileNamesSeparated = testFileNames.split(COLON);

        System.out.println(NEWLINE);
        for (String name : testFileNamesSeparated) {
            System.out.println("separatedName: " + name);
        }

        // find subdirectories that match program names
        List<String> matchingDirectories = new ArrayList<>();
        for (String programName : programNamesSeparated) {
            DirectoryNameMatcher directoryFinder = new DirectoryNameMatcher(programNamesSeparated[0]);
            try {
                Files.walkFileTree(Paths.get(testDirectory), directoryFinder);
                matchingDirectories = directoryFinder.getMatchingDirectories();
            } catch (IOException ex) {
                //TODO: Issue warning here - program name specified on command line doesn't have a test directory
            }

            for (String matchingDirectory : matchingDirectories) {
                System.out.println("matchingDirectory: " + matchingDirectory);
            }
        }

        // find files under subdirectory
        List<String> matchingFiles = new ArrayList<>();
        for (String testFileNameGlob : testFileNamesSeparated) {
            FileNameMatcher fileFinder = new FileNameMatcher(testFileNamesSeparated[0]);
            try {
                Files.walkFileTree(Paths.get(matchingDirectories.get(0)), fileFinder);
                matchingFiles = fileFinder.getMatchingFiles();
            } catch (IOException ex) {
                //TODO: Issue warning here - filename specified on command line doesn't exist in directory
            }

            for (String matchingFile : matchingFiles) {
                System.out.println("matchingFile: " + matchingFile);
            }
        }




//        String colonDelimitedTestFileNamesFromCommandLine = EMPTY_STRING;
//        if (options.isSet(TESTS_OPTION)) {
//            colonDelimitedTestFileNamesFromCommandLine = options.getValueFor(TESTS_OPTION);
//        }
//        String[] testFileNames = colonDelimitedTestFileNamesFromCommandLine.split(COLON);
//        String[] expandedTestFileNames = new String[testFileNames.length];
//        int nextIndex = 0;
//
//        for (String testFileName : testFileNames) {
//            expandedTestFileNames[nextIndex] = testDirectory + testFileName;
//
//            System.out.println("expandedTestFileName: " + expandedTestFileNames[nextIndex]);
//
//            nextIndex++;
//        }
//
//        String line;
//        for (String pathname : expandedTestFileNames) {
//            try {
//                Log.info(messages.get("INF007", pathname, concatenatedTestSuiteFileName));
//                testSuite = new BufferedReader(new FileReader(pathname));
//                while ((line = testSuite.readLine()) != null) {
//                    concatenatedTestSuitesWriter.write(line + NEWLINE);
//                }
//            } catch (IOException testSuiteNotFound) {
//                throw new TestSuiteInputFileNotFoundException(
//                        messages.get("ERR011", pathname),
//                        testSuiteNotFound);
//            }
//        }
//        FileReader testSuite;
//        try {
//            concatenatedTestSuitesWriter.close();
//            testSuite =  new FileReader(concatenatedTestSuiteFileName);
//        } catch (IOException concatenatedTestSuitesException) {
//            throw new ConcatenatedTestSuiteIOException(
//                    messages.get("ERR012", concatenatedTestSuiteFileName),
//                    concatenatedTestSuitesException);
//        }
        return testSuite;
    }
}
