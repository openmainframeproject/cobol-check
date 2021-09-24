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
package com.neopragma.cobolcheck.workers;

import com.neopragma.cobolcheck.features.Status.StatusController;
import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandler;
import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandlerController;
import com.neopragma.cobolcheck.features.environmentSetup.EnvironmentSetupController;
import com.neopragma.cobolcheck.features.parser.KeywordExtractor;
import com.neopragma.cobolcheck.features.TestSuiteConcatenator;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.launcher.LinuxProcessLauncher;
import com.neopragma.cobolcheck.features.launcher.ProcessLauncher;
import com.neopragma.cobolcheck.features.launcher.WindowsProcessLauncher;
import com.neopragma.cobolcheck.services.*;
import com.neopragma.cobolcheck.services.filehelpers.DirectoryNameMatcher;
import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.log.LogLevel;
import com.neopragma.cobolcheck.services.platform.Platform;
import com.neopragma.cobolcheck.services.platform.PlatformLookup;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;

import static com.neopragma.cobolcheck.services.filehelpers.PathHelper.endWithFileSeparator;
import static com.neopragma.cobolcheck.services.filehelpers.PathHelper.getMatchingDirectories;

/**
 * Main class for command-line execution.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Initializer {

    private ArgumentHandlerController argumentController;
    private EnvironmentSetupController environmentController;
    private StatusController statusController;

    // Methods that fall within the responsibility of the Initializer class
    public Initializer(String[] args) {
        argumentController = new ArgumentHandlerController(args);
        environmentController = new EnvironmentSetupController();
        statusController = new StatusController();
    }

    public void run() throws InterruptedException {
        if (argumentController.isKeySet("help")) {
            statusController.emitHelp();
            statusController.setExitStatusHalt();
            return;
        }
        if (argumentController.isKeySet("version")) {
            System.out.println(Version.current());
            statusController.setExitStatusHalt();
            return;
        }

        String programNames = argumentController.getKeyValue(Constants.PROGRAMS_OPTION);
        statusController.setSourceProgramNames(programNames.split(Constants.COLON));

        environmentController.runSetup(argumentController.getKeyValue("config-file"),
                argumentController.getKeyValue("log-level"));

        runTestSuites();

        Log.info(Messages.get("INF004"));        // We are terminating
    }

    public int getExitStatus(){
        return statusController.getExitCode();
    }

    public String[] getSourceProgramNames(){
        return statusController.getSourceProgramNames();
    }

    //TODO:Move methods below to Generator (or a feature)
    /**
     * For each program name specified in command-line option --programs, walk the directory tree under
     * test.suite.directory (from config) to find subdirectories that match the program name, and then pick up
     * all the testsuite files there (or the ones that match the specifications in command-line option --tests)
     * and concatenate them into a single testsuite source. For each program, invoke the Generator to merge the test
     * code into the program under test to produce a test program. Finally, launch an OS process to compile the test
     * program and execute it.
     */
    void runTestSuites() {
        // all test suites are located under this directory
        String testSuiteDirectory = Config.getTestSuiteDirectoryPathString();
        testSuiteDirectory = endWithFileSeparator(testSuiteDirectory);



        // Find test subdirectories that match program names
        List<String> matchingDirectories;
        for (String programName : statusController.getSourceProgramNames()) {
            Path path = Paths.get(programName);
            programName = path.getFileName().toString();
            try{
                matchingDirectories = getMatchingDirectories(programName, testSuiteDirectory);
            } catch (IOException ioException) {
                throw new PossibleInternalLogicErrorException(Messages.get("ERR019", programName));
            }


            for (String matchingDirectory : matchingDirectories) {
                Reader sourceReader = environmentController.getSourceReader(programName);
                Reader testSuiteReader = environmentController.getTestSuiteReader(matchingDirectory,
                        argumentController.getKeyValue(Constants.TESTS_OPTION));
                Writer testSourceWriter = environmentController.getTestSourceWriter(programName);
                String testSourceOutPath = environmentController.getTestSourceOutPath();

                Log.debug("Initializer.runTestSuites() testSourceOutPath: <" + testSourceOutPath + ">");

                mergeTestSuitesIntoTheTestProgram(sourceReader, testSuiteReader, testSourceWriter, programName);
            }
        }
    }

    /**
     * Merges source input and test suites into a single file
     *
     * @param sourceReader - For reading the cobol source.
     * @param testSuiteReader - For reading the test suites
     * @param testSourceWriter - For writing the merged output test file.
     * @param programName - The name of the cobol source program.
     */
    void mergeTestSuitesIntoTheTestProgram(Reader sourceReader, Reader testSuiteReader,
                                           Writer testSourceWriter, String programName) {
        Generator generator = new Generator(new KeywordExtractor());
        generator.mergeTestSuite(testSuiteReader, sourceReader, testSourceWriter);

        try {
            testSourceWriter.close();
        } catch (IOException closeTestSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR017", programName));
        }
    }










}
