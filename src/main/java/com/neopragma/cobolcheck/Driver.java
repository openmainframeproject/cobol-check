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

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;

/**
 * Main class for command-line execution.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Driver implements Constants, StringHelper {

    private final Config config;
    private static Messages messages;
    private final GetOpt options;
    private Reader testSuite;
    private Reader cobolSourceIn;
    private Writer testSourceOut;
    private static final String optionSpec = "c:l:p:t:v:h --long config-file:,log-level:,programs:,tests:,version:,help";
    private String configFileFromCommandLine = EMPTY_STRING;

    private final String[] helpText = {
            "cobolcheck",
            "  -c|--config-file path-to-config-file",
            "      Java properties file containing configuration settings for this run.",
            "      Default: ./config.properties",
            "  -l|--log-level off|fatal|info|warn|error|debug|trace",
            "      Default: INFO",
            "  -p|--programs program-name-glob[:program-name-glob[:program-name-glob]]",
            "      The name of the program(s) to test",
            "  -t|--tests filename-glob[:filename-glob[:filename-glob]]",
            "      Test suite input file(s) for this run.",
            "  -v|--version",
            "      Displays the current version of cobol-check"
    };

    public Driver(
            Config config,
            GetOpt options) {
        this.config = config;
        Driver.messages = config.getMessages();
        this.options = options;
    }

    void run() {
        if (options.isSet("help")) {
            emitHelp();
            return;
        }
        if (options.isSet("version")) {
            System.out.println(Version.current());
            return;
        }
        initialize();
        runTestSuites();

        Log.info(messages.get("INF004"));        // We are terminating
    }

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
        String testSuiteDirectory =
                config.getString(TEST_SUITE_DIRECTORY_CONFIG_KEY, Constants.CURRENT_DIRECTORY);
        if (!testSuiteDirectory.endsWith(FILE_SEPARATOR)) {
            testSuiteDirectory += FILE_SEPARATOR;
        }

        String programNames = options.getValueFor(PROGRAMS_OPTION);
        String[] programNamesSeparated = programNames.split(COLON);

        // find subdirectories that match program names
        List<String> matchingDirectories;
        for (String programName : programNamesSeparated) {
            DirectoryNameMatcher directoryFinder = new DirectoryNameMatcher(programName);
            try {
                Files.walkFileTree(Paths.get(testSuiteDirectory), directoryFinder);
                matchingDirectories = directoryFinder.getMatchingDirectories();
                if (matchingDirectories.isEmpty()) {
                    Log.warn(messages.get("WRN001", programName, testSuiteDirectory));
                }
            } catch (IOException ioException) {
                throw new PossibleInternalLogicErrorException(
                        messages.get("ERR019", programName));
            }

            for (String matchingDirectory : matchingDirectories) {
                TestSuiteConcatenator concatenator =
                        new TestSuiteConcatenator(config, options);
                testSuite = concatenator.concatenateTestSuites(matchingDirectory);

                // create READER for the Cobol source program to be tested
                StringBuilder cobolSourceInPath = new StringBuilder();
                cobolSourceInPath.append(config.getString(
                        APPLICATION_SOURCE_DIRECTORY_CONFIG_KEY,
                        DEFAULT_APPLICATION_SOURCE_DIRECTORY));
                if (!cobolSourceInPath.toString().endsWith(FILE_SEPARATOR)) {
                    cobolSourceInPath.append(FILE_SEPARATOR);
                }
                cobolSourceInPath.append(programName);
                cobolSourceInPath.append(FILE_SEPARATOR);
                cobolSourceInPath.append(programName);
                cobolSourceInPath.append(config.getApplicationFilenameSuffix());
                try {
                    cobolSourceIn = new FileReader(cobolSourceInPath.toString());
                } catch (IOException cobolSourceInException) {
                    throw new PossibleInternalLogicErrorException(
                            messages.get("ERR018", programName));
                }

                // create WRITER for the test source program (copy of program to be tested plus test code)
                StringBuilder testSourceOutPath = new StringBuilder();
                testSourceOutPath.append(new File(EMPTY_STRING).getAbsolutePath());
                testSourceOutPath.append(FILE_SEPARATOR);
                testSourceOutPath.append(programName);
                testSourceOutPath.append(
                        config.getString(TEST_PROGRAM_SUFFIX_CONFIG_KEY,
                                DEFAULT_TEST_PROGRAM_SUFFIX));
                try {
                    testSourceOut = new FileWriter(testSourceOutPath.toString());
                } catch (IOException testSourceOutException) {
                    throw new PossibleInternalLogicErrorException(
                            messages.get("ERR016", programName));
                }

                mergeTestSuitesIntoTheTestProgram();
                try {
                    testSourceOut.close();
                } catch (IOException closeTestSourceOutException) {
                    throw new PossibleInternalLogicErrorException(
                            messages.get("ERR017", programName));
                }

                // compile and run the test program
                String processConfigKeyPrefix;
                ProcessLauncher launcher = null;
                switch (PlatformLookup.get()) {
                    case LINUX :
                        processConfigKeyPrefix = "linux";
                        launcher = new LinuxProcessLauncher(config);
                        break;
                    case WINDOWS :
                        processConfigKeyPrefix = "windows";
                        //launcher = new WindowsProcessLauncher(config);
                        break;
                    case OSX :
                        processConfigKeyPrefix = "osx";
                        //launcher = new OSXProcessLauncher(config);
                        break;
                    case ZOS :
                        processConfigKeyPrefix = "zos";
                        //launcher = new ZOSProcessLauncher(config);
                        break;
                    default :
                        processConfigKeyPrefix = "unix";
                        //launcher = new UnixProcessLauncher(config);
                        break;
                }
                String processConfigKey = processConfigKeyPrefix + PROCESS_CONFIG_KEY;
                String processName = config.getString(processConfigKey);
                if (isBlank(processName)) {
                    String errorMessage = messages.get("ERR021", processConfigKey);
                    Log.error(errorMessage);
                    throw new PossibleInternalLogicErrorException(errorMessage);
                }
                StringBuilder testProgramName = new StringBuilder();
                testProgramName.append(programName);
                testProgramName.append(
                        config.getString(TEST_PROGRAM_SUFFIX_CONFIG_KEY,
                                DEFAULT_TEST_PROGRAM_SUFFIX));
                Process process = launcher.run(testProgramName.toString());
                int exitCode = 1;
                try {
                    exitCode = process.waitFor();
                } catch (InterruptedException ignored) {
                }
                Log.info(messages.get("INF009", processName, String.valueOf(exitCode)));
            }
        }
    }

    void mergeTestSuitesIntoTheTestProgram() {
        Generator generator = new Generator(
                new KeywordExtractor(),
                config);
        generator.mergeTestSuite(
                testSuite,
                cobolSourceIn,
                testSourceOut
        );
    }

    void initialize() {
        configFileFromCommandLine = options.getValueFor("config-file");
        loadConfigurationSettings();

        String logLevelFromCommandLine = options.getValueFor("log-level");
        if (notBlank(logLevelFromCommandLine)) {
            Log.set(LogLevel.valueOf(logLevelFromCommandLine.toUpperCase()));
        }

        Log.info(messages.get("INF003"));        // We are starting
        Log.info(messages.get("INF005",          // Log level is x
                Log.level().toString()));
        Log.info(messages.get("INF006",          // We are using config x
                config.getString("config.loaded")));
    }

    void loadConfigurationSettings() {
        if (notBlank(configFileFromCommandLine)) {
            config.load(configFileFromCommandLine);
        } else {
            config.load();
        }
        Locale configDefaultLocale = config.getDefaultLocale();
        if (configDefaultLocale != null) {
            messages.setLocale(configDefaultLocale);
        }
    }

    void emitHelp() {
        for (String line : helpText) {
            System.out.println(line);
        }
    }

    public static void main(String[] args) {
        messages = new Messages();
        Driver app = new Driver(
                new Config(messages),
                new GetOpt(args, optionSpec, messages));
        app.run();
        System.exit(Constants.STATUS_NORMAL);
    }
}
