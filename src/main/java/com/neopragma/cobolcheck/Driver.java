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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;

/**
 * Main class for command-line execution.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Driver implements StringHelper {

    private final Config config;
    private static Messages messages;
    private final GetOpt options;
    private String configFileFromCommandLine = Constants.EMPTY_STRING;
    private static int exitStatus;

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
    // Methods that fall within the responsibility of the Driver class
    public Driver(
            Config config,
            GetOpt options) {
        this.config = config;
        Driver.messages = config.getMessages();
        this.options = options;
        exitStatus = Constants.STATUS_NORMAL;
    }

    void run() throws InterruptedException {
        if (options.isSet("help")) {
            emitHelp();
            exitStatus = Constants.STATUS_HALT;
            return;
        }
        if (options.isSet("version")) {
            System.out.println(Version.current());
            exitStatus = Constants.STATUS_HALT;
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
    void runTestSuites() throws InterruptedException {
        // all test suites are located under this directory
        String testSuiteDirectory = config.getTestSuiteDirectoryPathString();
        testSuiteDirectory = endWithFileSeparator(testSuiteDirectory);

        String programNames = options.getValueFor(Constants.PROGRAMS_OPTION);
        String[] programNamesSeparated = programNames.split(Constants.COLON);

        // Find test subdirectories that match program names
        List<String> matchingDirectories;
        for (String programName : programNamesSeparated) {
            Path path = Paths.get(programName);
            programName = path.getFileName().toString();
            try{
                matchingDirectories = getMatchingDirectories(programName, testSuiteDirectory);
            } catch (IOException ioException) {
                throw new PossibleInternalLogicErrorException(messages.get("ERR019", programName));
            }


            for (String matchingDirectory : matchingDirectories) {
                Reader sourceReader = getSourceReader(programName);
                Reader testSuiteReader = getTestSuiteReader(matchingDirectory);
                Writer testSourceWriter = getTestSourceWriter(programName);
                String testSourceOutPath = getTestSourceOutPath();

                Log.debug("Driver.runTestSuites() testSourceOutPath: <" + testSourceOutPath + ">");

                mergeTestSuitesIntoTheTestProgram(sourceReader, testSuiteReader, testSourceWriter, programName);

                runTestProgram(testSourceOutPath);
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
     * @throws InterruptedException - pass any InterruptedException to the caller.
     */
    void mergeTestSuitesIntoTheTestProgram(Reader sourceReader, Reader testSuiteReader,
                                           Writer testSourceWriter, String programName) {
        Generator generator = new Generator(new KeywordExtractor(), config);
        generator.mergeTestSuite(testSuiteReader, sourceReader, testSourceWriter);

        try {
            testSourceWriter.close();
        } catch (IOException closeTestSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR017", programName));
        }
    }

    /**
     * Runs the merged test program
     *
     * @param testPath - The path to the test.
     * @throws InterruptedException - pass any InterruptedException to the caller.
     */
    void runTestProgram(String testPath) throws InterruptedException {
        // Compile and run the test program
        ProcessLauncher launcher = getPlatformSpecificLauncher(PlatformLookup.get());
        String processConfigKey = launcher.getProcessConfigKeyPrefix() + Constants.PROCESS_CONFIG_KEY;
        String processName = config.getString(processConfigKey);

        if (isBlank(processName)) {
            String errorMessage = messages.get("ERR021", processConfigKey);
            Log.error(errorMessage);
            throw new PossibleInternalLogicErrorException(errorMessage);
        }

        int exitCode = launchProgram(launcher, testPath);
        Log.info(messages.get("INF009", processName, String.valueOf(exitCode)));
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

    //String manipulation methods
    /**
     * Appends a file separator, if string doesn't end one
     *
     * @param path - String to append to.
     */
    String endWithFileSeparator(String path){
        if (!path.endsWith(Constants.FILE_SEPARATOR)) {
            path += Constants.FILE_SEPARATOR;
        }
        return path;
    }

    //Launcher methods
    public int getExitStatus()
    {
        return exitStatus;
    }

    /**
     * Launches a program and returns the exit code. Returns -1 if launcher is null.
     *
     * @param launcher - The launcher used.
     * @param programPath - Path to the program to be launched.
     * @throws IOException - pass any InterruptedException to the caller.
     */
    int launchProgram(ProcessLauncher launcher, String programPath) throws InterruptedException {
        if (launcher == null) return -1;
        Process process = launcher.run(programPath);
        int exitCode = 1;
//        try {
        exitCode = process.waitFor();
//        } catch (InterruptedException interruptedException) {
//            exitCode = 1;
//        }
        return exitCode;
    }

    /**
     * Gets a launcher based on the current platform: Linux, Windows, OSX, ZOS or Unix.
     * NOTE: Currently not supporting OSX or ZOS.
     */
    ProcessLauncher getPlatformSpecificLauncher(Platform platform){
        ProcessLauncher launcher = null;
        switch (platform) {
            case LINUX :
                Log.debug("Driver launching Linux process");
                launcher = new LinuxProcessLauncher(config, "linux");
                break;
            case WINDOWS :
                Log.debug("Driver launching Windows process");
                launcher = new WindowsProcessLauncher(config, "windows");
                break;
            case OSX :
                Log.debug("Driver launching OS X process");
                //launcher = new OSXProcessLauncher(config, "osx");
                break;
            case ZOS :
                Log.debug("Driver launching z/OS process");
                //launcher = new ZOSProcessLauncher(config, "zos");
                break;
            default :
                Log.debug("Driver launching default process");
                launcher = new LinuxProcessLauncher(config, "unix");
                break;
        }
        return launcher;
    }

    //I/O or File/Path methods
    /**
     * Gets the path for the COBOL source directory (the COBOL input).
     */
    String getCobolSourceDirectory(){
        StringBuilder cobolSourceInPath = new StringBuilder();
        cobolSourceInPath.append(System.getProperty("user.dir"));
        cobolSourceInPath.append(Constants.FILE_SEPARATOR);
        cobolSourceInPath.append(config.getApplicationSourceDirectoryPathString());
        if (!cobolSourceInPath.toString().endsWith(Constants.FILE_SEPARATOR)) {
            cobolSourceInPath.append(Constants.FILE_SEPARATOR);
        }
        return cobolSourceInPath.toString();
    }

    /**
     * Returns a reader for the cobol source file.
     *
     * @param programName - The name of the file its reading from.
     */
    Reader getSourceReader(String programName){
        String cobolSourceInPath = getCobolSourceDirectory() + programName;
        cobolSourceInPath = appendMatchingFileSuffix(cobolSourceInPath, config.getApplicationFilenameSuffixes());
        cobolSourceInPath = adjustPathString(cobolSourceInPath);

        Reader sourceReader;
        try {
            sourceReader = new FileReader(cobolSourceInPath);
        } catch (IOException cobolSourceInException) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR018", programName));
        }
        return sourceReader;
    }

    /**
     * Returns a reader for the testSuites in a directory
     *
     * @param testDirectory - The directory of the test files.
     */
    Reader getTestSuiteReader(String testDirectory){
        TestSuiteConcatenator concatenator = new TestSuiteConcatenator(config, options);
        return concatenator.concatenateTestSuites(testDirectory);
    }
    /**
     * Gets the path for the output file.
     */
    String getTestSourceOutPath(){
        StringBuilder testSourceOutPath = new StringBuilder();
        testSourceOutPath.append(new File(Constants.EMPTY_STRING).getAbsolutePath());
        testSourceOutPath.append(Constants.FILE_SEPARATOR);
        testSourceOutPath.append(
                config.getString(Constants.TEST_PROGRAM_NAME_CONFIG_KEY,
                        Constants.DEFAULT_TEST_PROGRAM_NAME));
        return testSourceOutPath.toString();
    }

    /**
     * Returns a Writer for the output file.
     *
     * @param sourceFile - The name of the cobol source.
     */
    Writer getTestSourceWriter(String sourceFile){
        String testSourceOutPath = getTestSourceOutPath();
        Writer testSourceWriter;
        try {
            testSourceWriter = new FileWriter(testSourceOutPath);
        } catch (IOException testSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR016", sourceFile));
        }
        return testSourceWriter;
    }

    /**
     * Returns a string which has a matched file suffix
     *
     * @param filePath - Path to the file without a suffix.
     * @param applicationSuffixes - Possible suffixes for the file
     */
    String appendMatchingFileSuffix(String filePath, List<String> applicationSuffixes){
        for (String suffix : applicationSuffixes) {
            Log.debug("Driver looking for source file <" + filePath + suffix + ">");
            if (Files.isRegularFile(Paths.get(filePath + suffix))) {
                filePath += suffix;
                Log.debug("Driver recognized this file as a regular file: <" + filePath.toString() + ">");
                break;
            }
        }
        return filePath;
    }

    /**
     * Gets all directories with a matching name
     *
     * @param name - The name of directories to look for.
     * @param path - Determines what path to look for matching directories.
     * @throws IOException - pass any InterruptedException to the caller.
     */
    List<String> getMatchingDirectories(String name, String path) throws IOException{
        List<String> matchingDirectories;
        DirectoryNameMatcher directoryFinder = new DirectoryNameMatcher(name);

        Files.walkFileTree(Paths.get(path), directoryFinder);
        matchingDirectories = directoryFinder.getMatchingDirectories();
        if (matchingDirectories.isEmpty()) {
            Log.warn(messages.get("WRN001", name, path));
        }
        return matchingDirectories;
    }

}
