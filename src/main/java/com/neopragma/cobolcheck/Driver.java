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

import java.io.*;
import java.util.Locale;

/**
 * Main class for command-line execution.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Driver implements Constants, StringHelper {

    public static final String TEST_SUITE_PATH_OPTION = "test-suite-path";
    public static final String TEST_SUITE_PATH_CONFIG_KEY = "test.suite.path";
    private final Config config;
    private static Messages messages;
    private final GetOpt options;
    private Generator generator;
    private Reader testSuite;
    private Reader cobolSourceIn;
    private Writer testSourceOut;
    private static final String optionSpec = "c:l:t:h --long config-file:,log-level:,test-suite-path:,help";
    private String configFileFromCommandLine = EMPTY_STRING;

    private final String[] helpText = {
            "cobolcheck",
            "  -c|--config-file path-to-config-file",
            "      Java properties file containing configuration settings for this run.",
            "      Default: ./config.properties",
            "  -l|--log-level off|fatal|info|warn|error|debug|trace",
            "      Default: INFO",
            "  -t|--test-suite-path absolute-or-relative-path[:absolute-or-relative-path[...]]",
            "      Location(s) of test suite input file(s) for this run. Values specified here are prefixed",
            "      to those specified in the config file as test.suite.path (if any)."
    };

    public Driver(
            String[] args,
            Config config,
            Messages messages,
            GetOpt options) {
        this.config = config;
        Driver.messages = messages;
        this.options = options;
    }

    void run() {
        if (options.isSet("help")) {
            emitHelp();
            return;
        }
        initialize();
        concatenateTestSuitesForThisRun();
        mergeTestSuiteIntoTheProgramUnderTest();

        Log.info(messages.get("INF004"));        // We are terminating
    }

    /**
     * Multiple test suite files may be input to Cobol Check. Their absolute or relative paths are specified in the
     * config file under key test.suite.path and/or on the command line via option -t or --test-suite-path. Entries
     * specified on the command line are processed first and those specified in the config file second. There is no
     * default path or test suite filename. If nothing is specified, we throw TestSuiteInputFileNotFoundException.
     *
     * The various test suite input files are concatenated into a single file for the Generator to process.
     * In case of an IOException on this file, we throw ConcatenatedTestSuiteIOException and provide info to
     * help correct the error.
     *
     * @throws ConcatenatedTestSuiteIOException
     * @throws TestSuiteInputFileNotFoundException
     */
    //TODO: Extract this to a separate class
    void concatenateTestSuitesForThisRun() {
        //TODO: Make this a config setting
        String concatenatedTestSuiteFileName = "./ALLTESTS";
        FileWriter concatenatedTestSuitesWriter;
        try {
            concatenatedTestSuitesWriter = new FileWriter(concatenatedTestSuiteFileName);
        } catch (IOException concatenatedTestSuitesException) {
            throw new ConcatenatedTestSuiteIOException(
                    messages.get("ERR012", concatenatedTestSuiteFileName),
                    concatenatedTestSuitesException);
        }
        String testSuitePathsFromCommandLine = EMPTY_STRING;
        if (options.isSet(TEST_SUITE_PATH_OPTION)) {
            testSuitePathsFromCommandLine = options.getValueFor(TEST_SUITE_PATH_OPTION);
        }
        String testSuitePathsFromConfig =
                config.getString(TEST_SUITE_PATH_CONFIG_KEY, Constants.EMPTY_STRING);
        StringBuffer testSuitePaths = new StringBuffer();
        if (notBlank(testSuitePathsFromCommandLine)) {
            testSuitePaths.append(testSuitePathsFromCommandLine);
        }
        if (notBlank(testSuitePathsFromConfig)) {
            testSuitePaths.append(COLON);
            testSuitePaths.append(testSuitePathsFromConfig);
        }
        String[] testSuitePathList = testSuitePaths.toString().split(COLON);

        String line = EMPTY_STRING;
        for (String pathname : testSuitePathList) {
            try {
                Log.info(messages.get("INF007", pathname, concatenatedTestSuiteFileName));
                testSuite = new FileReader(pathname);
            } catch (FileNotFoundException testSuiteException) {
                throw new TestSuiteInputFileNotFoundException(
                        messages.get("ERR011", pathname),
                        testSuiteException);
            }

        }

        cobolSourceIn = new StringReader("MINIMAL-BEFORE.CBL");
        testSourceOut = new StringWriter();

    }

    void mergeTestSuiteIntoTheProgramUnderTest() {
        generator = new Generator(messages,
                new StringTokenizerExtractor(messages),
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
                args,
                new Config(messages),
                new Messages(),
                new GetOpt(args, optionSpec, messages));
        app.run();
        System.exit(Constants.STATUS_NORMAL);
    }
}
