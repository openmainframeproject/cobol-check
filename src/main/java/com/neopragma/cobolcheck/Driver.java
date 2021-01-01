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

import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
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
            "      to those specified in the config file as test.suite.path (if any). Globs are supported here",
            "      but not in the config file."
    };

    public Driver(
            String[] args,
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
        initialize();
        prepareInputAndOutputFiles();
//        mergeTestSuiteIntoTheProgramUnderTest();

        Log.info(messages.get("INF004"));        // We are terminating
    }

    void prepareInputAndOutputFiles() {
        TestSuiteConcatenator concatenator =
                new TestSuiteConcatenator(config, options);
        testSuite = concatenator.concatenateTestSuites();

        //TODO: Make these real
        cobolSourceIn = new StringReader(EMPTY_STRING);
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
                new GetOpt(args, optionSpec, messages));
        app.run();
        System.exit(Constants.STATUS_NORMAL);
    }
}
