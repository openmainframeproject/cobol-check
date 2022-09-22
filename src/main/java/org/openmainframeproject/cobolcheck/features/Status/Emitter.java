package org.openmainframeproject.cobolcheck.features.Status;

import org.openmainframeproject.cobolcheck.services.Version;

public class Emitter {

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
            "  -g|--generated-tests",
            "      Name of the generated program that carry out the given tests.",
            "      Default: CC##99.CBL",
            "  -a|--all-tests",
            "      Name of the generated file, where all testsuites are concatenated into one file.",
            "      Default: ALLTESTS",
            "  -e|--error-log",
            "      Name of the generated testsuite error log file.",
            "      Default: ParserErrorLog.txt",
            "  -s|--source-context",
            "      The path to specify the relative paths given in the config file, with the keys:",
            "      [application.source.directory], [application.copybook.directory] and [test.suite.directory].",
            "      Default: Cobol Check's root folder",
            "  -r|--run-directory",
            "      The path to specify the relative paths given in the config file. It specifies all paths that is",
            "      specifically in Cobol Check, like the config file, scripts and generated files.",
            "      Default: Cobol Check's root folder",
            "  -v|--version",
            "      Displays the current version of cobol-check"
    };

    /**
     * Emmits a help message to the console
     */
    void emitHelp(){
        emitByLine(helpText);
    }

    /**
     * Emmits a message to the console showing the current version
     */
    void emitVersion(){
        System.out.println("Version: " + Version.current());
    }

    /**
     * Outputs text to console line by line
     *
     * @param text array of lines to output
     */
    void emitByLine(String[] text) {
        for (String line : text) {
            System.out.println(line);
        }
    }
}
