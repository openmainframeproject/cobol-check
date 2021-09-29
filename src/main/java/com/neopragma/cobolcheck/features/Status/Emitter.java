package com.neopragma.cobolcheck.features.Status;

import com.neopragma.cobolcheck.services.Version;

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
        System.out.println(Version.current());
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
