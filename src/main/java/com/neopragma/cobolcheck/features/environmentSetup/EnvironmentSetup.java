package com.neopragma.cobolcheck.features.environmentSetup;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.TestSuiteConcatenator;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.filehelpers.PathHelper;
import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.log.LogLevel;

import java.io.*;
import java.util.Locale;

public class EnvironmentSetup {



    void loadConfigurationSettings(String configFileFromCommandLine) {
        if (StringHelper.notBlank(configFileFromCommandLine)) {
            Config.load(configFileFromCommandLine);
        } else {
            Config.load();
        }
        Locale configDefaultLocale = Config.getDefaultLocale();
        if (configDefaultLocale != null) {
            Messages.setLocale(configDefaultLocale);
        }
    }

    void setLogLevel(String logLevelFromCommandLine){
        if (StringHelper.notBlank(logLevelFromCommandLine)) {
            Log.set(LogLevel.valueOf(logLevelFromCommandLine.toUpperCase()));
        }
        Log.info(Messages.get("INF005",          // Log level is x
                Log.level().toString()));
    }


    //TODO: Move the three methods below
    /**
     * Returns a reader for the cobol source file.
     *
     * @param programName - The name of the file its reading from.
     */
    Reader getSourceReader(String programName){
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
     * Returns a reader for the testSuites in a directory
     *
     * @param testDirectory - The directory of the test files.
     */
    Reader getTestSuiteReader(String testDirectory, String testFileNames){
        TestSuiteConcatenator concatenator = new TestSuiteConcatenator(testFileNames);
        return concatenator.concatenateTestSuites(testDirectory);
    }


    /**
     * Returns a Writer for the output file.
     *
     * @param sourceFile - The name of the cobol source.
     */
    Writer getTestSourceWriter(String sourceFile){
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
}
