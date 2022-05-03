package org.openmainframeproject.cobolcheck.features.launcher;

import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.log.LogLevel;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;

import java.io.IOException;

/**
 * Launch a script to compile and execute a generated test program (Cobol)
 *
 * @author Dave Nicolette
 * @since 1.5
 */
public class LinuxProcessLauncher implements ProcessLauncher {
    private String processConfigKeyPrefix;

    public LinuxProcessLauncher(String processConfigKeyPrefix) {
        this.processConfigKeyPrefix = processConfigKeyPrefix;
    }

    @Override
    public String getProcessConfigKeyPrefix() {
        return processConfigKeyPrefix;
    }

    @Override
    public Process run(String programName) {
        if (Log.level() == LogLevel.DEBUG) {
            System.out.println("Entering LinuxProcessLauncher.run() method, programName is <" + programName + ">");
        }
        if (StringHelper.isBlank(programName)) {
            Log.error(Messages.get("ERR020"));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR020"));
        }
        // Hack for Mac
        if (!programName.toUpperCase().endsWith("CBL")
        && !programName.toUpperCase().endsWith("COB")) {
            programName += ".CBL";
        }
        String processConfigKey = "linux" + Constants.PROCESS_CONFIG_KEY;
        String scriptName = Config.getString(processConfigKey);
        if (StringHelper.isBlank(scriptName)) {
            Log.error(Messages.get("ERR021", processConfigKey));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR021", processConfigKey));
        }
        String scriptDirectory = Config.getString(Constants.COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY,
                Constants.DEFAULT_COBOLCHECK_SCRIPT_DIRECTORY);
        if (StringHelper.isBlank(scriptDirectory)) {
            Log.error(Messages.get("ERR022", Constants.COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR022", Constants.COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY));
        }

        if (!scriptDirectory.endsWith(Constants.FILE_SEPARATOR)) {
            scriptDirectory += Constants.FILE_SEPARATOR;
        }
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(scriptDirectory + scriptName, programName);

        Process process = null;
        StringBuilder processArguments = new StringBuilder();
        String delim = Constants.EMPTY_STRING;
        for (String argument : processBuilder.command()) {
            processArguments.append(delim);
            processArguments.append(argument);
            delim = Constants.COMMA;
        }
        Log.info(Messages.get("INF008", processArguments.toString()));
        try {
            process = processBuilder.start();
        } catch (IOException processBuilderException) {
            Log.error(Messages.get("ERR023", processArguments.toString()));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR023",
                    processArguments.toString()), processBuilderException);
        }
        return process;
    }


}
