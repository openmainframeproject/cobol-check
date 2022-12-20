package org.openmainframeproject.cobolcheck.features.launcher;

import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;

import java.io.*;
import java.util.List;

/**
 * Launch a script to compile and execute a generated test program (Cobol)
 *
 * @author Dave Nicolette
 * @since 1.5
 */
public class WindowsProcessLauncher implements ProcessLauncher {

    private String processConfigKeyPrefix;

    public WindowsProcessLauncher(String processConfigKeyPrefix) {
        this.processConfigKeyPrefix = processConfigKeyPrefix;
    }

    @Override
    public String getProcessConfigKeyPrefix() {
        return processConfigKeyPrefix;
    }

    @Override
    public Process run(String programName) {
        Log.debug("Entering WindowsProcessLauncher.run() method, programName is <" + programName + ">");
        if (StringHelper.isBlank(programName)) {
            Log.error(Messages.get("ERR020"));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR020"));
        }
        String processConfigKey = "windows" + Constants.PROCESS_CONFIG_KEY;
        String scriptName = Config.getString(processConfigKey);

        if (StringHelper.isBlank(scriptName)) {
            Log.error(Messages.get("ERR021", processConfigKey));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR021", processConfigKey));
        }
        String scriptDirectory = Config.getScriptDirectory();
        if (StringHelper.isBlank(scriptDirectory)) {
            Log.error(Messages.get("ERR022", Config.COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY));
            throw new PossibleInternalLogicErrorException(Messages.get("ERR022", Config.COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY));
        }

        if (!scriptDirectory.endsWith(Constants.FILE_SEPARATOR)) {
            scriptDirectory += Constants.FILE_SEPARATOR;
        }

        List<String> compileOptions = Config.getGnuCOBOLCompileOptions();
        String[] commandParms = LaunchHelper.generateCommandParms(scriptDirectory + scriptName, programName, compileOptions);

        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(commandParms);

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