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
package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;

import java.io.IOException;

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
        processBuilder.inheritIO();
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
