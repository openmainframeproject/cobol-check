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

import java.io.IOException;

/**
 * Launch a script to compile and execute a generated test program (Cobol)
 *
 * @author Dave Nicolette
 * @since 1.5
 */
public class LinuxProcessLauncher implements ProcessLauncher, StringHelper, Constants {

    private Config config;
    private Messages messages;

    public LinuxProcessLauncher(Config config) {
        this.config = config;
        this.messages = config.getMessages();
    }

    @Override
    public Process run(String programName) {
        if (isBlank(programName)) {
            Log.error(messages.get("ERR020"));
            throw new PossibleInternalLogicErrorException(messages.get("ERR020"));
        }
        String scriptName = config.getString(Constants.LINUX_PROCESS_CONFIG_KEY);
        if (isBlank(scriptName)) {
            Log.error(messages.get("ERR021", LINUX_PROCESS_CONFIG_KEY));
            throw new PossibleInternalLogicErrorException(messages.get("ERR021", LINUX_PROCESS_CONFIG_KEY));
        }
        String scriptDirectory = config.getString(COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY,
                Constants.DEFAULT_COBOLCHECK_SCRIPT_DIRECTORY);
        if (isBlank(scriptDirectory)) {
            Log.error(messages.get("ERR022", COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY));
            throw new PossibleInternalLogicErrorException(messages.get("ERR022", COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY));
        }

        if (!scriptDirectory.endsWith(FILE_SEPARATOR)) {
            scriptDirectory += FILE_SEPARATOR;
        }
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(scriptDirectory + scriptName, programName);
        processBuilder.inheritIO();
        Process process = null;
        StringBuilder processArguments = new StringBuilder();
        String delim = EMPTY_STRING;
        for (String argument : processBuilder.command()) {
            processArguments.append(delim);
            processArguments.append(argument);
            delim = COMMA;
        }
        Log.info(messages.get("INF008", processArguments.toString()));
        try {
            process = processBuilder.start();
        } catch (IOException processBuilderException) {
            Log.error(messages.get("ERR023", processArguments.toString()));
            throw new PossibleInternalLogicErrorException(messages.get("ERR023",
                    processArguments.toString()), processBuilderException);
        }
        return process;
    }


}
