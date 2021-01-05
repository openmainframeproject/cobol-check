package com.neopragma.cobolcheck;

import java.io.IOException;

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

            System.out.println("programName is null");
            //TODO: throw and log
        }
        String scriptName = config.getString("linux.process");
        if (isBlank(scriptName)) {

            System.out.println("scriptName is null");
            //TODO: throw and log
        }
        String scriptDirectory = config.getString(COBOLCHECK_SCRIPT_DIRECTORY_CONFIG_KEY,
                Constants.DEFAULT_COBOLCHECK_SCRIPT_DIRECTORY);
        if (isBlank(scriptDirectory)) {

            System.out.println("scriptDirectory is null");
            //TODO: throw and log
        }

        if (!scriptDirectory.endsWith(FILE_SEPARATOR)) {
            scriptDirectory += FILE_SEPARATOR;
        }

        System.out.println("LinuxProcessLauncher, ProcessBuilder, scriptName: "
                + scriptName + ", scriptDirectory: " + scriptDirectory + ", programName: " + programName);

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
        Log.info(String.format(messages.get("INF008", processArguments.toString())));
        try {
            process = processBuilder.start();
        } catch (IOException processBuilderException) {
            processBuilderException.printStackTrace();
            //TODO: throw and log
        }
        return process;
    }


}
