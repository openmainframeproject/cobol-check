package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingConfigFile;
import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingTestResultFile;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.exceptions.TestResultsInputFileNotFoundException;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.filehelpers.PathHelper;
import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.platform.PlatformLookup;

import java.io.FileNotFoundException;
import java.io.IOException;

public class LauncherController {
    private Launcher launcher;
    private ProcessOutputWriter processOutputWriter;

    public LauncherController(){
        launcher = new Launcher();
        processOutputWriter = new ProcessOutputWriter();

    }

    /**
     * Runs the merged test program
     *
     * @throws InterruptedException - pass any InterruptedException to the caller.
     */
    public void runTestProgram() throws InterruptedException {
        // Compile and run the test program
        ProcessLauncher pLauncher = launcher.getPlatformSpecificLauncher(PlatformLookup.get());
        String processConfigKey = pLauncher.getProcessConfigKeyPrefix() + Constants.PROCESS_CONFIG_KEY;
        String processName = Config.getString(processConfigKey);

        if (StringHelper.isBlank(processName)) {
            String errorMessage = Messages.get("ERR021", processConfigKey);
            Log.error(errorMessage);
            throw new PossibleInternalLogicErrorException(errorMessage);
        }

//        int exitCode = launcher.launchProgram(pLauncher, PathHelper.getTestSourceOutPath(), (proc) ->
//                processOutputWriter.writeProcessOutputToTestResultsFile(proc, true));

        int exitCode = launcher.launchProgram(pLauncher, PathHelper.getTestSourceOutPath());

        Log.info(Messages.get("INF011", processName, processOutputWriter.getTestResultsFilePath()));
        Log.info(Messages.get("INF009", processName, String.valueOf(exitCode)));
    }

    /**
     * Runs a program
     *
     * @throws InterruptedException - pass any InterruptedException to the caller.
     */
    public void runProgram(String programPath, String outPutFilePath, boolean outputToConsole) throws InterruptedException {
        ProcessLauncher pLauncher = launcher.getPlatformSpecificLauncher(PlatformLookup.get());
        String processConfigKey = pLauncher.getProcessConfigKeyPrefix() + Constants.PROCESS_CONFIG_KEY;
        String processName = Config.getString(processConfigKey);

        if (StringHelper.isBlank(processName)) {
            String errorMessage = Messages.get("ERR021", processConfigKey);
            Log.error(errorMessage);
            throw new PossibleInternalLogicErrorException(errorMessage);
        }

        int exitCode = launcher.launchProgram(pLauncher, programPath, (proc) ->
                processOutputWriter.writeProcessOutputToFile(proc, outPutFilePath, outputToConsole));

        Log.info(Messages.get("INF011", processName, processOutputWriter.getTestResultsFilePath()));
        Log.info(Messages.get("INF009", processName, String.valueOf(exitCode)));
    }




}
