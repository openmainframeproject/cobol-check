package org.openmainframeproject.cobolcheck.features.launcher;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper;
import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.platform.PlatformLookup;

public class LauncherController {
    private Launcher launcher;
    private ProcessOutputWriter processOutputWriter;

    public LauncherController(){
        if (Config.getRunGeneratedTest()){
            launcher = new Launcher();
            processOutputWriter = new ProcessOutputWriter();
        }
    }

    /**
     * Runs the merged test program
     *
     * @throws InterruptedException - pass any InterruptedException to the caller.
     */
    public void runTestProgram(String programName, boolean isLastRun) throws InterruptedException {
        // Compile and run the test program
        ProcessLauncher pLauncher = launcher.getPlatformSpecificLauncher(PlatformLookup.get());
        String processConfigKey = pLauncher.getProcessConfigKeyPrefix() + Constants.PROCESS_CONFIG_KEY;
        String processName = Config.getString(processConfigKey);

        if (StringHelper.isBlank(processName)) {
            String errorMessage = Messages.get("ERR021", processConfigKey);
            Log.error(errorMessage);
            throw new PossibleInternalLogicErrorException(errorMessage);
        }

        int exitCode = launcher.launchProgram(pLauncher, PathHelper.getTestSourceOutPath(), (proc) ->
                processOutputWriter.writeProcessOutputToTestResultsFile(proc, Config.getTestResultFormat(),
                        Config.getTestResultFormatStyle(), programName, true, isLastRun));
        if (processOutputWriter.writeWasSuccesful){
            Log.info(Messages.get("INF011", processName, processOutputWriter.getTestResultsFilePath()));
        }
        Log.info(Messages.get("INF009", processName, String.valueOf(exitCode)));
    }
}
