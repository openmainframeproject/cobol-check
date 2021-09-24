package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.filehelpers.PathHelper;
import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.platform.PlatformLookup;

public class LauncherController {
    private Launcher launcher;

    public LauncherController(){
        launcher = new Launcher();
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

        int exitCode = launcher.launchProgram(pLauncher, PathHelper.getTestSourceOutPath());
        Log.info(Messages.get("INF009", processName, String.valueOf(exitCode)));
    }


}
