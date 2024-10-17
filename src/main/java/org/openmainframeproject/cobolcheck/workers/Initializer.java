package org.openmainframeproject.cobolcheck.workers;

import org.openmainframeproject.cobolcheck.features.Status.StatusController;
import org.openmainframeproject.cobolcheck.features.argumentHandler.ArgumentHandlerController;
import org.openmainframeproject.cobolcheck.features.environmentSetup.EnvironmentSetupController;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.log.Log;

/**
 * Class for initializing program
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Initializer {

    private ArgumentHandlerController argumentController;
    private EnvironmentSetupController environmentController;
    private StatusController statusController;

    public Initializer(String[] args) {
        Log.info(Messages.get("INF000", "0.2.12"));
        argumentController = new ArgumentHandlerController(args);
        environmentController = new EnvironmentSetupController();
        statusController = new StatusController();
    }

    /**
     * Runs the initialization process by setting up the statustController
     * and setting up the environment.
     */
    public void run() {
        if (argumentController.isKeySet("help")) {
            statusController.emitHelp();
            statusController.setExitStatusHalt();
            return;
        }
        if (argumentController.isKeySet("version")) {
            statusController.emitVersion();
            statusController.setExitStatusHalt();
            return;
        }
        environmentController.runSetup(argumentController.getKeyValue("config-file"),
                argumentController.getKeyValue("log-level"));

        argumentController.loadSettingsFromArguments();
        String programNames = argumentController.getKeyValue(Constants.PROGRAMS_OPTION);
        statusController.setSourceProgramNames(programNames.split("\\|"));
        statusController.setTestFileNames(argumentController.getKeyValue(Constants.TESTS_OPTION));
    }

    public String[] getSourceProgramNames(){
        return statusController.getSourceProgramNames();
    }

    public String getTestFileNames(){
        return statusController.getTestFileNames();
    }

    public boolean isExitStatusHalt() {
        return statusController.isExitStatusHalt();
    }

    public void exitProgram(){ statusController.exitProgram();}

    public boolean isLastSourceProgram(String name)
    {
        String[] sourceProgams = getSourceProgramNames();
        return name.equals(sourceProgams[sourceProgams.length-1]);
    }

    public boolean launchTestProgram(){
        return Config.getRunGeneratedTest();
    }

    public void setExitStatusHalt() {
        statusController.setExitStatusHalt();
    }
}
