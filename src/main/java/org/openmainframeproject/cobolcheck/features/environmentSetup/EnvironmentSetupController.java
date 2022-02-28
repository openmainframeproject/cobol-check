package org.openmainframeproject.cobolcheck.features.environmentSetup;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.log.Log;

public class EnvironmentSetupController {
    EnvironmentSetup environmentSetup;

    public EnvironmentSetupController(){
        environmentSetup = new EnvironmentSetup();
    }

    public void runSetup(String configFileFromCommandLine, String logLevelFromCommandLine) {
        environmentSetup.loadConfigurationSettings(configFileFromCommandLine);

        Log.info(Messages.get("INF003"));        // We are starting

        environmentSetup.setLogLevel(logLevelFromCommandLine);

        Log.info(Messages.get("INF006",          // We are using config x
                Config.getString("config.loaded")));
    }


}
