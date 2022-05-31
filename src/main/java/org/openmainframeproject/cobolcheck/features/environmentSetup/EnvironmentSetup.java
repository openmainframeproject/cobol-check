package org.openmainframeproject.cobolcheck.features.environmentSetup;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.RunInfo;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.log.LogLevel;

import java.io.File;
import java.util.Locale;

public class EnvironmentSetup {



    void loadConfigurationSettings(String configFileFromCommandLine) {
        if (StringHelper.notBlank(configFileFromCommandLine)) {
            RunInfo.setConfigFilePath(new File(configFileFromCommandLine).getAbsolutePath());
            Config.load(configFileFromCommandLine);
        } else {
            RunInfo.setConfigFilePath(new File(Config.DEFAULT_CONFIG_FILE_PATH).getAbsolutePath());
            Config.load();
        }
        Locale configDefaultLocale = Config.getDefaultLocale();
        if (configDefaultLocale != null) {
            Messages.setLocale(configDefaultLocale);
        }
    }

    void setLogLevel(String logLevelFromCommandLine){
        if (StringHelper.notBlank(logLevelFromCommandLine)) {
            Log.set(LogLevel.valueOf(logLevelFromCommandLine.toUpperCase()));
        }
        Log.info(Messages.get("INF005",          // Log level is x
                Log.level().toString()));
    }
}
