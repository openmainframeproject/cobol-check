package com.neopragma.cobolcheck.features.environmentSetup;

import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.log.LogLevel;
import java.util.Locale;

public class EnvironmentSetup {



    void loadConfigurationSettings(String configFileFromCommandLine) {
        if (StringHelper.notBlank(configFileFromCommandLine)) {
            Config.load(configFileFromCommandLine);
        } else {
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
