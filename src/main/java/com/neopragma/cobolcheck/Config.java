package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.ConfigFileNotFoundException;
import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingConfigFile;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

public class Config {

    private Properties config = null;
    private static final String configFileExceptionMessage = "Name: %s";

    void load() {
        load("/%s/config.properties"
                .formatted(this.getClass().getPackageName().replaceAll("\\.", Constants.FILE_SEPARATOR)));
    }

    void load(String configResourceName) {
        try {
            try {
                config = new Properties();
                config.load(this.getClass().getResourceAsStream(configResourceName));

                config.compute("copybook.path", (key, val)
                    -> new File(config.getProperty("resources.directory")
                              + Constants.FILE_SEPARATOR
                              + config.getProperty("copybook.directory")));

            } catch (NullPointerException npe) {
                throw new ConfigFileNotFoundException(String.format(configFileExceptionMessage, configResourceName));
            } catch (IOException ioe) {
                throw new IOExceptionProcessingConfigFile(String.format(configFileExceptionMessage, configResourceName));
            }
        } finally {}
    }

    String getString(String key) {
        return getString(key, Constants.EMPTY_STRING);
    }

    String getString(String key, String defaultValue) {
        return (String) config.getOrDefault(key, defaultValue);
    }

    File getCopybookDirectory() {
        return (File) config.get("copybook.path");
    }

}
