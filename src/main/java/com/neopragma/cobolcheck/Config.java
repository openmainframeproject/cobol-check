package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.ConfigFileNotFoundException;
import com.neopragma.cobolcheck.exceptions.ExpectedConfigSettingNotFoundException;
import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingConfigFile;

import java.io.File;
import java.io.IOException;
import java.util.Locale;
import java.util.Properties;

public class Config {

    public Config(Messages messages) {
        this.messages = messages;
    }

    private Messages messages;
    private Properties config = null;
    private static final String configFileExceptionMessage = "Name: %s";
    private String configFilePath;

    void load() {
        configFilePath = "/%s/config.properties"
                .formatted(this.getClass().getPackageName().replaceAll("\\.", Constants.FILE_SEPARATOR));
        load(configFilePath);
    }

    void load(String configResourceName) {
        Log.info(String.format("Attempting to load config from: %s", configResourceName));
        try {
            config = new Properties();
            config.load(this.getClass().getResourceAsStream(configResourceName));
        } catch (NullPointerException npe) {
            throw new ConfigFileNotFoundException(String.format(configFileExceptionMessage, configResourceName));
        } catch (IOException ioe) {
            throw new IOExceptionProcessingConfigFile(String.format(configFileExceptionMessage, configResourceName));
        }
        setCopybookPathFileObject();
        setDefaultLocaleOverride();
        Log.info(String.format("Config successfully loaded from: %s", configResourceName));

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

    void remove(String key) {
        config.remove(key);
    }

    Locale getDefaultLocale() { return (Locale) config.get("default.locale"); }

    private void setCopybookPathFileObject() {
        if (config.containsKey("resources.directory") && config.containsKey("copybook.directory")) {
            config.compute("copybook.path", (key, val)
                    -> new File(config.getProperty("resources.directory")
                    + Constants.FILE_SEPARATOR
                    + config.getProperty("copybook.directory")));
        } else {
            throw new ExpectedConfigSettingNotFoundException(
                    messages.get("ERR002", configFilePath, "resources.directory, copybook.directory"));
        }
    }

    private void setDefaultLocaleOverride() {

        if (config.containsKey("locale.language") == false) {
            return;
        }
        Locale locale;
        if (config.containsKey("locale.country") == false) {
            locale = new Locale(config.getProperty("locale.language"));
        } else if (config.containsKey("locale.variant") == false) {
            locale = new Locale(
                    config.getProperty("locale.language"),
                    config.getProperty("locale.country"));
        } else {
            locale = new Locale(config.getProperty("locale.language"),
                    config.getProperty("locale.country"),
                    config.getProperty("locale.variant"));
        }
        config.put("default.locale", locale);
    }
}
