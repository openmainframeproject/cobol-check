package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.ExpectedConfigSettingNotFoundException;
import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingConfigFile;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Locale;
import java.util.Properties;

/**
 * Loads and manages configuration settings.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Config {

    public Config(Messages messages) {
        this.messages = messages;
    }

    private Messages messages;
    private Properties config = null;
    private static final String configFileExceptionMessage = "Name: %s";
    private String configFilePath;

    void load() {
//        configFilePath = "/%s/config.properties"
//                .formatted(this.getClass().getPackageName().replaceAll("\\.", Constants.FILE_SEPARATOR));
        configFilePath = "config.properties";
        load(configFilePath);
    }

    void load(String configResourceName) {
        Log.info(messages.get("INF001", configResourceName));
        try {
            config = new Properties();
            config.load(new FileInputStream(configResourceName));
        } catch (IOException ioe) {
            throw new IOExceptionProcessingConfigFile(
                    messages.get("ERR003", configResourceName), ioe);
        } catch (NullPointerException npe) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001",
                            "configResourceName", "Config.load(configResourceName)"),
                            npe);
        }
        setCopybookPathFileObject();
        setDefaultLocaleOverride();
        Log.info(messages.get("INF002", configResourceName));

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
