/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingConfigFile;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

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
public class Config implements Constants {

    public Config(Messages messages) {
        this.messages = messages;
    }

    private final Messages messages;
    private Properties settings = null;
    private static final String configFileExceptionMessage = "Name: %s";

    void load() {
        String configFilePath = "config.properties";
        load(configFilePath);
    }

    void load(String configResourceName) {
        Log.info(messages.get("INF001", configResourceName));
        try {
            settings = new Properties();
            settings.load(new FileInputStream(configResourceName));
        } catch (IOException ioe) {
            throw new IOExceptionProcessingConfigFile(
                    messages.get("ERR003", configResourceName), ioe);
        } catch (NullPointerException npe) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001",
                            "configResourceName", "Config.load(configResourceName)"),
                            npe);
        }
        setDefaultLocaleOverride();
        Log.info(messages.get("INF002", configResourceName));

        setApplicationFilenameSuffix();
        setCopybookFilenameSuffix();
    }

    String getString(String key) {
        return getString(key, Constants.EMPTY_STRING);
    }

    String getString(String key, String defaultValue) {
        return (String) settings.getOrDefault(key, defaultValue);
    }

    void remove(String key) {
        settings.remove(key);
    }

    Locale getDefaultLocale() { return (Locale) settings.get("default.locale"); }

    Messages getMessages() {
        return messages;
    }

    String getApplicationFilenameSuffix() {
        return settings.getProperty("resolved.application.source.filename.suffix");
    }

    private void setApplicationFilenameSuffix() {
        String propertyValue = EMPTY_STRING;
        String suffix = getString(
                "application.source.filename.suffix",
                "none");
        if (!suffix.equalsIgnoreCase("none")) {
            propertyValue = PERIOD + suffix;
        }
        settings.put("resolved.application.source.filename.suffix", propertyValue);
    }

    String getCopybookFilenameSuffix() {
        return settings.getProperty("resolved.application.copybook.filename.suffix");
    }

    private void setCopybookFilenameSuffix() {
        String propertyValue = EMPTY_STRING;
        String suffix = getString(
                "application.copybook.filename.suffix",
                "none");
        if (!suffix.equalsIgnoreCase("none")) {
            propertyValue = PERIOD + suffix;
        }
        settings.put("resolved.application.copybook.filename.suffix", propertyValue);
    }

    private void setDefaultLocaleOverride() {
        if (!settings.containsKey("locale.language")) {
            return;
        }
        Locale locale;
        if (!settings.containsKey("locale.country")) {
            locale = new Locale(settings.getProperty("locale.language"));
        } else if (!settings.containsKey("locale.variant")) {
            locale = new Locale(
                    settings.getProperty("locale.language"),
                    settings.getProperty("locale.country"));
        } else {
            locale = new Locale(settings.getProperty("locale.language"),
                    settings.getProperty("locale.country"),
                    settings.getProperty("locale.variant"));
        }
        settings.put("default.locale", locale);
    }
}
