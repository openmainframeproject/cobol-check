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
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;

/**
 * Loads and manages configuration settings.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Config {

    public static final String LOCALE_LANGUAGE_CONFIG_KEY = "locale.language";
    public static final String LOCALE_COUNTRY_CONFIG_KEY = "locale.country";
    public static final String LOCALE_VARIANT_CONFIG_KEY = "locale.variant";
    public static final String DEFAULT_LOCALE_CONFIG_KEY = "default.locale";
    public static final String RESOLVED_APPLICATION_SOURCE_FILENAME_SUFFIX = "resolved.application.source.filename.suffix";
    public static final String APPLICATION_SOURCE_FILENAME_SUFFIX = "application.source.filename.suffix";
    public static final String RESOLVED_APPLICATION_COPYBOOK_FILENAME_SUFFIX = "resolved.application.copybook.filename.suffix";
    public static final String APPLICATION_COPYBOOK_FILENAME_SUFFIX = "application.copybook.filename.suffix";
    public static final String NONE = "none";
    public static final String DEFAULT_CONFIG_FILE_PATH = "config.properties";

    public Config(Messages messages) {
        this.messages = messages;
    }

    private final Messages messages;
    private Properties settings = null;

    void load() {
        String configFilePath = DEFAULT_CONFIG_FILE_PATH;
        load(configFilePath);
    }

    void load(String configResourceName) {
        Log.info(messages.get("INF001", configResourceName));
        try {
            settings = new Properties();
            InputStream configFile;
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

    Locale getDefaultLocale() { return (Locale) settings.get(DEFAULT_LOCALE_CONFIG_KEY); }

    Messages getMessages() {
        return messages;
    }

    String getApplicationFilenameSuffix() {
        return settings.getProperty(RESOLVED_APPLICATION_SOURCE_FILENAME_SUFFIX);
    }

    private void setApplicationFilenameSuffix() {
        String propertyValue = Constants.EMPTY_STRING;
        String suffix = getString(
                APPLICATION_SOURCE_FILENAME_SUFFIX,
                NONE);
        if (!suffix.equalsIgnoreCase(NONE)) {
            propertyValue = Constants.PERIOD + suffix;
        }
        settings.put(RESOLVED_APPLICATION_SOURCE_FILENAME_SUFFIX, propertyValue);
    }

    String getCopybookFilenameSuffix() {
        return settings.getProperty(RESOLVED_APPLICATION_COPYBOOK_FILENAME_SUFFIX);
    }

    private void setCopybookFilenameSuffix() {
        String propertyValue = Constants.EMPTY_STRING;
        String suffix = getString(
                APPLICATION_COPYBOOK_FILENAME_SUFFIX,
                NONE);
        if (!suffix.equalsIgnoreCase(NONE)) {
            propertyValue = Constants.PERIOD + suffix;
        }
        settings.put(RESOLVED_APPLICATION_COPYBOOK_FILENAME_SUFFIX, propertyValue);
    }

    private void setDefaultLocaleOverride() {
        if (!settings.containsKey(LOCALE_LANGUAGE_CONFIG_KEY)) {
            return;
        }
        Locale locale;
        if (!settings.containsKey(LOCALE_COUNTRY_CONFIG_KEY)) {
            locale = new Locale(settings.getProperty(LOCALE_LANGUAGE_CONFIG_KEY));
        } else if (!settings.containsKey(LOCALE_VARIANT_CONFIG_KEY)) {
            locale = new Locale(
                    settings.getProperty(LOCALE_LANGUAGE_CONFIG_KEY),
                    settings.getProperty(LOCALE_COUNTRY_CONFIG_KEY));
        } else {
            locale = new Locale(settings.getProperty(LOCALE_LANGUAGE_CONFIG_KEY),
                    settings.getProperty(LOCALE_COUNTRY_CONFIG_KEY),
                    settings.getProperty(LOCALE_VARIANT_CONFIG_KEY));
        }
        settings.put(DEFAULT_LOCALE_CONFIG_KEY, locale);
    }
}
