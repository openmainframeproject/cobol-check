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
package com.neopragma.cobolcheck.services;

import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.exceptions.IOExceptionProcessingConfigFile;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Properties;

/**
 * Loads and manages configuration settings.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Config implements StringHelper {

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
    public static final String TEST_SUITE_DIRECTORY_CONFIG_KEY = "test.suite.directory";
    public static final String APPLICATION_SOURCE_DIRECTORY_CONFIG_KEY = "application.source.directory";
    public static final String DEFAULT_APPLICATION_SOURCE_DIRECTORY = "src/main/cobol";

    public Config(Messages messages) {
        this.messages = messages;
    }

    private final Messages messages;
    private Properties settings = null;

    public void load() {
        load(DEFAULT_CONFIG_FILE_PATH);
    }

    public void load(String configResourceName) {
        Log.info(messages.get("INF001", configResourceName));
        try(FileInputStream configSettings = new FileInputStream(configResourceName)){
            settings = new Properties();
            settings.load(configSettings);
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

        setApplicationFilenameSuffixes();
        setCopybookFilenameSuffix();
    }

    public String getString(String key) {
        return getString(key, Constants.EMPTY_STRING);
    }

    public String getString(String key, String defaultValue) {
        return (String) settings.getOrDefault(key, defaultValue);
    }

    void remove(String key) {
        settings.remove(key);
    }

    public Locale getDefaultLocale() { return (Locale) settings.get(DEFAULT_LOCALE_CONFIG_KEY); }

    public Messages getMessages() {
        return messages;
    }

    public String getTestSuiteDirectoryPathString() {
        return adjustPathString(settings.getProperty(TEST_SUITE_DIRECTORY_CONFIG_KEY, Constants.CURRENT_DIRECTORY));
    }

    public String getApplicationSourceDirectoryPathString() {
        return adjustPathString(settings.getProperty(APPLICATION_SOURCE_DIRECTORY_CONFIG_KEY,
                DEFAULT_APPLICATION_SOURCE_DIRECTORY));
    }

    public List<String> getApplicationFilenameSuffixes() {
        return (List<String>) settings.get(RESOLVED_APPLICATION_SOURCE_FILENAME_SUFFIX);
    }

    private void setApplicationFilenameSuffixes() {
        resolveFilenameSuffixes(APPLICATION_SOURCE_FILENAME_SUFFIX, RESOLVED_APPLICATION_SOURCE_FILENAME_SUFFIX);
    }

    public List<String> getCopybookFilenameSuffixes() {
        return (List<String>) settings.get(RESOLVED_APPLICATION_COPYBOOK_FILENAME_SUFFIX);
    }

    private void setCopybookFilenameSuffix() {
        resolveFilenameSuffixes(APPLICATION_COPYBOOK_FILENAME_SUFFIX, RESOLVED_APPLICATION_COPYBOOK_FILENAME_SUFFIX);
    }

    private void resolveFilenameSuffixes(String configKey, String resolvedConfigKey) {
        String suffixSpecification = getString(configKey, NONE);
        List<String> suffixes = new ArrayList();
        String[] suffixValues = null;
        if (!suffixSpecification.equalsIgnoreCase(NONE)) {
            suffixValues = suffixSpecification.split(Constants.COMMA);
            for (String suffixValue : suffixValues) {
                suffixes.add(Constants.PERIOD + suffixValue);
            }
        }
        settings.put(resolvedConfigKey, suffixes);
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
