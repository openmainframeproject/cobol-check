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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ConfigIT {

    private Config config;

    @BeforeEach
    public void commonSetup() {
        config = new Config(new Messages());
    }

    @Test
    public void it_loads_configuration_settings_from_default_source() {
        config.load();
        assertEquals("production", config.getString("config.loaded"));
    }

    @Test
    public void it_loads_configuration_settings_from_specified_source() {
        config.load("testconfig.properties");
        assertEquals("test", config.getString("config.loaded"));
    }

    @Test
    public void it_throws_when_config_file_is_not_found() {
        Throwable ex = assertThrows(IOExceptionProcessingConfigFile.class, () -> config.load("bogus name"));
        assertEquals("ERR003: IOException accessing config file <bogus name> in Config.load(configResourceName).",
                ex.getMessage());
        if (PlatformLookup.get() == Platform.WINDOWS) {
            assertEquals("bogus name (The system cannot find the file specified)",
                ex.getCause().getMessage());
        } else {
            assertEquals("bogus name (No such file or directory)",
                    ex.getCause().getMessage());
        }
    }

    @Test
    public void it_throws_when_config_file_name_is_null() {
        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> config.load(null));
        assertEquals("ERR001: configResourceName is null on entry to Config.load(configResourceName) method.",
                ex.getMessage());
        assertEquals(NullPointerException.class,
                ex.getCause().getClass());
    }

    @Test
    public void it_can_find_the_copybooks_for_cobolcheck_based_on_config_settings() {
        config.load("testconfig.properties");
        assertEquals("CCHECKWS.CBL", findFileNamed("CCHECKWS.CBL").toFile().getName());
        assertEquals("CCHECKPD.CBL", findFileNamed("CCHECKPD.CBL").toFile().getName());
    }

    private Path findFileNamed(String filename) {
        String resourcesDirectory = config.getString("resources.directory");
        String packagePathSegment = "com/neopragma/cobolcheck";
        String copybookPathSegment = config.getString("cobolcheck.copybook.directory");
        return new File(
                resourcesDirectory + Constants.FILE_SEPARATOR
                        + packagePathSegment + Constants.FILE_SEPARATOR
                        + copybookPathSegment + Constants.FILE_SEPARATOR
                        + filename).toPath();
    }

    @Test
    public void it_gets_the_default_locale_override() {
        config.load("testconfig.properties");
        assertEquals(Locale.JAPAN, config.getDefaultLocale());
    }

    @Test
    public void it_returns_empty_string_when_application_source_filenames_have_no_suffix() {
        config.load("testconfigNoCopybookSuffix.properties");
        List<String> expected = new ArrayList();
        assertEquals(expected, config.getApplicationFilenameSuffixes());
    }

    @Test
    public void it_returns_list_of_specified_application_source_filename_suffixes() {
        config.load("testconfig.properties");
        List<String> expected = new ArrayList(Arrays.asList( ".CBL", ".cbl", ".COB", ".cob" ));
        assertEquals(expected, config.getApplicationFilenameSuffixes());
    }

    @Test
    public void it_returns_empty_string_when_application_copybook_filenames_have_no_suffix() {
        config.load("testconfigNoCopybookSuffix.properties");
        List<String> expected = new ArrayList();
        assertEquals(expected, config.getCopybookFilenameSuffixes());
    }

    @Test
    public void it_returns_list_of_specified_application_copybook_filename_suffixes() {
        config.load("testconfig.properties");
        List<String> expected = new ArrayList(Arrays.asList( ".CBL", ".cbl", ".COB", ".cob" ));
        assertEquals(expected, config.getCopybookFilenameSuffixes());
    }
}
