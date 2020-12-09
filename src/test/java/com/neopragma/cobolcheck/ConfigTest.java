package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.ConfigFileNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ConfigTest {

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
        config.load("/com/neopragma/cobolcheck/testconfig.properties");
        assertEquals("test", config.getString("config.loaded"));
    }

    @Test
    public void it_throws_when_config_file_is_not_found() {
        Throwable ex = assertThrows(ConfigFileNotFoundException.class, () -> {
            config.load("bogus name");
        });
        assertEquals("Name: bogus name", ex.getMessage());
    }

    @Test
    public void it_can_find_the_copybooks_for_ZUTZCPC() {
        config.load();
        assertEquals("src/main/resources/copybooks", config.getCopybookDirectory().getPath());
    }

    @Test
    public void it_gets_the_default_locale_override() {
        config.load("/com/neopragma/cobolcheck/testconfig.properties");
        assertEquals(Locale.JAPAN, config.getDefaultLocale());
    }
}
