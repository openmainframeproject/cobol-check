package org.openmainframeproject.cobolcheck;

import org.junit.jupiter.api.BeforeAll;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Version;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class VersionTest {

    @BeforeAll
    static void oneTimeSetup() {
        Config.load("testconfig.properties");
    }

    @Test
    public void it_returns_the_current_version_string() {
        assertTrue(Version.current().matches("^\\d\\.\\d\\..*$"));
    }
}
