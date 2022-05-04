package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.services.Version;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class VersionTest {

    @Test
    public void it_returns_the_current_version_string() {
        assertTrue(Version.current().matches("^Version: \\d\\.\\d\\..*$"));
    }
}
