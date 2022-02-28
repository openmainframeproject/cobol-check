package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.features.launcher.Launcher;
import org.openmainframeproject.cobolcheck.features.launcher.ProcessLauncher;
import org.openmainframeproject.cobolcheck.services.platform.Platform;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class LauncherTest {

    Launcher launcher= new Launcher();

    @Test
    void it_gets_correct_platform() {
        Platform platform = Platform.WINDOWS;
        ProcessLauncher processLauncher = launcher.getPlatformSpecificLauncher(platform);
        assertEquals("windows", processLauncher.getProcessConfigKeyPrefix());
    }
}
