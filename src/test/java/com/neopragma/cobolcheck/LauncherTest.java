package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.launcher.Launcher;
import com.neopragma.cobolcheck.features.launcher.ProcessLauncher;
import com.neopragma.cobolcheck.services.platform.Platform;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
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
