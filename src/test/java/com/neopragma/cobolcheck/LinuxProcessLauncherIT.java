package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class LinuxProcessLauncherIT {

    Messages messages = new Messages();
    Config config;
    private ProcessLauncher launcher;

    @BeforeEach
    public void commonSetup() {
        config = new Config(messages);
        config.load("testconfig.properties");
        launcher = new LinuxProcessLauncher(config);
    }

    @Test
    public void it_launches_process_to_compile_and_run_a_test_program() throws InterruptedException {
        Process process = launcher.run("LAUNCHTESTT.CBL");
        assertNotNull(process, "Process was not started.");
        int exitCode = process.waitFor();
        assertTrue(exitCode < 5, "Exit 0 means all tests passed, 4 means some failed but process worked");
    }
}
