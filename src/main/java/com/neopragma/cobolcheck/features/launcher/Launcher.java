package com.neopragma.cobolcheck.features.launcher;

import com.neopragma.cobolcheck.services.log.Log;
import com.neopragma.cobolcheck.services.platform.Platform;

import java.io.IOException;
import java.util.function.Consumer;

public class Launcher {

    /**
     * Launches a program and returns the exit code. Returns -1 if launcher is null.
     *
     * @param launcher The launcher used.
     * @param programPath Path to the program to be launched.
     * @param postLaunchAction Code to be run after a process has been launched and before it terminates,
     *                         it is given as a lambda expression taking a Process. Ex:. (proc) -> doStuff(proc)
     * @throws InterruptedException Pass any InterruptedException to the caller.
     */
    int launchProgram(ProcessLauncher launcher, String programPath,
                      Consumer<Process> postLaunchAction) throws InterruptedException {
        if (launcher == null) return -1;
        Process process = launcher.run(programPath);

        postLaunchAction.accept(process);

        int exitCode = 1;
        exitCode = process.waitFor();
        return exitCode;
    }

    int launchProgram(ProcessLauncher launcher, String programPath) throws InterruptedException {
        if (launcher == null) return -1;
        Process process = launcher.run(programPath);

        int exitCode = 1;
        exitCode = process.waitFor();
        return exitCode;
    }

    /**
     * Gets a launcher based on the current platform: Linux, Windows, OSX, ZOS or Unix.
     * NOTE: Currently not supporting OSX or ZOS.
     */
    public ProcessLauncher getPlatformSpecificLauncher(Platform platform){
        ProcessLauncher launcher = null;
        switch (platform) {
            case LINUX :
                Log.debug("Initializer launching Linux process");
                launcher = new LinuxProcessLauncher("linux");
                break;
            case WINDOWS :
                Log.debug("Initializer launching Windows process");
                launcher = new WindowsProcessLauncher("windows");
                break;
            case OSX :
                Log.debug("Initializer launching OS X process");
                //launcher = new OSXProcessLauncher(config, "osx");
                break;
            case ZOS :
                Log.debug("Initializer launching z/OS process");
                //launcher = new ZOSProcessLauncher(config, "zos");
                break;
            default :
                Log.debug("Initializer launching default process");
                launcher = new LinuxProcessLauncher("unix");
                break;
        }
        return launcher;
    }
}
