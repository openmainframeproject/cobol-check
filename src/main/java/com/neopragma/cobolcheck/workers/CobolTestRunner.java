package com.neopragma.cobolcheck.workers;

import com.neopragma.cobolcheck.features.launcher.LauncherController;

public class CobolTestRunner {
    LauncherController controller = new LauncherController();


    /**
     * Runs the current cobol test file
     *
     * @throws InterruptedException - pass any InterruptedException up to the caller
     */
    public void run() throws InterruptedException {
        controller.runTestProgram();
    }
}
