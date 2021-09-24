package com.neopragma.cobolcheck.workers;

import com.neopragma.cobolcheck.features.launcher.LauncherController;

public class CobolTestRunner {
    LauncherController controller = new LauncherController();



    public void run() throws InterruptedException {
        controller.runTestProgram();
    }
}
