package org.openmainframeproject.cobolcheck.workers;

import org.openmainframeproject.cobolcheck.features.launcher.LauncherController;

import java.nio.file.Path;
import java.nio.file.Paths;

public class CobolTestRunner {
    LauncherController controller = new LauncherController();


    /**
     * Runs the current cobol test file
     *
     * @throws InterruptedException - pass any InterruptedException up to the caller
     */
    public void run(String programName, boolean isLastRun) throws InterruptedException {
        Path path = Paths.get(programName);
        programName = path.getFileName().toString();
        controller.runTestProgram(programName, isLastRun);
    }
    
    public int getReturnCode(){
        return controller.getReturnCode();
    }
}
