package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.workers.CobolTestRunner;
import org.openmainframeproject.cobolcheck.workers.Generator;
import org.openmainframeproject.cobolcheck.workers.Initializer;
class Main {
    //delete this

    public static void main(String[] args) throws InterruptedException {
        Initializer initializer = new Initializer(args);
        initializer.run();

        if (initializer.isExitStatusHalt()) {
            initializer.exitProgram();
        }

        Generator generator = new Generator();
        CobolTestRunner testRunner = new CobolTestRunner();

        for (String programName : initializer.getSourceProgramNames()){

            generator.prepareAndRunMerge(programName, initializer.getTestFileNames());

            if (initializer.launchTestProgram())
                testRunner.run(programName, initializer.isLastSourceProgram(programName));
        }

        initializer.exitProgram();
    }
}
