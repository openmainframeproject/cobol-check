package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandler;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.workers.CobolTestRunner;
import com.neopragma.cobolcheck.workers.Generator;
import com.neopragma.cobolcheck.workers.Initializer;

class Main {




    public static void main(String[] args) throws InterruptedException {
        Initializer initializer = new Initializer(args);
        Generator generator = new Generator();
        CobolTestRunner testRunner = new CobolTestRunner();

        initializer.run();
        if (initializer.isExitStatusHalt()) {
            initializer.exitProgram();
        }

        for (String programName : initializer.getSourceProgramNames()){

            generator.prepareAndRunMerge(programName, initializer.getTestFileNames());

            testRunner.run(programName, initializer.isLastSourceProgram(programName));
        }

        initializer.exitProgram();
    }
}
