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
        Config.load();

        Initializer initializer = new Initializer(args);
        Generator generator;
        CobolTestRunner testRunner = new CobolTestRunner();


        initializer.run();

        for (String programName : initializer.getSourceProgramNames()){

            //TODO: Generate test here

            testRunner.run();
        }



        System.exit(initializer.getExitStatus());
    }
}
