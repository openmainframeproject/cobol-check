/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck.workers;

import com.neopragma.cobolcheck.features.Status.StatusController;
import com.neopragma.cobolcheck.features.argumentHandler.ArgumentHandlerController;
import com.neopragma.cobolcheck.features.environmentSetup.EnvironmentSetupController;
import com.neopragma.cobolcheck.services.*;

/**
 * Class for initializing program
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Initializer {

    private ArgumentHandlerController argumentController;
    private EnvironmentSetupController environmentController;
    private StatusController statusController;

    public Initializer(String[] args) {
        Config.load();
        argumentController = new ArgumentHandlerController(args);
        environmentController = new EnvironmentSetupController();
        statusController = new StatusController();
    }

    /**
     * Runs the initialization process by setting up the statustController
     * and setting up the environment.
     */
    public void run() {
        if (argumentController.isKeySet("help")) {
            statusController.emitHelp();
            statusController.setExitStatusHalt();
            return;
        }
        if (argumentController.isKeySet("version")) {
            statusController.emitVersion();
            statusController.setExitStatusHalt();
            return;
        }

        String programNames = argumentController.getKeyValue(Constants.PROGRAMS_OPTION);
        statusController.setSourceProgramNames(programNames.split(Constants.COLON));
        statusController.setTestFileNames(argumentController.getKeyValue(Constants.TESTS_OPTION));

        environmentController.runSetup(argumentController.getKeyValue("config-file"),
                argumentController.getKeyValue("log-level"));
    }

    public String[] getSourceProgramNames(){
        return statusController.getSourceProgramNames();
    }

    public String getTestFileNames(){
        return statusController.getTestFileNames();
    }

    public boolean isExitStatusHalt() {
        return statusController.isExitStatusHalt();
    }

    public void exitProgram(){ statusController.exitProgram();}

}
