package org.openmainframeproject.cobolcheck.features.Status;

import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.log.Log;

public class StatusController {
    CurrentStatus status;
    Emitter emitter;

    public StatusController(){
        status = new CurrentStatus();
        emitter = new Emitter();
    }

    public void setExitStatusNormal() {
        status.exitStatus = Constants.STATUS_NORMAL;
    }

    public void setExitStatusHalt() {
        status.exitStatus = Constants.STATUS_HALT;
    }

    public boolean isExitStatusNormal() { return status.exitStatus == Constants.STATUS_NORMAL;}
    public boolean isExitStatusHalt() { return status.exitStatus == Constants.STATUS_HALT;}

    public String[] getSourceProgramNames() {
        return status.sourceProgramNames;
    }

    public void setSourceProgramNames(String[] sourceProgramNames) {
        status.sourceProgramNames = sourceProgramNames;
    }

    public String getTestFileNames() {
        return status.testFileNames;
    }

    public void setTestFileNames(String testFileNames) {
        status.testFileNames = testFileNames;
    }

    /**
     * Emmits a help message to the console
     */
    public void emitHelp()
    {
        emitter.emitHelp();
    }

    /**
     * Emmits a message to the console showing the current version
     */
    public void emitVersion() { emitter.emitVersion(); }

    /**
     * Terminates the program with the current exitStatus
     */
    public void exitProgram(){
        Log.info(Messages.get("INF004") + ": " + status.exitStatus);
        System.exit(status.exitStatus);
    }
}
