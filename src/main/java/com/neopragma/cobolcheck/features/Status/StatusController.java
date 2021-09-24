package com.neopragma.cobolcheck.features.Status;

import com.neopragma.cobolcheck.services.Constants;

public class StatusController {
    CurrentStatus status;
    Emitter emitter;

    public StatusController(){
        status = new CurrentStatus();
        emitter = new Emitter();
    }

    public int getExitCode() {
        return status.exitStatus;
    }

    public void setExitStatusNormal() {
        status.exitStatus = Constants.STATUS_NORMAL;
    }

    public void setExitStatusHalt() {
        status.exitStatus = Constants.STATUS_HALT;
    }

    public String[] getSourceProgramNames() {
        return status.sourceProgramNames;
    }

    public void setSourceProgramNames(String[] sourceProgramNames) {
        status.sourceProgramNames = sourceProgramNames;
    }

    public void emitHelp()
    {
        emitter.emitHelp();
    }
}
