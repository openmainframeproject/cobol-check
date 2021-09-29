package com.neopragma.cobolcheck.features.Status;

import com.neopragma.cobolcheck.services.Constants;

public class CurrentStatus {

    public CurrentStatus(){
        exitStatus = Constants.STATUS_NORMAL;
    }

    int exitStatus;
    String[] sourceProgramNames;
    String testFileNames;


}
