package org.openmainframeproject.cobolcheck.features.Status;

import org.openmainframeproject.cobolcheck.services.Constants;

public class CurrentStatus {

    public CurrentStatus(){
        exitStatus = Constants.STATUS_NORMAL;
    }

    int exitStatus;
    String[] sourceProgramNames;
    String testFileNames;


}
