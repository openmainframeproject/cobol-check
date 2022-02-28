package org.openmainframeproject.cobolcheck.features.argumentHandler;

import org.openmainframeproject.cobolcheck.services.Constants;

public class ArgumentHandlerController {

    ArgumentHandler argumentHandler;

    public ArgumentHandlerController(String[] args){
        argumentHandler = new ArgumentHandler(args, Constants.COMMAND_lINE_OPTIONS);
    }
    public ArgumentHandlerController(String[] args, String optionsString){
        argumentHandler = new ArgumentHandler(args, optionsString);
    }

    public boolean isKeySet(String key){
        return argumentHandler.isSet(key);
    }

    public String getKeyValue(String key){
        return argumentHandler.getValueFor(key);
    }


}
