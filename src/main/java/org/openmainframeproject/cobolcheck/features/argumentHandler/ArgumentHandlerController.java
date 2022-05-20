package org.openmainframeproject.cobolcheck.features.argumentHandler;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;

public class ArgumentHandlerController {

    ArgumentHandler argumentHandler;

    public ArgumentHandlerController(String[] args){
        argumentHandler = new ArgumentHandler(args, Constants.COMMAND_lINE_OPTIONS);
    }
    public ArgumentHandlerController(String[] args, String optionsString){
        argumentHandler = new ArgumentHandler(args, optionsString);
    }

    public void loadSettingsFromArguments() {
        //Overwrite settings from config file
        if (isKeySet("generated-tests"))
            Config.setGeneratedTestFileName(getKeyValue("generated-tests"));

        if (isKeySet("all-tests")){
            String newFileName = getKeyValue("all-tests");
            String originalPath = Config.getConcatenatedTestSuitesPath();
            int index = originalPath.lastIndexOf(Constants.FILE_SEPARATOR);
            String newPath = originalPath.substring(0, index + 1) + newFileName;
            Config.setConcatenatedTestSuitesPath(newPath);
        }
        if (isKeySet("error-log")){
            Config.setTestSuiteParserErrorLogFileName(getKeyValue("error-log"));
        }
        if (isKeySet("source-context")){
            Config.setSourceFolderContext(getKeyValue("source-context"));
        }
        if (isKeySet("run-directory")){
            Config.setRunDirectory(getKeyValue("run-directory"));
        }

        //Load paths in program values
        argumentHandler.loadArgProgramPaths();
    }

    public boolean isKeySet(String key){
        return argumentHandler.isSet(key);
    }

    public String getKeyValue(String key){
        return argumentHandler.getValueFor(key);
    }


}
