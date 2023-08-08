package org.openmainframeproject.cobolcheck.testhelpers;

import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.StringHelper;

import java.util.ArrayList;
import java.util.List;

public class Utilities {

    public static List<String> getTrimmedList(String text){
        String[] lines = text.split(Constants.NEWLINE);
        List<String> result = new ArrayList<>();
        for (String line : lines){
            result.add(StringHelper.removeTrailingSpaces(line));
        }
        return result;
    }

    public static String removeBoilerPlateCode(String code, List<String> boilerPlateTags){
        boolean insideBoilerPlate = false;
        String result = "";
        String[] lines = code.split(Constants.NEWLINE);
        for (String line : lines){
            if (line.contains("*")){
                boolean skip = false;
                for(String tag : boilerPlateTags){
                    if (line.contains(tag)){
                        skip = true;
                        if (line.contains("END")){
                            insideBoilerPlate = false;
                            continue;
                        }
                        else {
                            insideBoilerPlate = true;
                            continue;
                        }
                    }
                }
                if (skip){
                    continue;
                }
            }
            if (!insideBoilerPlate){
                result += line + Constants.NEWLINE;
            }
        }
        return result;
    }
}
