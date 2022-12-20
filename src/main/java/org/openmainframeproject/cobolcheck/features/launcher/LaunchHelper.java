package org.openmainframeproject.cobolcheck.features.launcher;

import java.util.List;

public class LaunchHelper {
    static String[] generateCommandParms(String processKey, String programName, List<String> compileOptions) {
        /* We want to make a String[] that contains the processKey,
         * programName and then list every compile option afterwards.
         * Hence the compileOptions size+2
         */
        int commandParmsSize = compileOptions.size()+2;
        String[] commandParms = new String[commandParmsSize];
        commandParms[0] = processKey;
        commandParms[1] = "\"" + programName + "\"";
        /* We want to start at index 2 and add the 1st value from
         * the compileOptionsList, hence why we subtract 2 from the
         * index value in compileOptions.get() - We then get the value
         * from index 0
         */
        for (int i = 2; i < commandParmsSize; i++) {
            commandParms[i] = "\"" + compileOptions.get(i-2) + "\"";
        }
        return commandParms;
    }
}
