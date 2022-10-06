package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Constants;

import java.util.HashMap;
import java.util.Map;

public class ContextHandler {
    private static String currentContext;
    private static Map<String, String> startAndEndOfContexts;

    static {
        startAndEndOfContexts = new HashMap<>();
        startAndEndOfContexts.put(Constants.MOCK_KEYWORD, Constants.ENDMOCK_KEYWORD);
    }

    public static void tryEnterContext(String keyword){
        if (!insideOfContext() && startAndEndOfContexts.containsKey(keyword)){
            currentContext = keyword;
        }
        else if (insideOfContext() && keyword == getContextEndKey()){
            currentContext = null;
        }
    }

    public static boolean insideOfContext(){
        return currentContext != null;
    }

    public static String getCurrentContext() { return currentContext; }
    public static String getContextEndKey() {
        if (insideOfContext())
            return startAndEndOfContexts.get(currentContext);
        else
            return null;


    }

}
