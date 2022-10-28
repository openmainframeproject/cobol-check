package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Constants;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ContextHandler {
    private static String currentContext;
    private static Keyword keyword;
    private static Map<String, List<String>> startAndEndOfContexts;

    static {
        startAndEndOfContexts = new HashMap<>();
        startAndEndOfContexts.put(Constants.MOCK_KEYWORD,  Arrays.asList(Constants.ENDMOCK_KEYWORD));
        startAndEndOfContexts.put(Constants.EXPECT_KEYWORD,  Arrays.asList(Constants.ALPHANUMERIC_LITERAL_KEYWORD, Constants.NUMERIC_LITERAL_KEYWORD, Constants.BOOLEAN_VALUE));
        startAndEndOfContexts.put(Constants.VERIFY_KEYWORD,  Arrays.asList(Constants.ONCE_KEYWORD, Constants.TIMES_KEYWORD, Constants.NEVER_HAPPENED_KEYWORD, Constants.TIME_KEYWORD));
    }

    public static void tryEnterContext(String keyword){
        if (!insideOfContext() && startAndEndOfContexts.containsKey(keyword)){
            currentContext = keyword;
        }
    }

    public static void tryExitingContext(String keyword){
        if (insideOfContext() && doesKeyEndContext(keyword)){
            currentContext = null;
        }
    }

    //Used only for testing
    public static void forceContextExit(){
        currentContext = null;
    }



    public static boolean insideOfContext(){
        return currentContext != null;
    }

    public static String getCurrentContext() { return currentContext; }
    public static boolean doesKeyEndContext(String key) {
        if (insideOfContext()){
            for (String endKey : startAndEndOfContexts.get(currentContext)){
                keyword = Keywords.getKeywordFor(key, true);
                if (keyword.value().equals(endKey))
                    return true;
            }
            return false;
        }

        else
            return false;


    }

}
