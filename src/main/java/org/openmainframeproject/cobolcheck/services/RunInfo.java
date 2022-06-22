package org.openmainframeproject.cobolcheck.services;

import java.util.HashMap;
import java.util.Map;

public class RunInfo {
    private static String currentProgramName;
    private static String currentProgramPath;
    private static Map<String, String> testSuiteNameToPathMap = new HashMap<>();
    private static String configFilePath;
    private static String generatedCobolSourcePath;
    private static String compiledProgramPath;

    public static void setCurrentProgramName(String programName){currentProgramName = programName;}
    public static String getCurrentProgramName(){return currentProgramName;}

    public static String getCurrentProgramPath() {
        return currentProgramPath;
    }

    public static void setCurrentProgramPath(String currentProgramPath) {
        RunInfo.currentProgramPath = currentProgramPath;
    }

    public static String getTestSuitePath(String testSuiteName) {
        String tempName = "\"" + testSuiteName + "\"";
        if (testSuiteNameToPathMap.containsKey(tempName))
            return testSuiteNameToPathMap.get(tempName);
        tempName = "\'" + testSuiteName + "\'";
        if (testSuiteNameToPathMap.containsKey(tempName))
            return testSuiteNameToPathMap.get(tempName);
        else
            return "";
    }

    public static void addTestSuiteNameToPathMapKeyValuePair(String testSuiteName, String path) {
        //Remove ': ' at the beginning of string
        if (path != null && path.length() > 2)
            path = path.substring(2);
        testSuiteNameToPathMap.put(testSuiteName, path);
    }

    public static String getConfigFilePath() {
        return configFilePath;
    }

    public static void setConfigFilePath(String configFilePath) {
        RunInfo.configFilePath = configFilePath;
    }

    public static String getGeneratedCobolSourcePath() {
        return generatedCobolSourcePath;
    }

    public static void setGeneratedCobolSourcePath(String generatedCobolSourcePath) {
        RunInfo.generatedCobolSourcePath = generatedCobolSourcePath;
        if (generatedCobolSourcePath.contains(".")){
            RunInfo.compiledProgramPath =  generatedCobolSourcePath.substring(0, generatedCobolSourcePath.lastIndexOf("."));
        }

    }

    public static String getCompiledProgramPath() {
        return compiledProgramPath;
    }
}
