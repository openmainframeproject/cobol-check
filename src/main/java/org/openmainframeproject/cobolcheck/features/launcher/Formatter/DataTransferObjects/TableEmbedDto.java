package org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects;

import org.openmainframeproject.cobolcheck.services.RunInfo;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

public class TableEmbedDto extends DataTransferObject {

    String result = "";
    private Map<String, String> htmlCharacterEntityMap = new HashMap<>();

    @Override
    public Object getDataTransferObject() {
        initializeHtmlCharacterEntitiesMap();
        setCounts();
        result += getHeader();
        result += getTime();
        result += getTestOverView();

        for (TestSuite testSuite : testInstance.getTestSuites()){
            result += generateHtmlForTestSuite(testSuite);
            for (TestCase testCase : testSuite.getTestCases()){
                result += generateHtmlForTestCase(testCase);
            }
        }

        return result;
    }

    private String getHeader(){
        return "<h2>Test report for " + setHtmlCharacterEnities(testInstance.getTestingPrograms()) + "</h2> \n";
    }

    private String getTime(){
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
        LocalDateTime now = LocalDateTime.now();
        return "<sub>Run from: " + dtf.format(now) + "</sub>\n";
    }

    private String getTestOverView(){
        return "<table>\n" +
                "                    <tr>\n" +
                "                        <td><b>Suites (" + testInstance.getTestSuiteNumber() + ")</b></td>\n" +
                "                        <td><b>Tests (" + testInstance.getTestCaseNumber() + ")</b></td>\n" +
                "                    </tr>\n" +
                "                    <tr>\n" +
                "                        <td class=\"" + (testInstance.getTestSuiteNumberPassed() > 0 ? "GREEN" : "GREY") +
                                            "\">" + testInstance.getTestSuiteNumberPassed() + " passed</td>\n" +
                "                        <td class=\""+ (testInstance.getTestCaseNumberPassed() > 0 ? "GREEN" : "GREY") +
                                            "\">"+ testInstance.getTestCaseNumberPassed() +" passed</td>\n" +
                "                    </tr>\n" +
                "                    <tr>\n" +
                "                        <td class=\"" + (testInstance.getTestSuiteNumberFailed() > 0 ? "RED" : "GREY") +
                                            "\">" + testInstance.getTestSuiteNumberFailed() + " failed</td>\n" +
                "                        <td class=\"" + (testInstance.getTestCaseNumberFailed() > 0 ? "RED" : "GREY") +
                                            "\">" + testInstance.getTestCaseNumberFailed() + " failed</td>\n" +
                "                    </tr>\n" +
                "                </table>        \n" +
                "        ";
    }

    private String generateHtmlForTestSuite(TestSuite testSuite){
        return "<div class=\"testcaseResult unittestResultHeader\">" + testSuite.getProgramName() + " - " + testSuite.getName() + " (hover for details)\n" +
                "            <div class=\"testcaseResultArtifactDetails\">" + formatGeneratedArtifacts(testSuite) + "</div>\n" +
                "        </div>";
    }

    private String formatGeneratedArtifacts(TestSuite testSuite){
        return "        <ul>\n" +
                "            <li>COBOL source: <a href=\"" + testSuite.getProgramPath() + "\">" + testSuite.getProgramPath() + "\"</a></li>\n" +
                "            <li>Unittest source: <a href=\"" + testSuite.getPath() + "\">" + testSuite.getPath() + "</a></li>\n" +
                "            <li>Config file: <a href=\"" + RunInfo.getConfigFilePath() + "\">" + RunInfo.getConfigFilePath() + "</a></li>\n" +
                "            <li>Generated COBOL source: <a href=\"" + RunInfo.getGeneratedCobolSourcePath() + "\">" + RunInfo.getGeneratedCobolSourcePath() + "</a></li>\n" +
                "            <li>Compiled program: <a href=\"" + RunInfo.getCompiledProgramPath() + "\">" + RunInfo.getCompiledProgramPath() + "</a></li>\n" +
                "        </ul>;";
    }

    private String generateHtmlForTestCase(TestCase testCase) {
        return "<div class=\"testcaseResult " + (testCase.didPass() ? "passedResult" : "failedResult") + "\">\n" +
                "                <table class=\"testcaseResultTable\">\n" +
                "                    <tr title=\"" + testCase.getName() + "\">\n" +
                "                        <td>" + setHtmlCharacterEnities(testCase.getName()) + "</td>\n" +
                "                        <td>" + (testCase.didPass() ? "passed" : "failed") + "</td>\n" +
                "                    </tr>\n" +
                "                    <tr style=\"" + (!testCase.didPass() ? "\"" : "display:none;\"") + ">\n" +
                "                        <td colspan=\"2\">\n" +
                "                            <div class=\"testcaseResultOutput\">" + setHtmlCharacterEnities(testCase.getFailMessage())  + "</div>\n" +
                "                        </td>\n" +
                "                    </tr>\n" +
                "                </table>\n" +
                "            </div>";

    }

    private void initializeHtmlCharacterEntitiesMap(){
        htmlCharacterEntityMap.put("<", "&#60;");
        htmlCharacterEntityMap.put(">", "&#62;");
        htmlCharacterEntityMap.put("&", "&#38;");
        htmlCharacterEntityMap.put("\"", "&#34;");
        htmlCharacterEntityMap.put("'", "&#39;");
    }

    private String setHtmlCharacterEnities(String input){
        if (input == null)
            return null;

        for (String key : htmlCharacterEntityMap.keySet()){
            if (input.contains(key)){
                input = input.replace(key, htmlCharacterEntityMap.get(key));
            }
        }
        return input;
    }
}
