package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;

import java.util.ArrayList;
import java.util.List;

public class BeforeAfterRepo {
    List<BeforeAfterItem> beforeEachItems;
    EvaluationGenerator beforeEachEvaluationGenerator;
    List<BeforeAfterItem> afterEachItems;
    EvaluationGenerator afterEachEvaluationGenerator;

    String testCodePrefix;

    private static final String BEFORE_EACH_BRANCH_NAME = "%1$sBEFORE-EACH-BRANCH-%2$s";
    private static final String AFTER_EACH_BRANCH_NAME = "%1$sAFTER-EACH-BRANCH-%2$s";
    private static final String TESTSUITE_NAME_VARIABLE = "%1$sTEST-SUITE-NAME";
    private static final String PERFORM_LINE = "           PERFORM %s";

    private static final String BEFORE_EACH_PARAGRAPH_NAME = "%1$sBEFORE-EACH";
    private static final String AFTER_EACH_PARAGRAPH_NAME = "%1$sAFTER-EACH";



    public BeforeAfterRepo(){
        testCodePrefix = Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX);
        beforeEachItems = new ArrayList<>();
        beforeEachEvaluationGenerator = new EvaluationGenerator(String.format(TESTSUITE_NAME_VARIABLE, testCodePrefix));
        afterEachItems = new ArrayList<>();
        afterEachEvaluationGenerator = new EvaluationGenerator(String.format(TESTSUITE_NAME_VARIABLE, testCodePrefix));
    }

    void addBeforeEachItem(int id, String testSuiteName, List<String> lines){
        String name = String.format(BEFORE_EACH_BRANCH_NAME, testCodePrefix, id);
        beforeEachItems.add(new BeforeAfterItem(name, testSuiteName, lines));
        beforeEachEvaluationGenerator.addEvaluationItem(String.format(PERFORM_LINE, name), testSuiteName);
    }

    void addAfterEachItem(int id, String testSuiteName, List<String> lines){
        String name = String.format(AFTER_EACH_BRANCH_NAME, testCodePrefix, id);
        afterEachItems.add(new BeforeAfterItem(name, testSuiteName, lines));
        afterEachEvaluationGenerator.addEvaluationItem(String.format(PERFORM_LINE, name), testSuiteName);
    }

    List<String> getBeforeEachParagraphLines(){
        List<String> bodyLines = new ArrayList<>();
        List<String> comments = CobolGenerator.generateCommentBlock("This is performed before each Test Case");

        if (beforeEachEvaluationGenerator.containsItems())
            bodyLines = beforeEachEvaluationGenerator.getEvaluationLines(false, null, true);
        else
            bodyLines.add(CobolGenerator.getContinueStatement());

        return CobolGenerator.generateParagraphLines(String.format(BEFORE_EACH_PARAGRAPH_NAME,
                testCodePrefix), comments, bodyLines);
    }

    List<String> getAfterEachParagraphLines(){
        List<String> bodyLines = new ArrayList<>();
        List<String> comments = CobolGenerator.generateCommentBlock("This is performed after each Test Case");

        if (afterEachEvaluationGenerator.containsItems())
            bodyLines = afterEachEvaluationGenerator.getEvaluationLines(false, null, true);
        else
            bodyLines.add(CobolGenerator.getContinueStatement());

        return CobolGenerator.generateParagraphLines(String.format(AFTER_EACH_PARAGRAPH_NAME,
                testCodePrefix), comments, bodyLines);
    }

    List<String> getAllBranchingParagraphs(boolean withComments){
        List<String> lines = new ArrayList<>();
        for (BeforeAfterItem item : beforeEachItems){
            List<String> commentLines = null;
            if (withComments){
                commentLines = (CobolGenerator.generateCommentBlock("Called before Test Cases in Test Suite:", item.testSuite));
            }
            lines.addAll(CobolGenerator.generateParagraphLines(item.name, commentLines, item.lines));
            lines.add("");
        }
        for (BeforeAfterItem item : afterEachItems){
            List<String> commentLines = null;
            if (withComments){
                commentLines = (CobolGenerator.generateCommentBlock("Called after Test Cases in Test Suite:", item.testSuite));
            }
            lines.addAll(CobolGenerator.generateParagraphLines(item.name, commentLines, item.lines));
            lines.add("");
        }
        return lines;
    }

    private class BeforeAfterItem{
        String name;
        String testSuite;
        List<String> lines;

        BeforeAfterItem(String name, String testSuite, List<String> lines){
            this.name = name;
            this.testSuite = testSuite;
            this.lines = lines;
        }
    }
}
