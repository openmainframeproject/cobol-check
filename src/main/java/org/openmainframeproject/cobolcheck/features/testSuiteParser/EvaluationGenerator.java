package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import java.util.*;

public class EvaluationGenerator {
    private final String EVALUATE_START = "            EVALUATE ";
    private final String WHEN_FORMAT1 = "                WHEN %s";
    private final String WHEN_FORMAT2 = "                   ALSO %s";
    private final String PERFORM_FORMAT = "                    PERFORM %s";
    private final String WHEN_OTHER_LINE = "           WHEN OTHER";
    private final String END_EVALUATE_LINE = "            END-EVALUATE";
    private final String ANY_KEYWORD = "ANY";
    private final String ALSO_KEYWORD = "ALSO";

    String[] argumentIdentifiers;
    List<EvaluationItem> evaluationItems;

    public EvaluationGenerator(String... argumentIdentifiers){
        this.argumentIdentifiers = argumentIdentifiers;
        evaluationItems = new ArrayList<>();
    }

    public boolean containsItems(){
        return evaluationItems.size() > 0;
    }

    public void addEvaluationItem(String line, String... arguments){
        if (argumentIdentifiers.length != arguments.length)
            throw new IndexOutOfBoundsException("argument identifiers and argument values does not match up for EvaluationItem");
        evaluationItems.add(new EvaluationItem(new String[] {line}, arguments));
    }
    public void addEvaluationItem(String[] lines, String... arguments){
        if (argumentIdentifiers.length != arguments.length)
            throw new IndexOutOfBoundsException("argument identifiers and argument values does not match up for EvaluationItem");
        evaluationItems.add(new EvaluationItem(lines, arguments));
    }

    public List<String> getEvaluationLines(boolean withWhenOther, Collection<String> whenOtherLines, boolean withEndEvaluate){
        List<String> evaluationLines = new ArrayList<>();
        if (evaluationItems.isEmpty()) return evaluationLines;

        evaluationLines.addAll(getEvaluationHeader());
        evaluationLines.addAll(getEvaluationBody());

        if (withWhenOther){
            evaluationLines.add(WHEN_OTHER_LINE);
            evaluationLines.addAll(whenOtherLines);
        }
        if (withEndEvaluate)
            evaluationLines.add(END_EVALUATE_LINE);

        return evaluationLines;
    }

    private List<String> getEvaluationHeader(){
        List<String> evaluationHeaderLines = new ArrayList<>();
        String startLine = EVALUATE_START + argumentIdentifiers[0];
        evaluationHeaderLines.add(startLine);
        if (argumentIdentifiers.length > 1){

            for (int i = 1; i < argumentIdentifiers.length; i++){
                evaluationHeaderLines.add(String.format(WHEN_FORMAT2, argumentIdentifiers[i]));
            }
        }
        return evaluationHeaderLines;
    }

    private List<String> getEvaluationBody(){
        List<String> evaluationBodyLines = new ArrayList<>();
        for (EvaluationItem item : evaluationItems){
            evaluationBodyLines.add(String.format(WHEN_FORMAT1, item.arguments[0]));
            for (int i = 1; i < item.arguments.length; i++){
                evaluationBodyLines.add(String.format(WHEN_FORMAT2, item.arguments[i]));
            }
            evaluationBodyLines.addAll(Arrays.asList(item.lines));
        }
        return evaluationBodyLines;
    }

    private class EvaluationItem{
        String[] arguments;
        String[] lines;

        EvaluationItem(String[] lines, String... arguments){
            this.arguments = arguments;
            this.lines = lines;
        }
    }
}
