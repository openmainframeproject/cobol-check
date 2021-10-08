package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.cobolLogic.CobolVerbs;

import java.util.Arrays;
import java.util.List;

public class Interpreter {

    // Source tokens from Procedure Division that begin batch I/O statements
    private static final List<String> batchFileIOVerbs = Arrays.asList(
            "OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE", "START"
    );

    // Used for handling source lines from copybooks that may not have the standard 80-byte length
    private static final int minimumMeaningfulSourceLineLength = 7;
    private static final int commentIndicatorOffset = 6;
    private static final char commentIndicator = '*';

    //TODO: make quicker by adding 'else if's and putting if in if
    /**
     * Sets flags based on a line, to be able to know which kinds of source
     * statements to look for when reading and interpreting lines.
     *
     * @param line - current source line being processed
     * @param state - current state of flags
     * @return - true if a flag has been set
     */
    public static boolean setFlagsForCurrentLine(CobolLine line, State state){
        boolean flagSet = false;
        if (line.contains(Constants.IDENTIFICATION_DIVISION)) {
            state.setFlagFor(Constants.IDENTIFICATION_DIVISION);
            flagSet = true;
        }
        if (line.contains(Constants.ENVIRONMENT_DIVISION)) {
            state.setFlagFor(Constants.ENVIRONMENT_DIVISION);
            flagSet = true;
        }
        if (line.contains(Constants.CONFIGURATION_SECTION)) {
            state.setFlagFor(Constants.CONFIGURATION_SECTION);
            flagSet = true;
        }
        if (line.contains(Constants.INPUT_OUTPUT_SECTION)) {
            state.setFlagFor(Constants.INPUT_OUTPUT_SECTION);
            flagSet = true;
        }
        if (line.contains(Constants.FILE_CONTROL)){
            state.setFlagFor(Constants.FILE_CONTROL);
            flagSet = true;
        }
        if (line.contains(Constants.DATA_DIVISION)) {
            state.setFlagFor(Constants.DATA_DIVISION);
            flagSet = true;
        }
        if (line.contains(Constants.PROCEDURE_DIVISION)) {
            state.setFlagFor(Constants.PROCEDURE_DIVISION);
            flagSet = true;
        }
        if (line.contains(Constants.FILE_SECTION)) {
            state.setFlagFor(Constants.FILE_SECTION);
            flagSet = true;
        }
        if (line.contains(Constants.LOCAL_STORAGE_SECTION)) {
            state.setFlagFor(Constants.LOCAL_STORAGE_SECTION);
            flagSet = true;
        }
        if (line.contains(Constants.LINKAGE_SECTION)) {
            state.setFlagFor(Constants.LINKAGE_SECTION);
            flagSet = true;
        }
        if (line.contains(Constants.WORKING_STORAGE_SECTION)) {
            state.setFlagFor(Constants.WORKING_STORAGE_SECTION);
            flagSet = true;
        }
        if (line.contains(Constants.SELECT_TOKEN)) {
            state.unsetFlagFor(Constants.SELECT_TOKEN);
            state.setFlagFor(Constants.SELECT_TOKEN);
            flagSet = true;
        }
        if (line.contains(Constants.FILE_STATUS_TOKEN)) {
            state.setFlagFor(Constants.FILE_STATUS_TOKEN);
            flagSet = true;
        }
        if (line.contains(Constants.IS_TOKEN)) {
            state.setFlagFor(Constants.IS_TOKEN);
            flagSet = true;
        }
        if (line.contains(Constants.IS_TOKEN)) {
            state.setFlagFor(Constants.IS_TOKEN);
            flagSet = true;
        }
        if (line.contains(Constants.FD_TOKEN)) {
            state.unsetFlagFor(Constants.FD_TOKEN);
            state.setFlagFor(Constants.FD_TOKEN);
            flagSet = true;
        }
        if (line.contains(Constants.LEVEL_01_TOKEN)) {
            state.setFlagFor(Constants.LEVEL_01_TOKEN);
            flagSet = true;
        }
        if (line.contains(Constants.COPY_TOKEN)) {
            state.setFlagFor(Constants.COPY_TOKEN);
            flagSet = true;
        }
        return flagSet;
    }

    /**
     * Recognizes end of statement when
     * (a) - sourceLine ends with a period
     * (b) - previous line contains just a period
     * (c) - first token on this line is a Cobol verb
     *
     * @param currentLine - current source line being processed
     * @param nextMeaningfulLine - next source line that is not empty
     * @return - true if end of statement is recognized
     */
    public static boolean isEndOfStatement(CobolLine currentLine, CobolLine nextMeaningfulLine) {
        if (currentLine.getTrimmedString().endsWith(Constants.PERIOD)) {
            return true;
        }
        if (CobolVerbs.isStartOrEndCobolVerb(nextMeaningfulLine.getTokens().get(0))) {
            return true;
        }

        return false;
    }

    /**
     * This "shouldn't happen." Famous last words.
     *
     * @param line
     * @return true if the source line is too short to be a meaningful line of code in Cobol.
     */
    public static boolean isTooShortToBeMeaningful(CobolLine line) {
        return line.getOriginalString() == null || line.getOriginalString().length() < minimumMeaningfulSourceLineLength;
    }

    /**
     * @param line
     * @return true if the source line "looks like" a Cobol comment line.
     */
    public static boolean isComment(CobolLine line) {
        return line.getOriginalString().charAt(commentIndicatorOffset) == commentIndicator;
    }

    public static boolean isEmpty(CobolLine line){
        return line.tokensSize() == 0 && !line.contains(Constants.PERIOD);
    }

    public static boolean shouldLineBeParsed(CobolLine line, State state){
        if (isTooShortToBeMeaningful(line)){
            return false;
        }
        if (state.isFlagSetFor(Constants.FILE_SECTION)){
            return false;
        }
        if (state.isFlagSetFor(Constants.FILE_CONTROL)){
            return false;
        }

        return true;
    }

    public static boolean shouldLineBeCommentedOut(CobolLine line, State state){
        if (state.isFlagSetFor(Constants.PROCEDURE_DIVISION)){
            if (checkForBatchFileIOStatement(line))
            {
                return true;
            }
        }
        return false;
    }

    public static boolean containsOnlyPeriod(CobolLine line){
        return line.getTrimmedString().equals(Constants.PERIOD);
    }

    public static boolean checkForBatchFileIOStatement(CobolLine line) {
        for (String ioVerb : batchFileIOVerbs) {
            if (isBatchFileIOStatement(line.getTokens(), ioVerb)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isBatchFileIOStatement(List<String> tokens, String ioVerb) {
        return tokens.contains(ioVerb);
    }



}
