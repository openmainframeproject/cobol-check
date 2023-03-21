package org.openmainframeproject.cobolcheck.services.cobolLogic;

import org.openmainframeproject.cobolcheck.features.interpreter.Area;
import org.openmainframeproject.cobolcheck.features.interpreter.State;
import org.openmainframeproject.cobolcheck.services.Constants;

import java.util.*;

/*
 * Used by Writer
 */

public class Interpreter {

    // Source tokens from Procedure Division that begin batch I/O statements
    private static final List<String> batchFileIOVerbs = Arrays.asList(
            "OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE", "START"
    );

    // Used for handling source lines from copybooks that may not have the standard 80-byte length
    private static final int minimumMeaningfulSourceLineLength = 7;
    private static final int commentIndicatorOffset = 6;
    private static final List<Character> commentIndicators = Arrays.asList('*', '/');

    //Used to find areas
    private static final int sequenceNumberAreaEnd = 6;
    private static final int indicatorAreaEnd = 7;
    private static final int A_AreaEnd = 11;
    private static final int B_AreaEnd = 71;

    public static int getSequenceNumberAreaIndex(){
        return sequenceNumberAreaEnd;
    }

    //TODO: Speed up method by adding 'else if's and putting 'if's inside 'if's
    /**
     * Sets flags based on a line, to be able to know which kinds of source
     * statements to look for when reading and interpreting lines.
     *
     * @param line - current source line being processed
     * @param state - current state of flags
     * @return - the part of the program just entered or null if no part was entered
     */
    public static String setFlagsForCurrentLine(CobolLine line, CobolLine nextLine, State state){
        String partOfProgram = null;
        if (line.containsToken(Constants.IDENTIFICATION_DIVISION)) {
            state.setFlagFor(Constants.IDENTIFICATION_DIVISION);
            partOfProgram = Constants.IDENTIFICATION_DIVISION;
        }
        if (line.containsToken(Constants.ENVIRONMENT_DIVISION)) {
            state.setFlagFor(Constants.ENVIRONMENT_DIVISION);
            partOfProgram = Constants.ENVIRONMENT_DIVISION;
        }
        if (line.containsToken(Constants.CONFIGURATION_SECTION)) {
            state.setFlagFor(Constants.CONFIGURATION_SECTION);
            partOfProgram = Constants.CONFIGURATION_SECTION;
        }
        if (line.containsToken(Constants.SPECIAL_NAMES_PARAGRAPH)) {
            state.setFlagFor(Constants.SPECIAL_NAMES_PARAGRAPH);
            partOfProgram = Constants.SPECIAL_NAMES_PARAGRAPH;
        }
        if (line.containsToken(Constants.INPUT_OUTPUT_SECTION)) {
            state.setFlagFor(Constants.INPUT_OUTPUT_SECTION);
            partOfProgram = Constants.INPUT_OUTPUT_SECTION;
        }
        if (line.containsToken(Constants.FILE_CONTROL)){
            state.setFlagFor(Constants.FILE_CONTROL);
            partOfProgram = Constants.FILE_CONTROL;
        }
        if (line.containsToken(Constants.DATA_DIVISION)) {
            state.setFlagFor(Constants.DATA_DIVISION);
            partOfProgram = Constants.DATA_DIVISION;
        }
        if (line.containsToken(Constants.PROCEDURE_DIVISION)) {
            state.setFlagFor(Constants.PROCEDURE_DIVISION);
            partOfProgram = Constants.PROCEDURE_DIVISION;
        }
        if (line.containsToken(Constants.FILE_SECTION)) {
            state.setFlagFor(Constants.FILE_SECTION);
            partOfProgram = Constants.FILE_SECTION;
        }
        if (line.containsToken(Constants.LOCAL_STORAGE_SECTION)) {
            state.setFlagFor(Constants.LOCAL_STORAGE_SECTION);
            partOfProgram = Constants.LOCAL_STORAGE_SECTION;
        }
        if (line.containsToken(Constants.LINKAGE_SECTION)) {
            state.setFlagFor(Constants.LINKAGE_SECTION);
            partOfProgram = Constants.LINKAGE_SECTION;
        }
        if (line.containsToken(Constants.WORKING_STORAGE_SECTION)) {
            state.setFlagFor(Constants.WORKING_STORAGE_SECTION);
            partOfProgram = Constants.WORKING_STORAGE_SECTION;
        }
        if (line.containsToken(Constants.SELECT_TOKEN)) {
            state.unsetFlagFor(Constants.SELECT_TOKEN);
            state.setFlagFor(Constants.SELECT_TOKEN);
            partOfProgram = Constants.SELECT_TOKEN;
        }
        if (line.containsToken(Constants.FILE_STATUS_TOKEN)) {
            state.setFlagFor(Constants.FILE_STATUS_TOKEN);
            partOfProgram = Constants.FILE_STATUS_TOKEN;
        }
        if (line.containsToken(Constants.IS_TOKEN)) {
            state.setFlagFor(Constants.IS_TOKEN);
            partOfProgram = Constants.IS_TOKEN;
        }
        if (line.containsToken(Constants.IS_TOKEN)) {
            state.setFlagFor(Constants.IS_TOKEN);
            partOfProgram = Constants.IS_TOKEN;
        }
        if (line.containsToken(Constants.FD_TOKEN)) {
            state.unsetFlagFor(Constants.FD_TOKEN);
            state.setFlagFor(Constants.FD_TOKEN);
            partOfProgram = Constants.FD_TOKEN;
        }
        if (line.containsToken(Constants.LEVEL_01_TOKEN)) {
            state.setFlagFor(Constants.LEVEL_01_TOKEN);
            partOfProgram = Constants.LEVEL_01_TOKEN;
        }
        if (line.containsToken(Constants.COPY_TOKEN)) {
            state.setFlagFor(Constants.COPY_TOKEN);
            partOfProgram = Constants.COPY_TOKEN;
        }
        if (line.containsToken(Constants.SECTION_TOKEN)) {
            state.unsetFlagFor(Constants.SECTION_TOKEN);
            state.setFlagFor(Constants.SECTION_TOKEN);
            partOfProgram = Constants.SECTION_TOKEN;
        }
        if (isParagraphHeader(line, nextLine, state)) {
            state.unsetFlagFor(Constants.PARAGRAPH_TOKEN);
            state.setFlagFor(Constants.PARAGRAPH_TOKEN);
            partOfProgram = Constants.PARAGRAPH_TOKEN;
        }
        if (line.containsToken(Constants.CALL_TOKEN)) {
            state.unsetFlagFor(Constants.CALL_TOKEN);
            state.setFlagFor(Constants.CALL_TOKEN);
            partOfProgram = Constants.CALL_TOKEN;
        }

        return partOfProgram;
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
        if (nextMeaningfulLine == null){
            return true;
        }
        if (currentLine.getTrimmedString().endsWith(Constants.PERIOD)) {
            return true;
        }
        if (containsOnlyPeriod(nextMeaningfulLine)){
            return false;
        }
        if (CobolVerbs.isStartOrEndCobolVerb(nextMeaningfulLine.getTokens().get(0))) {
            return true;
        }

        return false;
    }

    public static boolean lineEndsParagraphOrSection(CobolLine currentLine, CobolLine nextLine, CobolLine lineFollowingNext, State state) {
        if (currentLine == null || nextLine == null || lineFollowingNext == null)
            return true;

        if (endsInPeriod(currentLine) || containsOnlyPeriod(currentLine)){
            return (isSectionHeader(nextLine, state) || isParagraphHeader(nextLine, lineFollowingNext, state));
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
        return line.getUnNumberedString() == null || line.getUnNumberedString().length() < minimumMeaningfulSourceLineLength;
    }

    /**
     * @param line
     * @return true if the source line "looks like" a Cobol comment line.
     */
    public static boolean isComment(CobolLine line) {
        return isComment(line.getUnNumberedString());
    }

    public static boolean isComment(String line) {
        return commentIndicators.contains(line.charAt(commentIndicatorOffset));
    }

    public static boolean isMeaningful(CobolLine line){
        return line != null && !isEmpty(line) && !isComment(line) && !isTooShortToBeMeaningful(line);
    }

    /**
     * @param line
     * @return true if the source line is empty
     */
    public static boolean isEmpty(CobolLine line){
        return line.tokensSize() == 0 && !containsOnlyPeriod(line);
    }

    /**
     * @param line
     * @param state
     * @return true if the source line should be parsed
     */
    public static boolean shouldLineBeParsed(CobolLine line, State state){
        if (isTooShortToBeMeaningful(line) && line.tokensSize() > 0){
            return false;
        }
        if (state.isFlagSetFor(Constants.FILE_SECTION) && ! (line.containsToken(Constants.FILE_SECTION))){
            if (line.containsToken(Constants.REPLACE_TOKEN))
                return true;

            return false;
        }
        if (state.isFlagSetFor(Constants.FILE_CONTROL)&& ! (line.containsToken(Constants.FILE_CONTROL))){
            if (line.containsToken(Constants.REPLACE_TOKEN))
                return true;

            return false;
        }

        return true;
    }

    /**
     * @param line
     * @param state
     * @return true if the source line should be commented out
     */
    public static boolean shouldLineBeStubbed(CobolLine line, State state){
        if (state.isFlagSetFor(Constants.PROCEDURE_DIVISION)){
            if (checkForBatchFileIOStatement(line) || line.containsToken(Constants.CALL_TOKEN) ||
                    line.containsToken(Constants.EXEC_SQL_TOKEN) || line.containsToken(Constants.EXEC_CICS_TOKEN))
            {
                return true;
            }
        }
        return false;
    }

    /**
     * @param line
     * @param state
     * @return true if the source line should be commented out
     */
    public static String getStubEndToken(CobolLine line, State state){
        if (state.isFlagSetFor(Constants.PROCEDURE_DIVISION)){
            if (line.containsToken(Constants.EXEC_SQL_TOKEN) || line.containsToken(Constants.EXEC_CICS_TOKEN))
            {
                return Constants.END_EXEC_TOKEN;
            }
        }
        return null;
    }

    /**
     * @param line
     * @param state
     * @return true if the source line should be commented out
     */
    public static boolean shouldLineBeReadAsStatement(CobolLine line, State state){
        if (state.isFlagSetFor(Constants.FILE_SECTION) || state.isFlagSetFor(Constants.FILE_CONTROL)){
            if (line.containsToken(Constants.REPLACE_TOKEN))
                return true;
        }
        if (state.isFlagSetFor(Constants.DATA_DIVISION)){
            if (!line.getTrimmedString().endsWith(Constants.PERIOD)){
                return true;
            }
        }


        return false;
    }

    public static boolean containsOnlyPeriod(CobolLine line){
        return line == null ? false : line.getTrimmedString().equals(Constants.PERIOD);
    }

    /**
     * @param line
     * @return true if the source line contains a batch file IO verb
     */
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


    public static String getSectionOrParagraphName(CobolLine line){
        if (line.tokensSize() > 0){
            return line.getToken(0);
        }
        else {
            return null;
        }
    }

    public static boolean isSectionHeader(CobolLine line, State state){
        return state.isFlagSetFor(Constants.PROCEDURE_DIVISION)
                && line.containsToken(Constants.SECTION_TOKEN)
                && getBeginningArea(line, true) == Area.A;
    }

    /**
     * As paragraph headers are not associated with any keyword, the method matches the
     * source line against specific attributes that makes up a paragraph header.
     *
     * @param line - The line to check
     * @param nextLine - The line after the line param
     * @param state - current state of flags
     * @return true if the source line have all the attributes of a paragraph header.
     */
    public static boolean isParagraphHeader(CobolLine line, CobolLine nextLine, State state){
        return (state.isFlagSetFor(Constants.PROCEDURE_DIVISION)
                && isParagraphHeaderFormat(line, nextLine)
                && !line.containsToken(Constants.PROCEDURE_DIVISION)
                && !line.containsToken(Constants.DECLARATIVES_TOKEN));
    }

    /**
     * Checks if the line is in the format of a paragraph header, which is:
     * - It begins in area 'A'
     * - It has only one token
     * - The token is followed by a period on this or the next line.
     *
     * @param line - The line to check
     * @param nextLine - The line after the line param
     * @return true if sourceLine is of the format of a paragraph header
     */
    private static boolean isParagraphHeaderFormat(CobolLine line, CobolLine nextLine){
        if (getBeginningArea(line, true) == Area.A){
            if (line.tokensSize()  == 1) {
                if (line.getTrimmedString().endsWith(Constants.PERIOD) ||
                        (nextLine != null &&
                        nextLine.getTrimmedString().equals(Constants.PERIOD)))
                    return true;
            }
        }
        return false;
    }

    public static List<String> getUsingArgs(CobolLine line) {
        List<String> arguments = new ArrayList<>();
        List<String> argumentReferences = Arrays.asList(Constants.BY_REFERENCE_TOKEN,
                Constants.BY_CONTENT_TOKEN, Constants.BY_VALUE_TOKEN);
        String currentArgumentReference = Constants.BY_REFERENCE_TOKEN;
        if (line.containsToken(Constants.USING_TOKEN)){
            int usingIndex = line.getTokenIndexOf(Constants.USING_TOKEN);

            for(int i = usingIndex + 1; i < line.tokensSize(); i++){
                if (line.getToken(i).toUpperCase(Locale.ROOT).equals(Constants.END_CALL_TOKEN))
                    break;
                if (argumentReferences.contains(line.getToken(i).toUpperCase(Locale.ROOT))){
                    currentArgumentReference = line.getToken(i).toUpperCase();
                    continue;
                }

                currentArgumentReference = currentArgumentReference.replace("BY ", "");
                arguments.add(currentArgumentReference + " " + line.getToken(i).replace(",",""));
                currentArgumentReference = Constants.BY_REFERENCE_TOKEN;
            }
        }
        return arguments;
    }

    /**
     * Looks through the prefix-spaces in the source line in order to determine the
     * beginning area (SEQUENCE_NUMBER, INDICATOR, A or B)
     *
     * @param line - the line to get the area for
     * @return the beginning area of the source line.
     */
    public static Area getBeginningArea(CobolLine line, boolean ignoreSequenceArea){
        if (isTooShortToBeMeaningful(line) ||
                (ignoreSequenceArea && line.getUnNumberedString().length() <= sequenceNumberAreaEnd + 1)){
            return Area.NONE;
        }
        char[] characters;
        int index = 0;
        if (ignoreSequenceArea){
            characters = line.getUnNumberedString().toCharArray();
            index = sequenceNumberAreaEnd;
        }
        else{
            characters = line.getOriginalString().toCharArray();
        }

        while (characters[index] == ' '){
            index++;
        }

        if (index < sequenceNumberAreaEnd) return Area.SEQUENCE_NUMBER;
        if (index == indicatorAreaEnd - 1) return Area.INDICATOR;
        if (index < A_AreaEnd) return Area.A;
        if (index < B_AreaEnd) return Area.B;

        return Area.NONE;
    }

    public static boolean isInNumericFormat(String token){
        List<Character> numberCharacters = Arrays.asList('9', 'Z', 'V', 'S', '*', '$');
        char firstLetter = token.toCharArray()[0];
        return numberCharacters.contains(Character.toUpperCase(firstLetter));
    }

    /**
     * Checks if this line is ending the current component (SECTION, CALL, etc.)
     * This should be called from inside the component, as it only checks, if
     * the trimmed line ends with a period
     *
     * @param line - the line to check
     * @return true if the trimmed line ends with a period
     */
    public static boolean endsInPeriod(CobolLine line) {
        return line.getTrimmedString().endsWith(Constants.PERIOD);
    }

    /**
     * Checks if the last of these lines is ending the current component (SECTION, CALL, etc.)
     * This should be called from inside the component, as it only checks, if
     * the trimmed line ends with a period
     *
     * @param lines - the lines to check
     * @return true if the last line (trimmed) ends with a period
     */
    public static boolean endsInPeriod(List<CobolLine> lines) {
        return lines.size() > 0 && lines.get(lines.size() - 1).getTrimmedString().endsWith(Constants.PERIOD);
    }

    /**
     * Depending on the line that is being interpreted, we want to make sure that we update 
     * the current datastructure. This structure is based on the lines we have read so far in 
     * working storage.
     * This will make sure to add or remove any referenced field within the structure, based on
     * the level of said field within the structure.
     */
    public static TreeMap<Integer,String> updateCurrentDataStructure(List<CobolLine> currentStatement, TreeMap<Integer, String> currentHierarchy) {

        String statementString = "";

        for(CobolLine loopLine: currentStatement){
            statementString += loopLine.getTrimmedString(); 
        }
        statementString = statementString.trim().replace(Constants.PERIOD, "");
        String[] statementWords = statementString.split("\\s+");

        if (currentHierarchy==null) {
            currentHierarchy = new TreeMap<>();
        }

        int lastKeyOfCurrentHierarchy;
        if (currentHierarchy.isEmpty()) {
            lastKeyOfCurrentHierarchy=0;
        }
        else {
            lastKeyOfCurrentHierarchy = currentHierarchy.lastKey();
        }

        int cobolLevelNumber;
        try {
            cobolLevelNumber=Integer.parseInt(statementWords[0]);
        }
        catch (NumberFormatException e) {
            return currentHierarchy;
        }

        if (cobolLevelNumber == 77){
            cobolLevelNumber = 01;
        }
        String variableName = "";
        if (statementWords.length > 1) {
            variableName = statementWords[1];
        } else {
            variableName = "FILLER";
        }

        while (lastKeyOfCurrentHierarchy > cobolLevelNumber && cobolLevelNumber > 0) {
            currentHierarchy.remove(lastKeyOfCurrentHierarchy);
            lastKeyOfCurrentHierarchy = currentHierarchy.lastKey();
        }
        if (currentHierarchy.containsKey(cobolLevelNumber))
            currentHierarchy.replace(cobolLevelNumber, variableName);
        else
            currentHierarchy.put(cobolLevelNumber, variableName);
        return currentHierarchy;
    }
}
