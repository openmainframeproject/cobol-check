package org.openmainframeproject.cobolcheck.features.interpreter;

import org.openmainframeproject.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.cobolLogic.*;
import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.platform.Platform;
import org.openmainframeproject.cobolcheck.services.platform.PlatformLookup;
import org.openmainframeproject.cobolcheck.services.RunInfo;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.*;

import static org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter.updateCurrentDataStructure;

public class InterpreterController {
    private CobolReader reader;
    private LineRepository lineRepository;
    private NumericFields numericFields;
    private TokenExtractor tokenExtractor;
    private boolean hasReadLine;
    private String possibleMockIdentifier;
    private String possibleMockType;
    private List<String> possibleMockArgs;
    private List<String> extractedCopyBook;
    private boolean insideSectionOrParagraphMockBody;
    private TreeMap<Integer,String> currentDataStructure;
    private final String stubTag;
    private SectionOrParagraph sectionOrParagraph;

    public InterpreterController(BufferedReader sourceReader) {
        if (sourceReader == null) {
            throw new PossibleInternalLogicErrorException(
                Messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        reader = new CobolReader(sourceReader);
        lineRepository = new LineRepository();
        numericFields = new NumericFields();
        tokenExtractor = new StringTokenizerExtractor();
        currentDataStructure = new TreeMap<>();
        stubTag = Config.getStubTag();
        sectionOrParagraph = new SectionOrParagraph();
    }

    // Getters for lists of specific source lines
    public List<String> getFileSectionStatements() {
        return lineRepository.getFileSectionStatements();
    }

    public List<String> getFileControlStatements() {
        return lineRepository.getFileControlStatements();
    }

    public Map<String, String> getFileIdentifiersAndStatuses() {
        return lineRepository.getFileIdentifiersAndStatuses();
    }

    public List<String> getCopyTokens() {
        return lineRepository.getCopyTokens();
    }

    // Getters for numeric field info
    public DataType getNumericFieldDataTypeFor(String fieldName) {
        return numericFields.dataTypeOf(fieldName);
    }

    public NumericFields getNumericFields() {
        return numericFields;
    }

    // Getters from reader
    public List<String> getTokensForCurrentLine() {
        return reader.getCurrentLine().getTokens();
    }

    public List<String> getCurrentStatement() {
        return convertToStrings(reader.getCurrentStatement());
    }

    public CobolLine getCurrentLineAsStatement() throws IOException {
        return reader.readStatementAsOneLine();
    }

    // Getting info from reader
    public boolean hasStatementBeenRead() {
        return reader.hasStatementBeenRead();
    }

    public boolean isReading(String partOfProgram) {
        return reader.isFlagSet(partOfProgram);
    }

    public boolean didLineJustEnter(String partOfProgram) {
        return partOfProgram.equals(reader.getLineJustEntered());
    }

    public boolean hasReaderStateChanged() {
        return reader.getLineJustEntered() != null;
    }

    public boolean currentLineContains(String str) {
        return reader.getCurrentLine().containsToken(str);
    }

    // Mock info
    public boolean isCurrentComponentMockable() {
        return possibleMockIdentifier != null;
    }

    public String getPossibleMockIdentifier() {
        return possibleMockIdentifier;
    }

    public String getPossibleMockType() {
        return possibleMockType;
    }

    public List<String> getPossibleMockArgs() {
        return possibleMockArgs;
    }

    public boolean isInsideSectionOrParagraphMockBody() {
        return insideSectionOrParagraphMockBody;
    }

    public void setInsideSectionOrParagraphMockBody(boolean insideSectionOrParagraphMockBody) {
        this.insideSectionOrParagraphMockBody = insideSectionOrParagraphMockBody;
    }

    public boolean shouldCurrentLineBeParsed() {
        if (hasStatementBeenRead() && !getCurrentStatement().isEmpty())
            return Interpreter.shouldLineBeParsed(reader.getCurrentStatement().get(0), reader.getState());
        else
            return Interpreter.shouldLineBeParsed(reader.getCurrentLine(), reader.getState());
    }

    public boolean shouldCurrentLineBeStubbed() throws IOException {
        if (reader.getState().isFlagSetFor(Constants.PROCEDURE_DIVISION)) {
            if (Interpreter.shouldLineBeStubbed(reader.getCurrentLine(), reader.getState())) {
                if (!insideSectionOrParagraphMockBody && Interpreter.endsInPeriod(reader.getCurrentLine()))
                reader.putNextLine("           .");
                reader.putNextLine("            CONTINUE");
                return true;
            }
        }
        if (reader.getState().isFlagSetFor(Constants.WORKING_STORAGE_SECTION)) {
            return Interpreter.shouldLineBeStubbed(reader.getCurrentLine(), reader.getState());
        }
        return false;
    }

    public boolean shouldCurrentStatementBeStubbed() {
        for (CobolLine line : reader.getCurrentStatement()) {
            if (Interpreter.shouldLineBeStubbed(line, reader.getState())) {
                if (!insideSectionOrParagraphMockBody && Interpreter.endsInPeriod(reader.getCurrentLine()))
                    reader.putNextLine("           .");
                reader.putNextLine("            CONTINUE");
                return true;
            }
        }
        return false;
    }

    public boolean isCurrentLineEndingSectionOrParagraph() throws IOException {
        CobolLine nextLine = reader.peekNextMeaningfulLine();
        CobolLine lineFollowingNext = reader.peekNextMeaningfulLine();

        return Interpreter.lineEndsParagraphOrSection(reader.getCurrentLine(), nextLine, lineFollowingNext,
                reader.getState());
    }

    public boolean isCurrentLineEndingWorkingStorageSection() {
        return (reader.getCurrentLine().containsToken(Constants.LOCAL_STORAGE_SECTION) ||
                reader.getCurrentLine().containsToken(Constants.LINKAGE_SECTION) ||
                reader.getCurrentLine().containsToken(Constants.PROCEDURE_DIVISION));
    }

    public boolean canWriteEndEvaluateBeforeCurrentLine() {
        if (Interpreter.containsOnlyPeriod(reader.getCurrentLine()))
            return true;
        else if (Interpreter.endsInPeriod(reader.getCurrentLine())) {
            reader.putNextLine("           .");
            return false;
        }
        return false;

    }

    /**
     * Interprets the next line from the source file. Based on the line, the following values
     * will be updated:
     * - Current line
     * - Current tokens
     * - previous line
     * The following values can be updated, based on the line:
     * - Current statement (only if multiline statement was read)
     * - The state of the reader (flags that shows what part of the program we are reading)
     * - List of FILE SECTION statements
     * - List of FILE CONTROL statements
     * - List mapping file identifiers to statuses
     * - List of copy tokens
     * - Numeric fields
     * If the line is part of a longer statement, the reader might read multiple lines.
     *
     * @return The line that was read as a string
     */
    public String interpretNextLine() {
        CobolLine line;

        resetPossibleMockValues();
        try {
            line = reader.readLine();

            if (line == null) {
                if (!hasReadLine) {
                    throw new PossibleInternalLogicErrorException(Messages.get("ERR007"));
                }
                return null;
            }

            if (reader.getLineNumber() == 1) {
                updateCBLOptions(line);
            }

            if (Interpreter.isMeaningful(line)) {
                updateDependencies(line);
                hasReadLine = true;
            }
        } catch (IOException ex) {
            throw new CobolSourceCouldNotBeReadException(ex);
        }

        // Current line might change from when it was originally read
        return reader.getCurrentLine().getUnNumberedString();
    }

    public void closeReader() {
        try {
            reader.close();
        } catch (IOException ex) {
            throw new CobolSourceCouldNotBeReadException(ex);
        }
    }

    /**
     * Updates dependencies of the interpreter.
     * The following values can be updated, based on the given line:
     * - Current statement (only if multiline statement was read)
     * - The state of the reader (flags that shows what part of the program we are reading)
     * - List of FILE SECTION statements
     * - List of FILE CONTROL statements
     * - List mapping file identifiers to statuses
     * - List of copy tokens
     * - Numeric fields
     *
     * @param line - The line the update is based upon
     */
    private void updateDependencies(CobolLine line) throws IOException {
        reader.updateState();
        updateLineRepository(line);

        List<CobolLine> currentStatement = new ArrayList<>();
        if (Interpreter.shouldLineBeReadAsStatement(line, reader.getState())) {
            currentStatement = reader.readTillEndOfStatement();
        } else {
             currentStatement.add(line);
        }

        if (reader.isFlagSet(Constants.SPECIAL_NAMES_PARAGRAPH)) {
            updateDecimalPointIsComma(line);
        }

        if (reader.isFlagSet(Constants.DATA_DIVISION)) {
            this.currentDataStructure = updateCurrentDataStructure(currentStatement, currentDataStructure);
            line = line.convertCobolLinesToCobolLine(currentStatement);
            updateNumericFields(line);
        }

        if (reader.isFlagSet(Constants.PROCEDURE_DIVISION)) {
            updatePossibleMock(line);
            updatePossibleStub(line);
            tryReadBatchFileIOStatement();
        }
    }

    private void updateDecimalPointIsComma(CobolLine line) {
        List<String> orderedDecimalIsCommaKeywords = Arrays.asList(Constants.DECIMAL_POINT_KEYWORD,
                Constants.IS_TOKEN, Constants.COMMA_KEYWORD);

        if (line.containsAllTokensInConsecutiveOrder(orderedDecimalIsCommaKeywords)) {
            Config.setDecimalPointIsComma(true);
        }
    }

    private void updateReplaceStatement(CobolLine line) {
        List<String> orderedDecimalIsCommaKeywords = Arrays.asList(Constants.DECIMAL_POINT_KEYWORD,
                Constants.IS_TOKEN, Constants.COMMA_KEYWORD);

        if (line.containsAllTokensInConsecutiveOrder(orderedDecimalIsCommaKeywords)) {
            Config.setDecimalPointIsComma(true);
        }
    }

    /**
     * Updates possibleMockIdentifier, possibleMockType and possibleMockArgs
     * if a specific part of the program was just entered.
     *
     * @param line - The line the update is based upon
     */
    private void updatePossibleMock(CobolLine line) throws IOException {
        boolean periodShouldBeOnThisLine = false;
        CobolLine nextLine = reader.peekNextMeaningfulLine();
        if (Interpreter.isSectionHeader(line, reader.getState())) {
            possibleMockIdentifier = Interpreter.getSectionOrParagraphName(line);
            possibleMockType = Constants.SECTION_TOKEN;
            possibleMockArgs = new ArrayList<>();
            periodShouldBeOnThisLine = true;
        }
        if (Interpreter.isParagraphHeader(line, nextLine, reader.getState())) {
            possibleMockIdentifier = Interpreter.getSectionOrParagraphName(line);
            possibleMockType = Constants.PARAGRAPH_TOKEN;
            possibleMockArgs = new ArrayList<>();
            periodShouldBeOnThisLine = true;
        }
        if (didLineJustEnter(Constants.CALL_TOKEN)) {
            CobolLine statement = reader.readStatementAsOneLine();
            possibleMockIdentifier = statement.getToken(statement.getTokenIndexOf(Constants.CALL_TOKEN) + 1);
            possibleMockType = Constants.CALL_TOKEN;
            possibleMockArgs = Interpreter.getUsingArgs(statement);
        }

        if (possibleMockIdentifier != null && periodShouldBeOnThisLine
                && !Interpreter.endsInPeriod(line)
                && Interpreter.containsOnlyPeriod(nextLine)) {
            // We might generate code after the current line, thus if the period is on the next line,
            // we append it to this line. This prevents us generating code in the wrong place.
            reader.appendNextMeaningfulLineToCurrentLine(true);
        }
    }

    private void updatePossibleStub(CobolLine line) throws IOException {
        if (Interpreter.shouldLineBeStubbed(line, reader.getState())) {
            String stubEndToken = Interpreter.getStubEndToken(line, reader.getState());
            if (stubEndToken != null) {
                reader.readTillHitToken(stubEndToken, false);
            }
        }
    }

    /**
     * If the current line is a batch file IO statement, the reader will read the
     * whole statement no matter how many lines it spans.
     */
    private void tryReadBatchFileIOStatement() {
        CobolLine line = reader.getCurrentLine();
        if (Interpreter.checkForBatchFileIOStatement(line)) {
            try {
                reader.readTillEndOfStatement();
            } catch (IOException ex) {
                throw new CobolSourceCouldNotBeReadException(ex);
            }

        }
    }

    /**
     * If the current line is a numeric field, it will be added to the list of numeric fields.
     * We need to save field names of numeric data items in the DATA DIVISION in case a test case
     * references them. There's no way to distinguish numeric fields while reading the PROCEDURE DIVISION.
     */
    private void updateNumericFields(CobolLine line) {
        if (line.tokensSize() > 1) {
            String variableNameWeWantToSave = line.getToken(1);

            if (!this.currentDataStructure.isEmpty()) {
                variableNameWeWantToSave = generateVariableNameBasedOnDataStructure(this.currentDataStructure);
            }

            if (line.containsToken(Constants.COMP_3_VALUE)) {
                numericFields.setDataTypeOf(variableNameWeWantToSave.toUpperCase(Locale.ROOT), DataType.PACKED_DECIMAL);
            } else {
                if (Interpreter.lineContainsBinaryFieldDefinition(line)) {
                    numericFields.setDataTypeOf(variableNameWeWantToSave.toUpperCase(Locale.ROOT), DataType.BINARY);
                } else {
                    int ix = 0;
                    for (String token : line.getTokens()) {
                        if (token.equalsIgnoreCase(Constants.PIC_VALUE)
                                || (token.equalsIgnoreCase(Constants.PICTURE_VALUE))) {
                            if (Interpreter.isInNumericFormat(line.getToken(ix + 1))) {
                                numericFields.setDataTypeOf(variableNameWeWantToSave.toUpperCase(Locale.ROOT),
                                        DataType.DISPLAY_NUMERIC);
                            }
                            break;
                        }
                        ix++;
                    }
                }
            }
        }
    }

    /**
    * In order for us to verify wether a given field is numeric, we need to generate a key,
    * based on how the datastructure for the field is referenced.
    * We will generate this key, by using the fieldname and seperate each datastructure value
    * with a comma.
    * Example: FIELD IN DATA2 OF DATA1 will get the following key: FIELD,DATA2,DATA1
    */
   private String generateVariableNameBasedOnDataStructure(TreeMap<Integer,String> dataStructure) {
       NavigableMap<Integer,String> descendingMap = dataStructure.descendingMap();
       Collection<String> structureValues = descendingMap.values();
       return String.join(",",structureValues);
   }

    /**
     * Updates the line repository with the given line, if it might have further use when testing.
     * Lines ahead of the current one, might be peeked or even read, if the current statement spans
     * multiple lines. If the line fits any of below it will be saved in the repository:
     * - It is a line read under FILE CONTROL
     * - It has a SELECT token. File identifier and corresponding FILE STATUS will be saved
     * - It contains a copy token
     * - It is a file section statement (file section statements from referenced copybooks are also included)
     * - It has an SQL statement and within le WORKING SECTION
     *
     * @param line - current source line
     */
    private void updateLineRepository(CobolLine line) throws IOException {
        if (reader.isFlagSet(Constants.FILE_CONTROL)) {
            lineRepository.addFileControlStatement(line.getUnNumberedString());

            updateLineRepoBySelectToken(line);
            updateLineRepoByFileStatusToken(line);
        }
        if (reader.isFlagSet(Constants.FILE_SECTION) && reader.isFlagSet(Constants.FD_TOKEN)) {
            if (line.containsToken(Constants.COPY_TOKEN)) {
                updateLineRepoByCopyStatement();
                extractedCopyBook = lineRepository.addExpandedCopyStatementsToFileSectionStatements();
                for (int i = 0; i < extractedCopyBook.size(); i++) {
                    CobolLine cobolLine = new CobolLine(extractedCopyBook.get(i), tokenExtractor);
                    updateNumericFields(cobolLine);
                }
            } else if (reader.isFlagSet(Constants.LEVEL_01_TOKEN)) {
                lineRepository.addFileSectionStatement(line.getUnNumberedString());
            }
        }
        
        if (reader.isFlagSet(Constants.WORKING_STORAGE_SECTION) &&
                line.containsToken(Constants.EXEC_SQL_TOKEN) &&
                (line.containsToken(Constants.INCLUDE)
                        || reader.peekNextMeaningfulLine().containsToken(Constants.INCLUDE))) {
            Platform platform = PlatformLookup.get();
            switch(platform){
                case ZOS:
                    if (line.containsToken("SQLCA") || line.containsToken("SQLDA"))
                        return;
                    break;
                default:
                    extractedCopyBook = lineRepository.addExpandedCopyDB2Statements(reader.readStatementAsOneLine());
                    for (int i = 0; i < extractedCopyBook.size(); i++) {
                        CobolLine cobolLine = new CobolLine(extractedCopyBook.get(i), tokenExtractor);
                        List<CobolLine> currentStatement = new ArrayList<>();
                        currentStatement.add(cobolLine);
                        this.currentDataStructure = updateCurrentDataStructure(currentStatement, currentDataStructure);
                        updateNumericFields(cobolLine);                
                    }
                    break;
            }
        }
    }

    /**
     * If the given line contains a SELECT token, the file identifier will be added, waiting for
     * a mapping to a corresponding file status.
     *
     * @param line - current source line
     */
    private void updateLineRepoBySelectToken(CobolLine line) {
        if (line.containsToken(Constants.SELECT_TOKEN)) {
            if (line.tokensSize() > 1) {
                lineRepository.addFileIdentifierWithNoStatus(line.getToken(1));
            } else {
                // First token on the next line should be the file identifier
                String nextLineToken0 = peekNextMeaningfulLineAndGetTokenAtIndex0();
                if (nextLineToken0 == null) {
                    throw new PossibleInternalLogicErrorException("Line after SELECT is empty");
                }
                lineRepository.addFileIdentifierWithNoStatus(nextLineToken0);
            }
        }
    }

    /**
     * If the given line contains a file status token, it will be added to a map to a
     * corresponding file identifier.
     *
     * @param line - current source line
     */
    private void updateLineRepoByFileStatusToken(CobolLine line) throws IOException {
        if (line.containsToken(Constants.FILE_STATUS_TOKEN)) {
            if (Interpreter.isEndOfStatement(line, reader.peekNextMeaningfulLine())) {
                // File status statement is on one line
                if (line.tokensSize() > 2) {
                    if (line.getToken(1).equalsIgnoreCase(Constants.IS_TOKEN)) {
                        lineRepository.addStatusForLastSetIdentifier(line.getToken(2));
                    }
                } else if (line.tokensSize() > 1) {
                    if (!line.containsToken(Constants.IS_TOKEN)) {
                        lineRepository.addStatusForLastSetIdentifier(line.getToken(1));
                    }
                }
            } else {
                // File status statement is written across multiple lines
                List<CobolLine> statement = reader.readTillEndOfStatement();
                for (CobolLine l : statement) {
                    List<String> tokens = l.getTokens();
                    for (String token : tokens) {
                        if (!token.equalsIgnoreCase(Constants.FILE_STATUS_TOKEN)
                                && !token.equalsIgnoreCase(Constants.IS_TOKEN)) {
                            lineRepository.addStatusForLastSetIdentifier(token);
                        }
                    }
                }
            }
        }
    }

    /**
     * This method should only be called if the current line contains a COPY token.
     * Reads the whole copy statement and adds each token of each line, to a list
     * of copy tokens.
     */
    private void updateLineRepoByCopyStatement() {
        List<CobolLine> copystatementLines;
        try {
            copystatementLines = reader.readTillEndOfStatement();
        } catch (IOException ex) {
            throw new CobolSourceCouldNotBeReadException(ex);
        }

        if (copystatementLines == null) {
            throw new PossibleInternalLogicErrorException("Copy statements was read as null");
        }
        for (CobolLine line : copystatementLines) {
            if (!Interpreter.isComment(line) && !Interpreter.isEmpty(line)
                    && !Interpreter.isTooShortToBeMeaningful(line)) {
                lineRepository.addAccumulatedTokensFromCopyStatementToCopyTokens(line.getUnNumberedString());
            }

        }
    }

    private void updateCBLOptions(CobolLine line) {
        String appendOptions = Config.getAppendRulesAndOptions();
        if (appendOptions != null) {
            if (line.containsToken(Constants.CBL_TOKEN)) {
                line = reader.appendToCurrentLine(", " + appendOptions);
            } else {

                reader.addLineBeforeCurrentRead("       " + Constants.CBL_TOKEN + " " + appendOptions);
            }
        }
    }

    private String peekNextMeaningfulLineAndGetTokenAtIndex0() {
        CobolLine peekedLine;
        try {
            peekedLine = reader.peekNextMeaningfulLine();
        } catch (IOException ex) {
            throw new CobolSourceCouldNotBeReadException(ex);
        }

        if (peekedLine != null && peekedLine.tokensSize() > 0) {
            return peekedLine.getToken(0);
        } else {
            return null;
        }
    }

    private List<String> convertToStrings(List<CobolLine> lines) {
        List<String> stringLines = new ArrayList<>();
        for (CobolLine l : lines) {
            stringLines.add(l.getUnNumberedString());
        }
        return stringLines;
    }

    private void resetPossibleMockValues() {
        possibleMockIdentifier = null;
        possibleMockType = null;
    }

    public List<String> getSectionOrParagraphLines(){
        return sectionOrParagraph.getLines();
    }

    public void removeSectionOrParagraphLines(){
        sectionOrParagraph.removeLines();
    }

    public void addSectionOrParagraphLine(){
        if(Interpreter.shouldLineBeStubbed(reader.getCurrentLine(), reader.getState()))
            sectionOrParagraph.addLine(StringHelper.stubLine(reader.getCurrentLine().getUnNumberedString(), stubTag));
        else sectionOrParagraph.addLine(reader.getCurrentLine().getUnNumberedString());
    }

    public void addSectionOrParagraphLine(String line){
        sectionOrParagraph.addLine(line);
    }

    public void addSectionOrParagraphLines(List<String> lines){
        for (String line : lines){
            sectionOrParagraph.addLine(line);
        }
    }


}
