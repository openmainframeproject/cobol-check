package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.cobolLogic.DataType;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class InterpreterController {
    private CobolReader reader;
    private LineRepository lineRepository;
    private NumericFields numericFields;
    private boolean hasReadLine;
    private String possibleMockIdentifier;

    public InterpreterController(BufferedReader sourceReader) {
        if (sourceReader == null) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        reader = new CobolReader(sourceReader);
        lineRepository = new LineRepository();
        numericFields = new NumericFields();
    }

    //Getters for lists of specific source lines
    public List<String> getFileSectionStatements(){
        return lineRepository.getFileSectionStatements();
    }
    public List<String> getFileControlStatements() { return lineRepository.getFileControlStatements(); }
    public Map<String, String> getFileIdentifiersAndStatuses() { return lineRepository.getFileIdentifiersAndStatuses(); }
    public List<String> getCopyTokens() { return lineRepository.getCopyTokens(); }

    //Getters for numeric field info
    public DataType getNumericFieldDataTypeFor(String fieldName){
        return numericFields.dataTypeOf(fieldName);
    }
    public NumericFields getNumericFields() { return numericFields; }

    //Getters from reader
    public List<String> getTokensForCurrentLine(){
        return reader.getCurrentLine().getTokens();
    }
    public List<String> getCurrentStatement(){
        return convertToStrings(reader.getCurrentStatement());
    }

    //Getting info from reader
    public boolean hasStatementBeenRead(){
        return reader.hasStatementBeenRead();
    }
    public boolean isReading(String partOfProgram){
        return reader.isFlagSet(partOfProgram);
    }
    public boolean didLineJustEnter(String partOfProgram) { return partOfProgram.equals(reader.getLineJustEntered()); }
    public boolean hasReaderStateChanged() {return reader.getLineJustEntered() != null;}
    public boolean currentLineContains(String str){
        return reader.getCurrentLine().containsToken(str);
    }

    //Mock info
    public boolean isCurrentComponentMockable() { return possibleMockIdentifier != null; }
    public String getPossibleMockIdentifier() { return possibleMockIdentifier; }

    public boolean shouldCurrentLineBeParsed(){
        return Interpreter.shouldLineBeParsed(reader.getCurrentLine(), reader.getState());
    }

    public boolean shouldCurrentLineBeCommentedOut(){
        return Interpreter.shouldLineBeCommentedOut(reader.getCurrentLine(), reader.getState());
    }

    public boolean shouldCurrentStatementBeCommentedOut(){
        for (CobolLine line : reader.getCurrentStatement()){
            if (Interpreter.shouldLineBeCommentedOut(line, reader.getState())){
                return true;
            }
        }
        return false;
    }

    public boolean doesCurrentLineEndInPeriod() {
        return Interpreter.doesCurrentLineEndInPeriod(reader.getCurrentLine());
    }

    /**Interprets the next line from the source file. Based on the line, the following values
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
     * @return The line that was read as a string
     */
    public String interpretNextLine(){
        CobolLine line;
        try{
            line = reader.readLine();

            if (line == null){
                if (!hasReadLine){
                    throw new PossibleInternalLogicErrorException(Messages.get("ERR007"));
                }
                return null;
            }

            if (!Interpreter.isTooShortToBeMeaningful(line) && !Interpreter.isComment(line)){
                updateDependencies(line);
                hasReadLine = true;

            }
        } catch (IOException ex){
            throw new CobolSourceCouldNotBeReadException(ex);
        }

        return line.getOriginalString();
    }

    public void closeReader(){
        try{
            reader.close();
        } catch (IOException ex){
            throw new CobolSourceCouldNotBeReadException(ex);
        }
    }

     /**Updates dependencies of the interpreter.
      * The following values can be updated, based on the given line:
     * - Current statement (only if multiline statement was read)
     * - The state of the reader (flags that shows what part of the program we are reading)
     * - List of FILE SECTION statements
     * - List of FILE CONTROL statements
     * - List mapping file identifiers to statuses
     * - List of copy tokens
     * - Numeric fields
      * @param line - The line the update is based upon
     */
    private void updateDependencies(CobolLine line) throws IOException {
        reader.updateState();
        updateLineRepository(line);

        if (reader.isFlagSet(Constants.DATA_DIVISION)){
            updateNumericFields(line);
        }

        if (reader.isFlagSet(Constants.PROCEDURE_DIVISION)){
            possibleMockIdentifier = null;
            updatePossibleMock(line);
            tryReadBatchFileIOStatement();
        }
    }

    /**Updates possibleMockIdentifier if a specific part of the program was just entered.
     * @param line - The line the update is based upon
     */
    private void updatePossibleMock(CobolLine line) throws IOException {
        if (didLineJustEnter(Constants.SECTION_TOKEN)){
            possibleMockIdentifier = Interpreter.getSectionOrParagraphName(line);
        }
        if (Interpreter.isParagraphHeader(line, reader.peekNextMeaningfulLine(), reader.getState())){
            possibleMockIdentifier = Interpreter.getSectionOrParagraphName(line);
        }
    }

    /**If the current line is a batch file IO statement, the reader will read the
     * whole statement no matter how many lines it spans.
     */
    private void tryReadBatchFileIOStatement(){
        CobolLine line = reader.getCurrentLine();
        if (Interpreter.checkForBatchFileIOStatement(line)){
            try{
                reader.readTillEndOfStatement();
            } catch (IOException ex){
                throw new CobolSourceCouldNotBeReadException(ex);
            }

        }
    }
    /**If the current line is a numeric field, it will be added to the list of numeric fields.
     * We need to save field names of numeric data items in the DATA DIVISION in case a test case
     * references them. There's no way to distinguish numeric fields while reading the PROCEDURE
     * DIVISION.
     */
    private void updateNumericFields(CobolLine line){
        if (line.tokensSize() > 1) {
            if (line.containsToken(Constants.COMP_3_VALUE)) {
                numericFields.setDataTypeOf(line.getToken(1).toUpperCase(Locale.ROOT), DataType.PACKED_DECIMAL);
            } else {
                if (line.containsToken(Constants.COMP_VALUE)) {
                    numericFields.setDataTypeOf(line.getToken(1).toUpperCase(Locale.ROOT), DataType.FLOATING_POINT);
                } else {
                    int ix = 0;
                    for (String token : line.getTokens()) {
                        if (token.equalsIgnoreCase(Constants.PIC_VALUE)
                                || (token.equalsIgnoreCase(Constants.PICTURE_VALUE))) {
                            Pattern pattern = Pattern.compile(Constants.NUMERIC_PICTURE_CLAUSE_PATTERN);
                            Matcher matcher = pattern.matcher(line.getToken(ix + 1));
                            boolean matched = matcher.find();
                            if (matched) {
                                numericFields.setDataTypeOf(line.getToken(1).toUpperCase(Locale.ROOT), DataType.DISPLAY_NUMERIC);
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
     * Updates the line repository with the given line, if it might have further use when testing.
     * Lines ahead of the current one, might be peeked or even read, if the current statement spans
     * multiple lines. If the line fits any of below it will be saved in the repository:
     * - It is a line read under FILE CONTROL
     * - It has a SELECT token. File identifier and corresponding FILE STATUS will be saved
     * - It contains a copy token
     * - It is a file section statement (file section statements from referenced copybooks are also included)
     * @param line - current source line
     */
    private void updateLineRepository(CobolLine line) throws IOException {
        if (reader.isFlagSet(Constants.FILE_CONTROL)){
            lineRepository.addFileControlStatement(line.getOriginalString());

            updateLineRepoBySelectToken(line);
            updateLineRepoByFileStatusToken(line);
        }
        if(reader.isFlagSet(Constants.FILE_SECTION) && reader.isFlagSet(Constants.FD_TOKEN)){
            if (line.containsToken(Constants.COPY_TOKEN)){
                updateLineRepoByCopyStatement();
                lineRepository.addExpandedCopyStatementsToFileSectionStatements();
            }
            else if(reader.isFlagSet(Constants.LEVEL_01_TOKEN)){
                lineRepository.addFileSectionStatement(line.getOriginalString());
            }
        }
    }

    /**
     * If the given line contains a SELECT token, the file identifier will be added, waiting for
     * a mapping to a corresponding file status.
     * @param line - current source line
     */
    private void updateLineRepoBySelectToken(CobolLine line){
        if (line.containsToken(Constants.SELECT_TOKEN)){
            if (line.tokensSize() > 1) {
                lineRepository.addFileIdentifierWithNoStatus(line.getToken(1));
            }
            else {
                // First token on the next line should be the file identifier
                String nextLineToken0 = peekNextMeaningfulLineAndGetTokenAtIndex0();
                if (nextLineToken0 == null)
                {
                    throw new PossibleInternalLogicErrorException("Line after SELECT is empty");
                }
                lineRepository.addFileIdentifierWithNoStatus(nextLineToken0);
            }
        }
    }

    /**
     * If the given line contains a file status token, it will be added to a map to a
     * corresponding file identifier.
     * @param line - current source line
     */
    private void updateLineRepoByFileStatusToken(CobolLine line) throws IOException {
        if (line.containsToken(Constants.FILE_STATUS_TOKEN)){
            if (Interpreter.isEndOfStatement(line, reader.peekNextMeaningfulLine())){
                //File status statement is on one line
                if (line.tokensSize() > 2) {
                    if (line.getToken(1).equalsIgnoreCase(Constants.IS_TOKEN)) {
                        lineRepository.addStatusForLastSetIdentifier(line.getToken(2));
                    }
                }
                else if (line.tokensSize() > 1){
                    if (!line.containsToken(Constants.IS_TOKEN)) {
                        lineRepository.addStatusForLastSetIdentifier(line.getToken(1));
                    }
                }
            } else {
                //File status statement is written across multiple lines
                List<CobolLine> statement = reader.readTillEndOfStatement();
                for (CobolLine l : statement){
                    List<String> tokens = l.getTokens();
                    for(String token : tokens){
                        if (!token.equalsIgnoreCase(Constants.FILE_STATUS_TOKEN) && !token.equalsIgnoreCase(Constants.IS_TOKEN)){
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
    private void updateLineRepoByCopyStatement(){
        List<CobolLine> copystatementLines;
        try{
            copystatementLines = reader.readTillEndOfStatement();
        } catch (IOException ex){
            throw new CobolSourceCouldNotBeReadException(ex);
        }

        if (copystatementLines == null)
        {
            throw new PossibleInternalLogicErrorException("Copy statements was read as null");
        }
        for(CobolLine line : copystatementLines){
            if(!Interpreter.isComment(line) && !Interpreter.isEmpty(line) && !Interpreter.isTooShortToBeMeaningful(line)){
                lineRepository.addAccumulatedTokensFromCopyStatementToCopyTokens(line.getOriginalString());
            }

        }
    }

    private String peekNextMeaningfulLineAndGetTokenAtIndex0()
    {
        CobolLine peekedLine;
        try{
            peekedLine = reader.peekNextMeaningfulLine();
        } catch (IOException ex){
            throw new CobolSourceCouldNotBeReadException(ex);
        }

        if (peekedLine != null && peekedLine.tokensSize() > 0){
            return peekedLine.getToken(0);
        }
        else {
            return null;
        }
    }

    private List<String> convertToStrings(List<CobolLine> lines){
        List<String> stringLines = new ArrayList<>();
        for (CobolLine l : lines){
            stringLines.add(l.getOriginalString());
        }
        return stringLines;
    }
}
