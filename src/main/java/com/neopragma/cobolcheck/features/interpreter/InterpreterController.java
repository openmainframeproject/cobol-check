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

    public InterpreterController(BufferedReader sourceReader) {
        if (sourceReader == null) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        reader = new CobolReader(sourceReader);
        lineRepository = new LineRepository();
        numericFields = new NumericFields();
    }

    public String interpretNextLine(){
        CobolLine line;
        try{
            line = reader.readLine();
        } catch (IOException ex){
            throw new CobolSourceCouldNotBeReadException(ex);
        }
        if (line == null){
            return null;
        }

        if (!Interpreter.isTooShortToBeMeaningful(line) && !Interpreter.isComment(line)){
            updateDependencies(line);
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

    private void updateDependencies(CobolLine line){
        reader.updateState();
        updateLineRepository(line);

        if (reader.isFlagSet(Constants.DATA_DIVISION)){
            updateNumericFields(line);
        }

        if (reader.isFlagSet(Constants.PROCEDURE_DIVISION)){
            tryReadBatchFileIOStatement();
        }
    }

    public boolean isReading(String partOfProgram){
        return reader.isFlagSet(partOfProgram);
    }

    public boolean hasStatementBeenRead(){
        return reader.hasStatementBeenRead();
    }

    public List<String> getCurrentStatement(){
        return convertToStrings(reader.getCurrentStatement());
    }

    public boolean hasReaderStateChanged() { return reader.hasStateChanged(); }

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

    private void tryReadBatchFileIOStatement(){
        CobolLine line = reader.getCurrentLine();
        if (Interpreter.checkForBatchFileIOStatement(line)){
            try{
                List<CobolLine> lines = reader.readTillEndOfStatement();
            } catch (IOException ex){
                throw new CobolSourceCouldNotBeReadException(ex);
            }

        }
    }

    public List<String> getFileSectionStatements(){
        return lineRepository.getFileSectionStatements();
    }
    public List<String> getFileControlStatements() { return lineRepository.getFileControlStatements(); }
    public Map<String, String> getFileIdentifiersAndStatuses() { return lineRepository.getFileIdentifiersAndStatuses(); }
    public List<String> getCopyTokens() { return lineRepository.getCopyTokens(); }
    public DataType getNumericFieldDataTypeFor(String fieldName){
        return numericFields.dataTypeOf(fieldName);
    }
    public NumericFields getNumericFields() { return numericFields; }

    public boolean currentLineContains(String str){
        return reader.getCurrentLine().contains(str);
    }

    public List<String> getTokensForCurrentLine(){
        return reader.getCurrentLine().getTokens();
    }

    private List<String> convertToStrings(List<CobolLine> lines){
        List<String> stringLines = new ArrayList<>();
        for (CobolLine l : lines){
            stringLines.add(l.getOriginalString());
        }
        return stringLines;
    }

    // Need to save field names of numeric data items in case a test case references them.
    // There's no way to distinguish numeric fields while reading the Procedure Division.
    private void updateNumericFields(CobolLine line){
        if (line.tokensSize() > 1) {
            if (line.contains(Constants.COMP_3_VALUE)) {
                numericFields.setDataTypeOf(line.getToken(1).toUpperCase(Locale.ROOT), DataType.PACKED_DECIMAL);
            } else {
                if (line.contains(Constants.COMP_VALUE)) {
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
     * If we are currently reading the FILE CONTROL paragraph of the program under test, look for specific
     * source lines that require explicit processing by cobol-check.
     * Specifically, we need to save the file identifiers associated with SELECT statements and store the
     * corresponding field names of FILE STATUS specifications in case they are referenced in user-written
     * test cases. We also need to copy any record layout items into Working-Storage, as storage for FD areas
     * will not be allocated when we stub out the OPEN statements for files.
     *
     * @param line - current source line
     */
    private void updateLineRepository(CobolLine line){
        if (reader.isFlagSet(Constants.FILE_CONTROL)){
            lineRepository.addFileControlStatement(line.getOriginalString());

            updateLineRepoBySelectToken(line);
            updateLineRepoByFileStatusToken(line);
        }
        if(reader.isFlagSet(Constants.FILE_SECTION) && reader.isFlagSet(Constants.FD_TOKEN)){
            if (line.contains(Constants.COPY_TOKEN)){
                updateLineRepoByCopyStatement();
                lineRepository.addExpandedCopyStatementsToFileSectionStatements();
            }
            else if(reader.isFlagSet(Constants.LEVEL_01_TOKEN)){
                lineRepository.addFileSectionStatement(line.getOriginalString());
            }
        }
    }

    private void updateLineRepoBySelectToken(CobolLine line){
        if (line.contains(Constants.SELECT_TOKEN)){
            if (line.tokensSize() > 1) {
                lineRepository.addFileIdentifierWithNoStatus(line.getToken(1));
            }
            else {
                // We expect the next token from the source program to be the file identifier associated with the
                // most recent SELECT statement we encountered. It will become a key in a map of file identifiers
                // to file status field names.
                String nextLineToken0 = peekNextMeaningfulLineAndGetTokenAtIndex0();
                if (nextLineToken0 == null)
                {
                    throw new PossibleInternalLogicErrorException("Line after SELECT is empty");
                }
                lineRepository.addFileIdentifierWithNoStatus(nextLineToken0);
            }
        }
    }

    private void updateLineRepoByFileStatusToken(CobolLine line){
        if (line.contains(Constants.FILE_STATUS_TOKEN)){
            if (line.tokensSize() > 2) {
                if (line.getToken(1).equalsIgnoreCase(Constants.IS_TOKEN)) {
                    lineRepository.addStatusForLastSetIdentifier(line.getToken(2));
                }
            } else {
                // When the current source line contains FILE STATUS, the next tokens will be [IS] FIELDNAME.
                // Those tokens may be coded on the same line or on subsequent lines in the source program.
                if (line.tokensSize() > 1) {
                    if (line.getToken(1).equalsIgnoreCase(Constants.IS_TOKEN)) {
                        String nextLineToken0 = peekNextMeaningfulLineAndGetTokenAtIndex0();
                        if (nextLineToken0 == null)
                        {
                            throw new PossibleInternalLogicErrorException("Line after FILE STATUS IS is empty");
                        }
                        lineRepository.addStatusForLastSetIdentifier(nextLineToken0);
                    } else {
                        lineRepository.addStatusForLastSetIdentifier(line.getToken(1));
                    }
                } else {
                    String nextLineToken0 = peekNextMeaningfulLineAndGetTokenAtIndex0();
                    if (nextLineToken0 == null)
                    {
                        throw new PossibleInternalLogicErrorException("Line after FILE STATUS is empty");
                    }
                    lineRepository.addStatusForLastSetIdentifier(nextLineToken0);
                }
            }
        }
    }

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


}
