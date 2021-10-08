package com.neopragma.cobolcheck.features.interpreter;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.cobolLogic.TokenExtractor;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class CobolReader {
    private BufferedReader reader;

    private State state;
    private TokenExtractor tokenExtractor;

    private CobolLine prevoiusLine;
    private CobolLine currentLine;
    private List<CobolLine> nextLines;
    private List<CobolLine> currentStatement;

    private boolean stateHasChanged = false;

    public CobolReader(BufferedReader sourceReader) {
        reader = sourceReader;
        state = new State();
        tokenExtractor = new StringTokenizerExtractor();
        nextLines = new ArrayList<>();
        currentStatement = new ArrayList<>();
    }

    /**
     * Reads the next line of the cobol file and updates the status of the reader based on the line.
     * Returns a CobolLine
     *
     * @throws IOException - pass any IOExceptions up to the caller
     */
    public CobolLine readLine() throws IOException {
        currentStatement = null;
        if (!nextLines.isEmpty()){
            prevoiusLine = currentLine;
            currentLine = nextLines.get(0);
            nextLines.remove(0);
            return  currentLine;
        }
        String line = reader.readLine();
        if (line == null){
            return null;
        }
        prevoiusLine = currentLine;
        currentLine = new CobolLine(line, tokenExtractor);
        return currentLine;
    }

    void updateState(){
        stateHasChanged = Interpreter.setFlagsForCurrentLine(currentLine, state);
    }

    public void close() throws IOException {
        reader.close();
    }

    public State getState() {return state; }

    public boolean hasStateChanged() {
        return stateHasChanged;
    }

    public CobolLine getPrevoiusLine() {
        return prevoiusLine;
    }
    public CobolLine getCurrentLine() { return currentLine; }

    boolean hasStatementBeenRead(){
        return currentStatement != null;
    }

    List<CobolLine> getCurrentStatement(){
        return currentStatement;
    }

    /**
     * Peeks the next line of the cobol file that is meaningful - that is; a line that is
     * not empty or a comment.
     * Peeking does not alter the reader in any way, thus next time ReadLine() is called,
     * the returned line will not be any different from the one you would have gotten,
     * if you had not peeked.
     * Peeking more than once, without having read the peeked line, will return the same
     * line.
     * @return the first peeked CobolLine that is neither empty nor a comment.
     *
     * @throws IOException - pass any IOExceptions up to the caller
     */
    public CobolLine peekNextMeaningfulLine() throws IOException {
        if (!nextLines.isEmpty()){
            return nextLines.get(nextLines.size() - 1);
        }
        while (true){
            String line = reader.readLine();
            if (line == null){
                return null;
            }
            CobolLine cobolLine = new CobolLine(line, tokenExtractor);
            nextLines.add(cobolLine);
            if (!Interpreter.isEmpty(cobolLine) && !Interpreter.isComment(cobolLine)){
                return cobolLine;
            }
        }
    }

    /**
     * Reads the current statement till the end. The reader only updates its state at
     * the end of the statement.
     *
     * @return Each line in the statement as a list
     *
     * @throws IOException - pass any IOExceptions up to the caller
     * @throws PossibleInternalLogicErrorException - If a line is null, the cobol-program
     * would terminate mid-statement
     */
    public List<CobolLine> readTillEndOfStatement() throws IOException {
        List<CobolLine> statementLines = new ArrayList<>();

        statementLines.add(currentLine);
        peekNextMeaningfulLine();

        while (!Interpreter.isEndOfStatement(currentLine, nextLines.get(nextLines.size() - 1))){
            readLine();
            if (currentLine == null){
                throw new PossibleInternalLogicErrorException("File ends mid-statement");
            }
            statementLines.add(currentLine);
            peekNextMeaningfulLine();
        }
//        currentLine = readLine();
        currentStatement = statementLines;
        return statementLines;
    }

    public boolean isFlagSet(String partOfProgram){
        return state.getFlags().get(partOfProgram).isSet();
    }


}
