package org.openmainframeproject.cobolcheck.features.interpreter;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.cobolLogic.CobolLine;
import org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;

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
    private int lineNumber;

    private String lineJustEneterd = null;

    public CobolReader(BufferedReader sourceReader) {
        reader = sourceReader;
        state = new State();
        tokenExtractor = new StringTokenizerExtractor();
        nextLines = new ArrayList<>();
        currentStatement = new ArrayList<>();
    }

    State getState() {return state; }
    CobolLine getCurrentLine() { return currentLine; }
    CobolLine getPrevoiusLine() { return prevoiusLine; }
    List<CobolLine> getCurrentStatement(){ return currentStatement; }

    public String getLineJustEntered() { return lineJustEneterd; }
    boolean hasStatementBeenRead(){ return currentStatement != null; }
    int getLineNumber() { return lineNumber; }

    /**
     * Reads the next line of the cobol file.
     *
     * @return (CobolLine) The line that was read
     *
     * @throws IOException - pass any IOExceptions up to the caller
     */
    CobolLine readLine() throws IOException {
        currentStatement = null;
        lineNumber++;
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

    //Will potentially make interpretation easier (not used)
    CobolLine readStatementAsOneLine() throws IOException {
        while (!Interpreter.isEndOfStatement(currentLine, peekNextMeaningfulLine())){
            appendNextMeaningfulLineToCurrentLine();
        }
        return currentLine;
    }

    /**
     * Sets and unsets flags that signifies the current state of the cobol being read, based
     * on the current line.
     */
    void updateState() throws IOException {
        CobolLine nextLine = peekNextMeaningfulLine();
        lineJustEneterd = Interpreter.setFlagsForCurrentLine(currentLine, nextLine, state);
    }

    void close() throws IOException {
        reader.close();
    }

    CobolLine appendNextMeaningfulLineToCurrentLine() throws IOException{
        List<CobolLine> statementLines = new ArrayList<>();
        CobolLine nextMeaningfulLine = peekNextMeaningfulLine();
        if (Interpreter.containsOnlyPeriod(nextMeaningfulLine)){
            currentLine = new CobolLine(currentLine.getOriginalString() +
                    nextMeaningfulLine.getTrimmedString(), tokenExtractor);
        }
        else {
            currentLine = new CobolLine(currentLine.getOriginalString() + " " +
                    nextMeaningfulLine.getTrimmedString(), tokenExtractor);
        }

        nextLines.remove(nextLines.size() - 1);

        if (!nextLines.isEmpty()){
            statementLines.add(currentLine);
            statementLines.addAll(nextLines);
            currentStatement = statementLines;
            nextLines.clear();
        }

        return currentLine;

    }

    /**
     * Appends the given String to the current line
     *
     * @param appendString - The string to append
     * @return The line, with the given String appended
     */
    CobolLine appendToCurrentLine(String appendString){
        currentLine = new CobolLine(currentLine.getOriginalString() + appendString, tokenExtractor);
        return currentLine;
    }

    /**
     * Turns the current read line into a read statement (if not already a statement).
     * Adds the given line as the first statement line.
     * @param line - The line to add
     */
    void addLineBeforeCurrentRead(String line){
        if (currentStatement == null){
            currentStatement = new ArrayList<>();
            currentStatement.add(currentLine);
        }
        currentStatement.add(0, new CobolLine(line, tokenExtractor));
    }

    /**
     * Turns the current read line into a read statement (if not already a statement).
     * Adds the given line as the last statement line.
     * @param line - The line to add
     */
    void addLineAfterCurrentRead(String line){
        if (currentStatement == null){
            currentStatement = new ArrayList<>();
            currentStatement.add(currentLine);
        }
        currentStatement.add(new CobolLine(line, tokenExtractor));
    }

    /**
     * Peeks the next line of the cobol file that is meaningful - that is; a line that is
     * not empty nor a comment.
     * Peeking does not alter the reader in any way, thus next time ReadLine() is called,
     * the returned line will not be any different from the one you would have gotten,
     * if you had not peeked.
     * Peeking more than once, without having read the peeked line, will return the same
     * line.
     * @return the first peeked CobolLine that is neither empty nor a comment.
     *
     * @throws IOException - pass any IOExceptions up to the caller
     */
    CobolLine peekNextMeaningfulLine() throws IOException {
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
            if (Interpreter.isMeaningful(cobolLine)){
                return cobolLine;
            }
        }
    }

    void putNextLine(CobolLine line){
        nextLines.add(0, line);
    }

    void putNextLine(String line){
        nextLines.add(0, new CobolLine(line, tokenExtractor));
    }

    /**
     * Reads the current statement till the end. This will forward the reader till the end of
     * the statement.
     *
     * @return Each line in the statement as a list
     *
     * @throws IOException pass any IOExceptions up to the caller
     * @throws PossibleInternalLogicErrorException If a line is null, the cobol-program
     * would terminate mid-statement
     */
    List<CobolLine> readTillEndOfStatement() throws IOException {
        List<CobolLine> statementLines = new ArrayList<>();

        statementLines.add(currentLine);
        peekNextMeaningfulLine();

        while (nextLines.size() > 0 && !Interpreter.isEndOfStatement(currentLine, nextLines.get(nextLines.size() - 1))){
            readLine();
            if (currentLine == null){
                throw new PossibleInternalLogicErrorException("File ends mid-statement");
            }
            statementLines.add(currentLine);
            peekNextMeaningfulLine();
        }
        currentStatement = statementLines;
        return statementLines;
    }

    /**
     * Checks whether a flag is set in the state of the reader, for a specific part of the program.
     * If this is the case, it means, that we are currently reading that part.
     *
     * @param partOfProgram - The part of the program to check - ex.: "IDENTIFICATION DIVISION".
     * @return True if the flag is set
     */
    boolean isFlagSet(String partOfProgram){
        return state.getFlags().get(partOfProgram).isSet();
    }


}
