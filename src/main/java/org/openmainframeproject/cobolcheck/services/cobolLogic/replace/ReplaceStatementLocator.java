package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.*;
import java.util.LinkedList;

/**
 * This class is used to find REPLACE statements in the source code.
 */
public class ReplaceStatementLocator {

    private LinkedList<ReplaceSet> replaceSets = new LinkedList<>();

    // while going through the file, we need to keep track of the current statement
    // because it can be split over multiple lines
    private final ReplaceTokenizer tokenizer = new ReplaceTokenizer();
    protected StringBuilder currentStatement;
    protected boolean we_are_parsing_a_replace_statement = false;
    protected int sourceLinesProcessed = 0;
    protected int commentLinesFound = 0;

    public ReplaceStatementLocator() {
        Log.trace("ReplaceStatementLocator(): No file provided, only for testing purposes");
    }

    public ReplaceStatementLocator(File cobolFile) {
        Log.trace("ReplaceStatementLocator(): Inspecting the COBOL program file: " + cobolFile);
        //Iterate over the file and inspect each line
        try (BufferedReader reader = new BufferedReader(new FileReader(cobolFile))) {
            String line;
            while ((line = reader.readLine()) != null) {
                accumulateStatement(line);
            }
        } catch (FileNotFoundException e) {
            Log.error("ReplaceStatementLocator(): File not found: " + e.getMessage());
            throw new RuntimeException(e);
        } catch (IOException e) {
            Log.error("ReplaceStatementLocator(): Error reading the COBOL program file: " + e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public LinkedList<ReplaceSet> getReplaceSets() {
        return replaceSets;
    }

    protected void accumulateStatement(String line) {
        // tokenize the line
        tokenizer.tokenize(line);

        if (tokenizer.isComment()) {
            commentLinesFound++;
            return;
        }

        sourceLinesProcessed++;
        ReplaceToken t;
        // loop through the tokens
        while (tokenizer.hasMoreTokens()) {
            t = tokenizer.nextToken();
            if (we_are_parsing_a_replace_statement) {
                // if we are parsing a REPLACE statement, accumulate the tokens
                currentStatement.append(" ").append(t.getValue());
            }
            if (t.getType() == ReplaceTokenType.REPLACE) {
                // if we have a REPLACE token, start accumulating the statement
                currentStatement = new StringBuilder().append(t.getValue());
                we_are_parsing_a_replace_statement = true;
            } else if (t.getType() == ReplaceTokenType.TERMINATOR && we_are_parsing_a_replace_statement) {
                // if we have a terminator token, process the statement
                createStatements(currentStatement.toString());
                we_are_parsing_a_replace_statement = false;
            }
        }
    }


    /**
     * process a complete <i>REPLACE</i> statement and create the ReplaceSet objects
     * @param statement string of tokens from replace to terminator (.)
     */
    protected void createStatements(String statement) {
        ReplaceTokenizer statementTokenizer = new ReplaceTokenizer();
        statementTokenizer.tokenize(statement);

        ReplaceSet replaceSet = new ReplaceSet();

        ReplaceToken t;
        boolean nextTokenIsTo = false;

        while (statementTokenizer.hasMoreTokens()) {
            t = statementTokenizer.nextToken();

            // any replace and by keywords are ignored
            // the tokens with other type, are used for the from/to keyset
            switch (t.getType()) {
                case REPLACE:
                    break;
                case BY:
                    nextTokenIsTo = true;
                    break;
                case LEADING:
                    replaceSet.setLeading(true);
                    break;
                case TRAILING:
                    replaceSet.setTrailing(true);
                    break;
                case OTHER:
                    if (nextTokenIsTo) {
                        replaceSet.setTo(t.getValue().replace("==", ""));
                        nextTokenIsTo = false;
                        replaceSets.add(replaceSet);
                        replaceSet = new ReplaceSet();
                    } else {
                        replaceSet.setFrom(t.getValue().replace("==", ""));
                    }
                    break;
            }
        }
    }
}
