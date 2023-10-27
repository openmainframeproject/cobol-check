package org.openmainframeproject.cobolcheck.features.writer;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter;
import org.openmainframeproject.cobolcheck.services.StringHelper;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

public class CobolWriter {

    Writer writer;
    private boolean currentLineIsComment;
    private final int maxLineLength = 72;

    private boolean storeLines = false;
    private final List<String> storedLines = new ArrayList<>();
    private final String stubTag;

    public CobolWriter(Writer writer){
        this.writer = writer;
        stubTag = Config.getStubTag();
    }

    /**
     * Writes a line of cobol code to the test output file. If the given line is too
     * long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param line - line to be written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeLine(String line) throws IOException {
        line = StringHelper.removeTrailingSpaces(line);
        if (line.length() <= maxLineLength){
            line = StringHelper.fixedLength(line);
            if (storeLines)
                storedLines.add(line);
            else
                writer.write(line);
        }
        else {
            //We need to check if this line is to be commented out or if it is already a comment
            currentLineIsComment = currentLineIsComment || Interpreter.isComment(line);
            writeMultiLine(line, currentLineIsComment, false);
        }
        currentLineIsComment = false;
    }

    /**
     * Puts the writer in a state, where lines will be stored for later use, rather than written,
     * to the file.
     */
    public void startStoringLines(){
        storeLines = true;
    }

    /**
     * Makes the writer write lines, instead of storing them.
     */
    public void stopStoringLines() {
        storeLines = false;
    }

    /**
     * Writes all stored lines, and clears the stored lines.
     */
    public void releaseStoredLines() throws IOException {
        writeLines(storedLines);
        storedLines.clear();
    }

    /**
     * Writes an out-commented line of cobol code to the test output file. If the given line is too
     * long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param line - line to be commented out and then written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeCommentedLine(String line) throws IOException {
        currentLineIsComment = true;
        writeLine(StringHelper.commentOutLine(line));
    }

    /**
     * Writes an out-commented line with a stub-tag to the test output file. If the given line is too
     * long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param line - line to be commented out and then written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeStubbedLine(String line) throws IOException {
        currentLineIsComment = true;
        writeLine(StringHelper.stubLine(line, stubTag));
    }

    void writeFormattedLine(String format, Object... args) throws IOException {
        writeLine(String.format(format, args));
    }

    /**
     * Writes all the given lines of cobol code to the test output file. If the any of the lines
     * are too long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param lines - lines to be written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeLines(List<String> lines) throws IOException {
        for (String line : lines){
            writeLine(line);
        }
    }

    /**
     * Comments out and writes all the given lines of cobol code to the test output file.
     * If the any of the lines are too long for cobol to handle, it will be correctly
     * split into multiple lines.
     *
     * @param lines - lines to be commented out, then written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeCommentedLines(List<String> lines) throws IOException {
        for (String line : lines){
            writeCommentedLine(line);
        }
    }

    /**
     * Comments out and adds a stub-tag and writes all the given lines of cobol code to the test output file.
     * If the any of the lines are too long for cobol to handle, it will be correctly
     * split into multiple lines.
     *
     * @param lines - lines to be commented out, then written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    void writeStubbedLines(List<String> lines) throws IOException {
        for (String line : lines){
            writeStubbedLine(line);
        }
    }

    void close() throws IOException {
        writer.close();
    }


    /**
     * Lines of test code in a test suite are Cobol-like, but not strictly Cobol. The descriptions for TESTSUITE and
     * TESTCASE specifications may exceed the maximum length allowed for Area B in the generated test Cobol program.
     * This method splits the line based on the context:
     * - If the split happens during a string literal subsequent lines will receive a continuation line.
     * - If the line is a comment, subsequent lines will receive a '*' marking, to turn it into a comment.
     * - If the split happens during code elements, the line will be split at a fitting space
     * If the line must be split into 3 or more lines, this method calls itself recursively.
     *
     * @param line - original line from test suite.
     * @param isComment should be true if the given line is a comment
     * @param isRecursiveCall if the call comes from within the method itself
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void writeMultiLine(String line, boolean isComment, boolean isRecursiveCall) throws IOException {
        String line1 = line.substring(0,maxLineLength);
        String line2 = line.substring(maxLineLength);
        if (line2.length() > 0 && !isComment) {
            //String continuation line
            char stringChar = getStringContinuationSign(line1, line2);
            if (stringChar == '\''){
                line2 = ("      -    '" + line2);
            }
            else if (stringChar == '"') {
                line2 = ("      -    \"" + line2);
            }
            else {
                //Continue line at applicable space (line is split neither during string nor comment)
                String[] words = null;
                if (StringHelper.isStringContinuationLine(line)){
                    words = line.substring(11).trim().split(" ");
                }
                else{
                    words = line.trim().split(" ");
                }
                if (StringHelper.isStringContinuationLine(line))
                    line1 = "      -    ";
                else if (isRecursiveCall)
                    line1 = "               ";
                else
                    line1 = "           ";
                line2 = "               ";
                for (int i = 0; i < words.length; i++){
                    if (line1.length() + words[i].length() <= maxLineLength)
                        line1 += words[i] + " ";
                    else {
                        line1 = line1.substring(0, line1.length() - 1);
                        line2 += line.trim().substring(line1.trim().length()).trim();
                        break;
                    }
                }
            }
            writeLine(line1);
        }
        else if (line2.length() > 0 && isComment){
            //Continue comment
            writeLine(line1);
            line2 = StringHelper.commentOutLine(line2);
        }
        //Handle line 2
        if (line2.length() > maxLineLength){
            writeMultiLine(line2, isComment, true);
        }
        else {
            writeLine(line2);
        }
    }

    private char getStringContinuationSign(String line1, String line2){
        if ((line1.contains("\"") || line1.contains("'")) && (line2.contains("\"") || line2.contains("'"))){
            boolean insideString = false;
            char stringSign = ' ';
            for (char c : line1.toCharArray()){
                if (c == '\''){
                    if (insideString && stringSign == '\''){
                        insideString = false;
                        stringSign = ' ';
                    }
                    else if (!insideString){
                        insideString = true;
                        stringSign = '\'';
                    }
                }
                else if (c == '"'){
                    if (insideString && stringSign == '"'){
                        insideString = false;
                        stringSign = ' ';
                    }
                    else if (!insideString){
                        insideString = true;
                        stringSign = '"';
                    }
                }
            }
            return stringSign;
        }else {
            return ' ';
        }
    }
}
