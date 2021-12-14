package com.neopragma.cobolcheck.features.writer;

import com.neopragma.cobolcheck.features.interpreter.Interpreter;
import com.neopragma.cobolcheck.services.StringHelper;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

public class CobolWriter {

    Writer writer;
    private boolean currentLineIsComment;

    public CobolWriter(Writer writer){
        this.writer = writer;
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
        if (line.length() <= 72){
            line = StringHelper.fixedLength(line);
            writer.write(line);
        }
        else {
            //We need to check if this line is to be commented out or if it is already a comment
            currentLineIsComment = currentLineIsComment || Interpreter.isComment(line);
            writeMultiLine(line, currentLineIsComment);
        }
        currentLineIsComment = false;
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

    void close() throws IOException {
        writer.close();
    }


    /**
     * Lines of test code in a test suite are Cobol-like, but not strictly Cobol. The descriptions for TESTSUITE and
     * TESTCASE specifications may exceed the maximum length allowed for Area B in the generated test Cobol program.
     * This method splits the literal and writes the value with a continuation line, if necessary. If the line must
     * be split into 3 or more lines, this method calls itself recursively. If the line is a comment, subsequent
     * lines will receive a '*' marking, to turn it into a comment.
     *
     * @param line - original line from test suite.
     * @param isComment should be true if the given line is a comment
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void writeMultiLine(String line, boolean isComment) throws IOException {
        String line1 = line.substring(0,72);
        String line2 = line.substring(72);
        writeLine(line1);
        if (line2.length() > 0 && !isComment) {
            if (line.contains("\"") || line.contains("'"))
            if (StringHelper.occursFirst(line, '\'', '"')){
                line2 = ("      -    '" + line2);
            }
            else {
                line2 = ("      -    \"" + line2);
            }
        }
        else if (line2.length() > 0 && isComment){
            line2 = ("      * " + line2);
        }
        if (line2.length() > 72){
            writeMultiLine(line2, isComment);
        }
        else {
            writeLine(line2);
        }
    }
}
