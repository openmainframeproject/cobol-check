package com.neopragma.cobolcheck.features.writer;

import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.StringHelper;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

public class CobolWriter {

    Writer writer;

    public CobolWriter(Writer writer){
        this.writer = writer;
    }

    public void writeLine(String line) throws IOException {
        line = removeEndingSpaces(line);
        if (line.length() <= 72){
            line = StringHelper.fixedLength(line);
            writer.write(line);
        }
        //TODO: Handle comments for multiLine!!
        else {
            writeMultiLine(line);
        }
    }

    public void writeCommentedLine(String line) throws IOException {
        writeLine(commentOutLine(line));
    }

    public void writeFormattedLine(String format, Object... args) throws IOException {
        writeLine(String.format(format, args));
    }

    public void writeStatement(List<String> statement) throws IOException {
        for (String line : statement){
            writeLine(line);
        }
    }

    public void writeCommentedStatement(List<String> statement) throws IOException {
        for (String line : statement){
            writeLine(commentOutLine(line));
        }
    }

    void close() throws IOException {
        writer.close();
    }

    private String removeEndingSpaces(String line){
        //Regex for trailing spaces
        String regex = "\\s+$";
        return line.replaceAll(regex, "");
    }

    private String commentOutLine(String line){
        return "      *" + line.substring(6 + 1);
    }

    /**
     * Lines of test code in a test suite are Cobol-like, but not strictly Cobol. The descriptions for TESTSUITE and
     * TESTCASE specifications may exceed the maximum length allowed for Area B in the generated test Cobol program.
     * This method splits the literal and writes the value with a continuation line, if necessary. If the line must
     * be split into 3 or more lines, this method calls itself recursively.
     *
     * @param line - original line from test suite.
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void writeMultiLine(String line) throws IOException {
        String line1 = line.substring(0,72);
        String line2 = line.substring(72);
        writeLine(line1);
        if (line2.length() > 0) {
            line2 = ("      -    \"" + line2);
        }
        if (line2.length() > 72){
            writeMultiLine(line2);
        }
        else {
            writeLine(line2);
        }
    }
}
