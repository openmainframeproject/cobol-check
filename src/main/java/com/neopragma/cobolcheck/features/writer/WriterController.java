package com.neopragma.cobolcheck.features.writer;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.services.Messages;

import java.io.*;
import java.util.List;

public class WriterController {

    private CobolWriter cobolWriter;

    public WriterController(Writer testSourceWriter){
        cobolWriter = new CobolWriter(testSourceWriter);
    }
    public WriterController(CobolWriter cobolWriter){
        this.cobolWriter = cobolWriter;
    }

    /**
     * Writes a line of cobol code to the test output file. If the given line is too
     * long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param line - line to be written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public void writeLine(String line) throws IOException { cobolWriter.writeLine(line);}

    /**
     * Writes an out-commented line of cobol code to the test output file. If the given line is too
     * long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param line - line to be commented out and then written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public void writeCommentedLine(String line) throws IOException { cobolWriter.writeCommentedLine(line); }

    /**
     * Writes all the given lines of cobol code to the test output file. If the any of the lines
     * are too long for cobol to handle, it will be correctly split into multiple lines.
     *
     * @param lines - lines to be written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public void writeLines(List<String> lines) throws IOException { cobolWriter.writeLines(lines); }

    /**
     * Writes and out-comments all the given lines of cobol code to the test output file.
     * If the any of the lines are too long for cobol to handle, it will be correctly
     * split into multiple lines.
     *
     * @param lines - lines to be commented out, then written
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public void writeCommentedLines(List<String> lines) throws IOException {
        cobolWriter.writeCommentedLines(lines);
    }

    public void closeWriter(String programName) {
        try {
            cobolWriter.close();
        } catch (IOException closeTestSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR017", programName));
        }
    }

}
