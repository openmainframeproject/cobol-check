package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.features.writer.WriterController;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class writerTest {

    WriterController writerController;
    Writer writer;
    Reader mockedTestSuiteReader;

    @BeforeAll
    public static void generalSetup(){
        Config.load();
    }

    @BeforeEach
    public void commonSetup(){
        mockedTestSuiteReader = Mockito.mock(BufferedReader.class);
        writer = new StringWriter();
        writerController = new WriterController(writer);
    }

    @Test
    void it_formats_a_Cobol_line_based_on_a_String_value() throws IOException {
        String originalText = "           MOVE ALPHA TO BETA.";
        String expectedLine = "           MOVE ALPHA TO BETA.                                                  ";
        expectedLine += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine, writer.toString());
    }

    @Test
    void it_formats_a_Cobol_continuation_line_based_on_a_long_String_value() throws IOException {
        String originalText = "           TESTCASE: \"This testcase name makes the line far, far too long for Cobol.\"";
        String expectedLine1 = "           TESTCASE: \"This testcase name makes the line far, far too lon        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      -    \"g for Cobol.\"                                                       ";
        expectedLine2 += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine1 + expectedLine2, writer.toString());
    }

    @Test
    void it_formats_a_Cobol_continuation_line_when_string_contains_2_types_of_apostrophes() throws IOException {
        String originalText = "           TESTCASE: \"This testcase name makes the line far, far too long for 'Cobol'.\"";
        String expectedLine1 = "           TESTCASE: \"This testcase name makes the line far, far too lon        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      -    \"g for 'Cobol'.\"                                                     ";
        expectedLine2 += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine1 + expectedLine2, writer.toString());
    }

    @Test
    void it_formats_a_Cobol_continuation_line_when_string_contains_2_types_of_apostrophes_switched() throws IOException {
        String originalText = "           TESTCASE: 'This testcase name makes the line far, far too long for \"Cobol\".'";
        String expectedLine1 = "           TESTCASE: 'This testcase name makes the line far, far too lon        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      -    'g for \"Cobol\".'                                                     ";
        expectedLine2 += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine1 + expectedLine2, writer.toString());
    }

    @Test
    void it_formats_a_Cobol_continuation_line_based_on_a_long_String_value_into_4_lines() throws IOException {
        String originalText = "           TESTCASE: 'This testcase name makes the line far, far tooo " +
                "looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo" +
                "oooooooooooooooooooooooooooooooooooooong for Cobol.'";
        String expectedLine1 = "           TESTCASE: 'This testcase name makes the line far, far tooo lo        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      -    'oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine2 += Constants.NEWLINE;
        String expectedLine3 = "      -    'oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine3 += Constants.NEWLINE;
        String expectedLine4 = "      -    'ng for Cobol.'                                                      ";
        expectedLine4 += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine1 + expectedLine2 + expectedLine3 + expectedLine4, writer.toString());
    }

    @Test
    void it_formats_a_long_Cobol_comment_into_3_lines() throws IOException {
        String originalText = "      * This comment is so loooooooooooooooooooooooooooooooooooooooooooo" +
                "oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong, that it needs to be multiple lines.";
        String expectedLine1 = "      * This comment is so loooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      * oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine2 += Constants.NEWLINE;
        String expectedLine3 = "      * ng, that it needs to be multiple lines.                                 ";
        expectedLine3 += Constants.NEWLINE;
        writerController.writeCommentedLine(originalText);
        assertEquals(expectedLine1 + expectedLine2 + expectedLine3, writer.toString());
    }

    @Test
    void it_formats_a_long_Cobol_comment_into_3_lines_without_comment_indicator() throws IOException {
        String originalText = "        This comment is so loooooooooooooooooooooooooooooooooooooooooooo" +
                "oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong, that it needs to be multiple lines.";
        String expectedLine1 = "      * This comment is so loooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      * oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine2 += Constants.NEWLINE;
        String expectedLine3 = "      * ng, that it needs to be multiple lines.                                 ";
        expectedLine3 += Constants.NEWLINE;
        writerController.writeCommentedLine(originalText);
        assertEquals(expectedLine1 + expectedLine2 + expectedLine3, writer.toString());
    }

    @Test
    void it_formats_a_long_Cobol_comment_into_3_lines_without_calling_commentedLine_method() throws IOException {
        String originalText = "      * This comment is so loooooooooooooooooooooooooooooooooooooooooooo" +
                "oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong, that it needs to be multiple lines.";
        String expectedLine1 = "      * This comment is so loooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "      * oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo        ";
        expectedLine2 += Constants.NEWLINE;
        String expectedLine3 = "      * ng, that it needs to be multiple lines.                                 ";
        expectedLine3 += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine1 + expectedLine2 + expectedLine3, writer.toString());
    }

    @Test
    void it_formats_a_long_Cobol_line_by_having_the_new_line_in_a_space() throws IOException {
        String originalText = "            MOVE 'HELLO' TO THIS-FIELD IN WS-THIS IN WS-THAT IN VERY-LONG-LONG-NAME IN " +
                "THIS-FIELD IN AND-ALSO-THIS-FIELD IN GETTING-TIRED IN OF-ALL-THESE-HYPHENS";
        String expectedLine1 = "           MOVE 'HELLO' TO THIS-FIELD IN WS-THIS IN WS-THAT IN                  ";
        expectedLine1 += Constants.NEWLINE;
        String expectedLine2 = "               VERY-LONG-LONG-NAME IN THIS-FIELD IN AND-ALSO-THIS-FIELD         ";
        expectedLine2 += Constants.NEWLINE;
        String expectedLine3 = "               IN GETTING-TIRED IN OF-ALL-THESE-HYPHENS                         ";
        expectedLine3 += Constants.NEWLINE;
        writerController.writeLine(originalText);
        assertEquals(expectedLine1 + expectedLine2 + expectedLine3, writer.toString());
    }

    @Test
    void it_formats_a_Cobol_line_and_comments_it_out_based_on_a_String_value() throws IOException {
        String originalText = "           OPEN INPUT INPUT-FILE";
        String expectedLine = StringHelper.fixedLength(commentOutLine(originalText));
        writerController.writeCommentedLine(originalText);
        assertEquals(expectedLine, writer.toString());
    }

    @Test
    void it_formats_Cobol_lines_based_on_a_list_of_Strings() throws IOException {
        String originalText1 = "           COPY";
        String originalText2 = "              OUTREC.";
        String expectedLine1 = StringHelper.fixedLength(originalText1);
        String expectedLine2 = StringHelper.fixedLength(originalText2);

        List<String> lines = new ArrayList<>();
        lines.add(originalText1);
        lines.add(originalText2);
        writerController.writeLines(lines);

        assertEquals(expectedLine1 + expectedLine2, writer.toString());
    }

    @Test
    void it_formats_Cobol_lines_based_on_a_list_of_strings_and_comments_them_out() throws IOException {
        String originalText1 = "           OPEN INPUT";
        String originalText2 = "              INPUT-FILE";
        String expectedLine1 = StringHelper.fixedLength(commentOutLine(originalText1));
        String expectedLine2 = StringHelper.fixedLength(commentOutLine(originalText2));

        List<String> lines = new ArrayList<>();
        lines.add(originalText1);
        lines.add(originalText2);
        writerController.writeCommentedLines(lines);

        assertEquals(expectedLine1 + expectedLine2, writer.toString());
    }


    private String commentOutLine(String line){
        return "      *" + line.substring(6 + 1);
    }


}
