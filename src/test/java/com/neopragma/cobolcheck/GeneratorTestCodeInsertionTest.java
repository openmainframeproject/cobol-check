package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

@ExtendWith(MockitoExtension.class)
public class GeneratorTestCodeInsertionTest implements Constants {

    private Generator generator;
    private Messages messages = new Messages();
    private Log log = new Log();
    private TokenExtractor tokenExtractor = new StringTokenizerExtractor(messages);

    @Mock
    Reader mockTestSuite;

    private String[] simple1CobolSource = new String[] {
        " IDENTIFICATION DIVISION.",
        " PROGRAM-ID. SIMPLE1.",
        " ENVIRONMENT DIVISION.",
        "       DATA DIVISION.",
        "       WORKING-STORAGE SECTION.",
        "       01  W-GROUP-1.",
        "           05  W-FIELD-1 PIC X(03) VALUE \"FOO\".",
        "           05  FILLER    PIC X(01) VALUE \"-\".",
        "           05  W-FIELD-2 PIC X(03) VALUE \"XXX\".",
        "       PROCEDURE DIVISION.",
        "       100-APPEND-TEXT.",
        "           MOVE \"ZZZ\" TO W-FIELD-2",
        "           DISPLAY W-GROUP-1 UPON CONSOLE.",
        "           .",
        "       200-NOT-UNDER-TEST.",
        "           DISPLAY \"Should not see this.\" UPON CONSOLE",
        "           ."
    };


    @BeforeEach
    public void commonSetup() {
        generator = new Generator(messages, log, tokenExtractor);
    }

    @Test
    public void it_inserts_test_copybooks_in_the_right_places() {
        StringReader cobolSourceIn = makeCobolSourceProgram(simple1CobolSource);
        StringWriter testSourceOut = new StringWriter();
        generator.mergeTestSuite(mockTestSuite, cobolSourceIn, testSourceOut);


        System.out.println("testSourceOut: ");
        System.out.println(testSourceOut.toString());

//        String data = testSourceOut.toString();
//
//        int offset = 0;
//        int length = 81;
//        int count = (data.length() / 81) - 1;
//        System.out.println("crude line count = " + count);
//        while(offset < data.length()) {
//            System.out.println("Output Line: <" + data.substring(offset,offset+length) + ">");
//            offset += length;
//            count++;
//        }

    }

    private StringReader makeCobolSourceProgram(String[] sourceLines) {
        StringBuilder sourceCode = new StringBuilder();
        for (String sourceLine : sourceLines) {
            sourceCode.append("      " + String.format("%1$-74s", sourceLine) + NEWLINE);
        }
        return new StringReader(sourceCode.toString());
    }
}
