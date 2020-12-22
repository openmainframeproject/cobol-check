/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.testhelpers.MD5;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
public class GeneratorTestCodeInsertionIT implements Constants {

    private Generator generator;
    private static final Messages messages = new Messages();
    private final Log log = new Log();
    private final TokenExtractor tokenExtractor = new StringTokenizerExtractor(messages);
    private static final Config config = new Config(messages);
    private static String pathToTestCobolSources = EMPTY_STRING;

    @Mock
    Reader mockTestSuite;

    private final String[] cobolSourceWithWorkingStorage = new String[] {
            " IDENTIFICATION DIVISION.",
            " PROGRAM-ID. SIMPLE1.",
            " ENVIRONMENT DIVISION.",
            " DATA DIVISION.",
            " LOCAL-STORAGE SECTION.",
            " 01  W-GROUP-1.",
            "     05  W-FIELD-1 PIC X(03) VALUE \"FOO\".",
            "     05  FILLER    PIC X(01) VALUE \"-\".",
            "     05  W-FIELD-2 PIC X(03) VALUE \"XXX\".",
            " PROCEDURE DIVISION.",
            " 100-APPEND-TEXT.",
            "     MOVE \"ZZZ\" TO W-FIELD-2",
            "     DISPLAY W-GROUP-1 UPON CONSOLE.",
            "     .",
            " 200-NOT-UNDER-TEST.",
            "     DISPLAY \"Should not see this.\" UPON CONSOLE",
            "     ."
    };

    private final String[] cobolSourceWithoutWorkingStorage = new String[] {
            " IDENTIFICATION DIVISION.",
            " PROGRAM-ID. SIMPLE1.",
            " ENVIRONMENT DIVISION.",
            " DATA DIVISION.",
            " LOCAL-STORAGE SECTION.",
            " 01  W-GROUP-1.",
            "     05  W-FIELD-1 PIC X(03) VALUE \"FOO\".",
            "     05  FILLER    PIC X(01) VALUE \"-\".",
            "     05  W-FIELD-2 PIC X(03) VALUE \"XXX\".",
            " PROCEDURE DIVISION.",
            " 100-APPEND-TEXT.",
            "     MOVE \"ZZZ\" TO W-FIELD-2",
            "     DISPLAY W-GROUP-1 UPON CONSOLE.",
            "     .",
            " 200-NOT-UNDER-TEST.",
            "     DISPLAY \"Should not see this.\" UPON CONSOLE",
            "     ."
    };

    @BeforeAll
    public static void oneTimeSetup() {
        config.load("testconfig.properties");
        pathToTestCobolSources =
                config.getString("resources.directory")
              + FILE_SEPARATOR
              + GeneratorTestCodeInsertionIT.class.getPackageName().replace(".", FILE_SEPARATOR)
              + FILE_SEPARATOR
              + config.getString("application.source.directory")
              + FILE_SEPARATOR;

    }

    @BeforeEach
    public void commonSetup() {
        generator = new Generator(messages, tokenExtractor, config);
    }

    @Test
    public void given_main_program_with_working_storage_it_inserts_test_copybooks_in_the_right_places() throws Exception {
        Reader cobolSourceReader = new FileReader(testFile("IN1.CBL"));
        BufferedReader reader = new BufferedReader(cobolSourceReader);
        Writer mergedSourceWriter = new FileWriter(testFile("MERGEDSOURCE.CBL"));
        generator.mergeTestSuite(mockTestSuite, cobolSourceReader, mergedSourceWriter);
        cobolSourceReader.close();
        mergedSourceWriter.close();

        String expectedHashValue = MD5.MD5HashFile(testFileName("EX1.CBL"));
        String actualHashValue = MD5.MD5HashFile(testFileName("MERGEDSOURCE.CBL"));
        assertEquals(expectedHashValue, actualHashValue,
                "Comparing expected file <" + testFileName("EX1.CBL")
                       + "> and actual file <" + testFileName("MERGEDSOURCE.CBL") + ">");

    }

//    @Test
//    // We're asserting on computed hashes; this "test" gives us a readable version of the output file.
//    public void see_the_merged_source_file_on_stdout() throws IOException {
//        StringReader cobolSourceIn = makeCobolSourceProgram(cobolSourceWithoutWorkingStorage);
//        StringWriter testSourceOut = new StringWriter();
//        generator.mergeTestSuite(mockTestSuite, cobolSourceIn, testSourceOut);
//        System.out.println("testSourceOut: ");
//        System.out.println(testSourceOut.toString());
//    }


    private File testFile(String fileName) {
        File file = new File(pathToTestCobolSources + fileName);
        System.out.println("testFile full path: " + file.getAbsolutePath());
        return file;
    }

    private String testFileName(String fileName) {
        return pathToTestCobolSources + fileName;
    }

    private StringReader makeCobolSourceProgram(String[] sourceLines) {
        StringBuilder sourceCode = new StringBuilder();
        for (String sourceLine : sourceLines) {
            sourceCode.append("      ").append(String.format("%1$-74s", sourceLine)).append(NEWLINE);
        }
        return new StringReader(sourceCode.toString());
    }

}
