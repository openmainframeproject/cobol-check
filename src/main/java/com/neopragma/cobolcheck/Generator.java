package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.List;

/**
 * This class merges a Test Suite (a text file) with the source of the Cobol program to be tested,
 * producing a Cobol program with the unit test cases embedded in it.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Generator implements Constants {
    private final Messages messages;
    private final Log log;
    private final TokenExtractor tokenExtractor;

    private boolean inDataDivision = false;

    public Generator(
            Messages messages,
            Log log,
            TokenExtractor tokenExtractor) {
        this.messages = messages;
        this.log = log;
        this.tokenExtractor = tokenExtractor;
    }

    /**
     * Merge test code with the program under test to produce a Cobol source program
     * that can be compiled and executed to run the test suite.
     *
     * @param testSuite (Reader) Test cases
     * @param cobolSourceIn (Reader) Source of Cobol program under test
     * @param testSourceOut (Writer) Cobol source with test cases merged into program under test
     * @return (Writer) Same Writer object as passed in, populated with Cobol source lines
     */
    public Writer mergeTestSuite(
            Reader testSuite,
            Reader cobolSourceIn,
            Writer testSourceOut) {
        if (testSuite == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        if (cobolSourceIn == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        BufferedReader reader = new BufferedReader(cobolSourceIn);
        String sourceLine;
        boolean emptyInputStream = true;
        try {
            while ((sourceLine = reader.readLine()) != null) {
                emptyInputStream = false;
                sourceLine = fixedLength(sourceLine);
                List<String> tokens = tokenExtractor.extractTokensFrom(sourceLine);
                processingBeforeEchoingTheSourceLineToTheOutput(
                        tokens, sourceLine, cobolSourceIn, testSourceOut);
                testSourceOut.write(sourceLine);
                processingAfterEchoingTheSourceLineToTheOutput(
                        tokens, sourceLine, cobolSourceIn, testSourceOut);
            }
            reader.close();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
            catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
        if (emptyInputStream) {
            throw new PossibleInternalLogicErrorException("Generator.runSuite() empty input stream (cobolSourceIn");
        }
        return testSourceOut;
    }

    private void processingBeforeEchoingTheSourceLineToTheOutput(
            List<String> tokens,
            String sourceLine,
            Reader cobolSourceIn,
            Writer testSourceOut) throws IOException {
    }

    private void processingAfterEchoingTheSourceLineToTheOutput(
            List<String> tokens,
            String sourceLine,
            Reader cobolSourceIn,
            Writer testSourceOut) throws IOException {

        if (hasToken(tokens, "WORKING-STORAGE SECTION")) {
            testSourceOut.write(fixedLength(
                    "      ***** insert working-storage code here *****"));
        }

        if (hasToken(tokens, "PROCEDURE DIVISION")) {
            testSourceOut.write(fixedLength(
                    "      ***** insert procedure division code here *****"));
        }
    }

    private boolean hasToken(List<String> tokens, String tokenValue) {
        return tokens.size() > 0 && tokens.contains(tokenValue);
    }


    /**
     * Ensure the input line is fixed length 80 bytes plus a newline.
     *
     * @param sourceLine
     * @return 81-byte String: 80-byte Cobol source line followed by a newline character.
     */
    private String fixedLength(String sourceLine) {
        return String.format("%1$-80s", sourceLine) + NEWLINE;
    }

}
