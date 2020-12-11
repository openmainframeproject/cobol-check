package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * This class merges a Test Suite (a text file) with the
 * source of the Cobol program to be tested, producing a
 * Cobol program with the unit test cases embedded in it.
 * Running the test suite amounts to executing this
 * generated program.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Generator {

    private static final String isNullMessage = "%s is null on entry to Generator.runSuite() method.";
    private Messages messages;

    public Generator(Messages messages) {
        this.messages = messages;
    }

    Writer mergeTestSuite(Reader testSuite, Reader cobolSourceIn, Writer testSourceOut) {
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


                testSourceOut.write(sourceLine + Constants.NEWLINE);
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

}
