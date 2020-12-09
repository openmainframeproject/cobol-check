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

    Writer mergeTestSuite(TestSuite testSuite, Reader cobolSourceIn, Writer testSourceOut) {
        if (testSuite == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        if (testSuite.getClass().getSimpleName().equals("EmptyTestSuite")) {
            Log.info("TestSuite is empty on entry to Generator.runSuite() method; nothing to do here.");
            return testSourceOut;
        }
        if (cobolSourceIn == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        BufferedReader reader = new BufferedReader(cobolSourceIn);
        String line;
        boolean emptyInputStream = true;
        try {
            while ((line = reader.readLine()) != null) {
                emptyInputStream = false;
                String sourceLine = String.format("%73s", line).substring(6).toUpperCase();


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


    /**
     * Merge test suite with Cobol source file and create Cobol test file
     *
     * @param testSuite - (TestSuite) the test cases
     * @param sourceFile - (Reader) the original Cobol source
     * @param testFile - (Writer) the output object for the merged Cobol test source
     * @return (Writer) the merged Cobol test source program
     */
    Writer runSuiteXXX(TestSuite testSuite, Reader sourceFile, Writer testFile) {
        Writer mergedSourceData = testFile;
        try {
            int data = sourceFile.read();
            while(data != Constants.END_OF_STREAM) {
                char dataChar = (char) data;
                testFile.write(data);
                data = sourceFile.read();
            }
            sourceFile.close();
            testFile.close();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
        catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
        return mergedSourceData;
    }
}
