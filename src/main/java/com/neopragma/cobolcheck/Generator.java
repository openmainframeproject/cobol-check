package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    private final State state = new State();

    public static final String IDENTIFICATION_DIVISION = "IDENTIFICATION DIVISION";
    public static final String ENVIRONMENT_DIVISION = "ENVIRONMENT DIVISION";
    public static final String DATA_DIVISION = "DATA DIVISION";
    public static final String PROCEDURE_DIVISION = "PROCEDURE DIVISION";
    public static final String FILE_SECTION = "FILE SECTION";
    public static final String LOCAL_STORAGE_SECTION = "LOCAL-STORAGE SECTION";
    public static final String LINKAGE_SECTION = "LINKAGE SECTION";
    public static final String WORKING_STORAGE_SECTION = "WORKING-STORAGE SECTION";

    private boolean workingStorageTestCodeHasBeenInserted = false;
    private String workingStorageHeader = fixedLength("       WORKING-STORAGE SECTION.");

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
            Writer testSourceOut) throws IOException {
        if (testSuite == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        if (cobolSourceIn == null) {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR001", "cobolSourceIn", "Generator.runSuite()"));
        }
        BufferedReader cobolSourceInReader
                = new BufferedReader(cobolSourceIn);
        String sourceLine;
        boolean emptyInputStream = true;
        try {
            while ((sourceLine = cobolSourceInReader.readLine()) != null) {
                emptyInputStream = false;
                sourceLine = fixedLength(sourceLine);
                List<String> tokens = tokenExtractor.extractTokensFrom(sourceLine);
                processingBeforeEchoingTheSourceLineToTheOutput(
                        tokens, sourceLine, cobolSourceInReader, testSourceOut);
                testSourceOut.write(sourceLine);
                processingAfterEchoingTheSourceLineToTheOutput(
                        tokens, sourceLine, cobolSourceInReader, testSourceOut);
            }
            cobolSourceInReader.close();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
            catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
        if (emptyInputStream) {
            throw new PossibleInternalLogicErrorException(messages.get("ERR007"));
        }
        return testSourceOut;
    }

    private void processingBeforeEchoingTheSourceLineToTheOutput(
            List<String> tokens,
            String sourceLine,
            Reader reader,
            Writer testSourceOut) throws IOException {

        if (sourceLineContains(tokens, DATA_DIVISION)) enteringDataDivision();

        if (sourceLineContains(tokens, PROCEDURE_DIVISION)) {
            enteringProcedureDivision();
            if (!workingStorageTestCodeHasBeenInserted) {
                testSourceOut.write(workingStorageHeader);
                insertWorkingStorageTestCode(testSourceOut);
            }
        }
        if (sourceLineContains(tokens, WORKING_STORAGE_SECTION)) enteringWorkingStorageSection();
    }

    private void processingAfterEchoingTheSourceLineToTheOutput(
            List<String> tokens,
            String sourceLine,
            Reader reader,
            Writer testSourceOut) throws IOException {

        if (sourceLineContains(tokens, WORKING_STORAGE_SECTION)) {
            insertWorkingStorageTestCode(testSourceOut);
        }

        if (sourceLineContains(tokens, PROCEDURE_DIVISION)) {
            insertProcedureDivisionTestCode(testSourceOut);
        }
    }

    private void insertWorkingStorageTestCode(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(
                "      ***** insert working-storage code here *****"));
        workingStorageTestCodeHasBeenInserted = true;
    }

    private void insertProcedureDivisionTestCode(Writer testSourceOut) throws IOException {
        testSourceOut.write(fixedLength(
                "      ***** insert procedure division code here *****"));
    }

    private boolean sourceLineContains(List<String> tokens, String tokenValue) {
        return tokens.size() > 0 && tokens.contains(tokenValue);
    }

    private void enteringDataDivision() {
        state.flags.get(DATA_DIVISION).set();
    }

    private void enteringProcedureDivision() {
        state.flags.get(PROCEDURE_DIVISION).set();
    }

    private void enteringWorkingStorageSection() {
        state.flags.get(WORKING_STORAGE_SECTION).set();
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


    class State {
        private Map<String, Flag> flags;

        public State() {
            flags = new HashMap<>();
            flags.put(FILE_SECTION, new Flag());
            flags.put(LINKAGE_SECTION, new Flag());
            flags.put(LOCAL_STORAGE_SECTION, new Flag());
            flags.put(WORKING_STORAGE_SECTION, new Flag());
            mutuallyExclusiveFlagsFor(FILE_SECTION,
                    LINKAGE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);
            mutuallyExclusiveFlagsFor(LINKAGE_SECTION,
                    FILE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);
            mutuallyExclusiveFlagsFor(LOCAL_STORAGE_SECTION,
                    LINKAGE_SECTION, FILE_SECTION, WORKING_STORAGE_SECTION);
            mutuallyExclusiveFlagsFor(WORKING_STORAGE_SECTION,
                    LINKAGE_SECTION, LOCAL_STORAGE_SECTION, FILE_SECTION);

            flags.put(IDENTIFICATION_DIVISION, new Flag());

            flags.put(ENVIRONMENT_DIVISION, new Flag());

            flags.put(DATA_DIVISION, new Flag());
            dependentFlagsFor(DATA_DIVISION,
                    FILE_SECTION, LINKAGE_SECTION, LOCAL_STORAGE_SECTION, WORKING_STORAGE_SECTION);

            flags.put(PROCEDURE_DIVISION, new Flag());

            mutuallyExclusiveFlagsFor(IDENTIFICATION_DIVISION,
                    DATA_DIVISION, ENVIRONMENT_DIVISION, PROCEDURE_DIVISION);
            mutuallyExclusiveFlagsFor(ENVIRONMENT_DIVISION,
                    IDENTIFICATION_DIVISION, DATA_DIVISION, PROCEDURE_DIVISION);
            mutuallyExclusiveFlagsFor(DATA_DIVISION,
                    IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, PROCEDURE_DIVISION);
            mutuallyExclusiveFlagsFor(PROCEDURE_DIVISION,
                    IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, DATA_DIVISION);


        }

        private void mutuallyExclusiveFlagsFor(String token, String... mutuallyExclusiveTokens) {
            List<Flag> mutuallyExclusiveFlags = new ArrayList<>();
            for (String mutuallyExclusiveToken : mutuallyExclusiveTokens) {
                mutuallyExclusiveFlags.add(flags.get(mutuallyExclusiveToken));
            }
            flags.get(token).setMutuallyExclusiveFlags(mutuallyExclusiveFlags);
        }
        private void dependentFlagsFor(String token, String... dependentTokens) {
            List<Flag> dependentFlags = new ArrayList<>();
            for (String dependentToken : dependentTokens) {
                dependentFlags.add(flags.get(dependentToken));
            }
            flags.get(token).setDependentFlags(dependentFlags);
        }
    }

    class Flag {
        private boolean state = false;
        private List<Flag> mutuallyExclusiveFlags;
        private List<Flag> dependentFlags;
        public Flag() {
            this.mutuallyExclusiveFlags = new ArrayList<>();
            this.dependentFlags = new ArrayList<>();
        }
        public void setMutuallyExclusiveFlags(List<Flag> mutuallyExclusiveFlags) {
            this.mutuallyExclusiveFlags = mutuallyExclusiveFlags;
        }
        public void setDependentFlags(List<Flag> dependentFlags) {
            this.dependentFlags = dependentFlags;
        }
        public boolean isSet() {
            return state;
        }
        public void set() {
            state = true;
            for (Flag flag : mutuallyExclusiveFlags) {
                flag.unset();
            }
        }
        public void unset() {
            state = false ;
            for (Flag flag : dependentFlags) {
                flag.unset();
            }
        }
    }

}
