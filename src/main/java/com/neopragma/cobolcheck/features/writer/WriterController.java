package com.neopragma.cobolcheck.features.writer;

import com.neopragma.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.exceptions.TestSuiteCouldNotBeReadException;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.StringHelper;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;

import java.io.*;
import java.util.List;

public class WriterController {

    private CobolWriter cobolWriter;
    private BufferedReader testSuiteReader;
    private final TestSuiteParser testSuiteParser;

    // The boilerplate copybooks for cobol-check test code inserted into Working-Storage and Procedure.
    // The names are a throwback to the proof-of-concept project, cobol-unit-test. Might change in future.
    private static final String workingStorageCopybookFilename = "CCHECKWS.CPY";
    private static final String procedureDivisionCopybookFilename = "CCHECKPD.CPY";

    private final String workingStorageHeader = ("       WORKING-STORAGE SECTION.");

    // Optionally replace identifier prefixes in cobol-check copybook lines and generated source lines,
    // in case of conflict with prefixes used in programs to be tested.
    // This is set in config.properties, cobolcheck.prefix entry.
    private String testCodePrefix;

    // Used to handle programs that don't have a Working-Storage Section
    private boolean workingStorageTestCodeHasBeenInserted = false;

    public WriterController(Writer testSourceWriter, Reader testSuiteReader){
        cobolWriter = new CobolWriter(testSourceWriter);
        this.testSuiteReader = new BufferedReader(testSuiteReader);
        testSuiteParser = new TestSuiteParser(new KeywordExtractor());
        testCodePrefix = Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX);
    }

    public void writeLine(String line) throws IOException { cobolWriter.writeLine(line);}

    public void writeCommentedLine(String line) throws IOException { cobolWriter.writeCommentedLine(line); }

    public void writeStatement(List<String> statement) throws IOException { cobolWriter.writeStatement(statement); }

    public void writeCommentedStatement(List<String> statement) throws IOException {
        cobolWriter.writeCommentedStatement(statement);
    }

    public void closeWriter(String programName) {
        try {
            cobolWriter.close();
        } catch (IOException closeTestSourceOutException) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR017", programName));
        }
    }

    public void closeTestSuiteReader(){
        try {
            testSuiteReader.close();
        } catch (IOException e){
            throw new TestSuiteCouldNotBeReadException(e);
        }
    }

    public boolean hasWorkingStorageTestCodeHasBeenInserted() {
        return workingStorageTestCodeHasBeenInserted;
    }

    public void insertWorkingStorageHeader() throws IOException {
        writeLine(workingStorageHeader);
    }

    /**
     * Insert test code into the Working-Storage Section of the test program being generated.
     *
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public void insertWorkingStorageTestCode(List<String> fileSectionStatements) throws IOException {
        // If this program had File Section source that we need to move to Working-Storage, inject the lines here.
        if (fileSectionStatements != null) {
            for (String line : fileSectionStatements) {
                writeLine(line);
            }
        }

        // Inject boilerplate test code from cobol-check Working-Storage copybook
        insertSecondarySourceIntoTestSource(workingStorageCopybookFilename);
        workingStorageTestCodeHasBeenInserted = true;
    }

    //TODO: Remove dependency on testSourceOut
    /**
     * Insert test code into the Procedure Division of the test program being generated.
     *
     * @param testSourceOut - writer attached to the test program being generated.
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public void insertProcedureDivisionTestCode(NumericFields numericFields) throws IOException {
        // Inject test initialization statement
        testSuiteParser.insertTestInitializationLineIntoTestSource(cobolWriter);

        // Parse the concatenated test suite and insert generated Cobol test statements
        testSuiteParser.parseTestSuite(testSuiteReader, cobolWriter, numericFields);

        // Inject boilerplate test code from cobol-check Procedure Division copybook
        insertSecondarySourceIntoTestSource(procedureDivisionCopybookFilename);

    }

    /**
     * Inject source statements from a secondary source (not the program under test) into the test program
     * being generated. Secondary sources are the cobol-check boilerplate copybooks, one for Working-Storage
     * and one for Procedure Division.
     *
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private void insertSecondarySourceIntoTestSource(String copybookFilename) throws IOException {
        InputStream is = this.getClass().getResourceAsStream(Constants.COBOLCHECK_COPYBOOK_DIRECTORY + copybookFilename);
        BufferedReader secondarySourceBufferedReader = new BufferedReader(new InputStreamReader(is));
        String secondarySourceLine = Constants.EMPTY_STRING;
        while ((secondarySourceLine = secondarySourceBufferedReader.readLine()) != null) {
            secondarySourceLine = secondarySourceLine
                    .replaceAll(Constants.TEST_CODE_PREFIX_PLACEHOLDER, testCodePrefix);
            writeLine(secondarySourceLine);
        }
        secondarySourceBufferedReader.close();
    }
}
