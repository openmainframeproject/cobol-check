package com.neopragma.cobolcheck.features.testSuiteParser;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.exceptions.TestSuiteCouldNotBeReadException;
import com.neopragma.cobolcheck.services.Config;
import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.cobolLogic.NumericFields;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class TestSuiteParserController {

    private final TestSuiteParser testSuiteParser;
    TestSuiteConcatenator testSuiteConcatenator;
    private MockRepository mockRepository;
    private MockGenerator mockGenerator;
    private BufferedReader testSuiteReader;

    //We parse the test suite early, in order to generate mocks.
    //Thus, we save the lines we get from parsing, to use them later.
    private List<String> parsedTestSuiteLines;

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

    public TestSuiteParserController(String testFileNames) {
        testSuiteConcatenator = new TestSuiteConcatenator(testFileNames);
        mockRepository = new MockRepository();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository);
        mockGenerator = new MockGenerator();
        testCodePrefix = Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX);
    }

    //Used for testing only
    public TestSuiteParserController(BufferedReader reader) {
        testSuiteReader = reader;
        mockRepository = new MockRepository();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository);
        mockGenerator = new MockGenerator();
        testCodePrefix = Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX);
    }

    public boolean hasWorkingStorageTestCodeHasBeenInserted() {
        return workingStorageTestCodeHasBeenInserted;
    }

    public String getWorkingStorageHeader() {
        return workingStorageHeader;
    }

    /**
     * Each testsuite might be in its own file. This method combines all the testsuites, pertaining to
     * the source program, and combines them into one file, so that only one reader is needed to read
     * all tests.
     *
     * @param programTestSuiteSubdirectory directory where the test files can be found
     */
    public void concatenateTestSuites(String programTestSuiteSubdirectory) {
        Reader testSuiteReader = testSuiteConcatenator.concatenateTestSuites(programTestSuiteSubdirectory);
        if (testSuiteReader == null) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        this.testSuiteReader = new BufferedReader(testSuiteReader);
    }

    /**
     * Reads through the concatenated testsuites. While reading, test statements, Mocks and Verifiers
     * will be generated for later use.
     *
     * @param numericFields numeric field values gathered from Interpreter
     */
    public void parseTestSuites(NumericFields numericFields){
        parsedTestSuiteLines = testSuiteParser.getParsedTestSuiteLines(testSuiteReader, numericFields);
    }


    /**
     * Gets the Working-Storage Section test code to be inserted into the program being generated.
     *
     * @param fileSectionStatements list of file section statements
     * @return A list of the lines generated
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public List<String> getWorkingStorageTestCode(List<String> fileSectionStatements) throws IOException {
        List<String> lines = new ArrayList<>();
        // If this program had File Section source that we need to move to Working-Storage, inject the lines here.
        if (fileSectionStatements != null) {
            lines.addAll(fileSectionStatements);
        }

        // Inject boilerplate test code from cobol-check Working-Storage copybook
        lines.addAll(getBoilerplateCodeFromCopybooks(workingStorageCopybookFilename));

        //Generates the variables used for counting
        lines.addAll(generateMockCountingFields());

        workingStorageTestCodeHasBeenInserted = true;

        return lines;
    }

    /**Generates the lines for keeping track of mock counting
     * For each mock a variable are created for:
     * - Current count
     * - Expected count
     * - Mock operation (string showing type and identity)
     * @return The generated lines
     */
    public List<String> generateMockCountingFields(){
        return mockGenerator.generateWorkingStorageMockCountLines(mockRepository.getMocks());
    }

    /**
     * Gets the PROCEDURE DIVISION test code to be inserted into the program being generated.
     *
     * @return A list of the lines generated
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public List<String> getProcedureDivisionTestCode() throws IOException {
        List<String> lines = new ArrayList<>();
        // Inject test initialization statement
        lines.add(testSuiteParser.getTestInitializationLine());


        if (testSuiteReader == null){
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR001", "testSuite", "Generator.runSuite()"));
        }
        // Insert generated Cobol test statements, from the testsuite parse
        lines.addAll(parsedTestSuiteLines);

        // Inject boilerplate test code from cobol-check Procedure Division copybook
        lines.addAll(getBoilerplateCodeFromCopybooks(procedureDivisionCopybookFilename));

        lines.addAll(generateMockCountInitializer());

        lines.addAll(generateMockSections(true));
        return lines;
    }

    /**Generates the lines for the initializer section.
     * This resets the current count and expected count of all global mocks in cobol.
     * @return The generated lines
     */
    public List<String> generateMockCountInitializer(){
        return mockGenerator.generateMockCountInitializer(mockRepository.getMocks());
    }

    /**Generates the lines for SECTIONs based on mocks,
     * for each mock in a given list.
     * @return The generated lines
     */
    public List<String> generateMockSections(boolean withComments){
        return mockGenerator.generateMockSections(mockRepository.getMocks(), withComments);
    }

    public boolean mockExistsFor(String identifier){
        return mockRepository.mockExistsFor(identifier);
    }

    /**Generates the lines for 'Evaluate when' to perform the correct generated SECTION
     * for a specific identifier.
     * @param identifier - The identifier of the SECTION, PARAGRAPH etc. that is mocked.
     * @return The generated lines
     */
    public List<String> generateMockPerformCalls(String identifier){
        return mockGenerator.generateMockPerformCalls(identifier, mockRepository.getMocks());
    }
    /**This line should be inserted at the end of a mocked component,
     * to end the EVALUATE started in the beginning of the mock.
     * @return END-EVALUATE line
     */
    public String getEndEvaluateLine(){
        return mockGenerator.getEndEvaluateLine();
    }

    /**
     * Gets the lines from the cobol-check boilerplate copybooks, one for Working-Storage
     * and one for Procedure Division.
     *
     * @param copybookFilename The name of the copybook to get lines from
     * @return A list of the gathered lines
     * @throws IOException - pass any IOExceptions to the caller.
     */
    private List<String> getBoilerplateCodeFromCopybooks(String copybookFilename) throws IOException {
        List<String> lines = new ArrayList<>();
        String path = Constants.COBOLCHECK_COPYBOOK_DIRECTORY + copybookFilename;
        InputStream is = this.getClass().getResourceAsStream(path);
        BufferedReader secondarySourceBufferedReader = new BufferedReader(new InputStreamReader(is));
        String secondarySourceLine = Constants.EMPTY_STRING;
        while ((secondarySourceLine = secondarySourceBufferedReader.readLine()) != null) {
            secondarySourceLine = secondarySourceLine
                    .replaceAll(Constants.TEST_CODE_PREFIX_PLACEHOLDER, testCodePrefix);
            lines.add(secondarySourceLine);
        }
        secondarySourceBufferedReader.close();
        return lines;
    }

    public void closeTestSuiteReader(){
        try {
            testSuiteReader.close();
        } catch (IOException e){
            throw new TestSuiteCouldNotBeReadException(e);
        }
    }
}
