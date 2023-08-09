package org.openmainframeproject.cobolcheck.features.testSuiteParser;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.exceptions.TestSuiteCouldNotBeReadException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.cobolLogic.NumericFields;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class TestSuiteParserController {

    private final TestSuiteParser testSuiteParser;
    TestSuiteConcatenator testSuiteConcatenator;
    private MockRepository mockRepository;
    private BeforeAfterRepo beforeAfterRepo;
    private MockGenerator mockGenerator;
    private BufferedReader testSuiteReader;
    private WhenOtherGenerator whenOtherGenerator;

    private TestSuiteErrorLog testSuiteErrorLog;

    //We parse the test suite early, in order to generate mocks.
    //Thus, we save the lines we get from parsing, to use them later.
    private List<String> parsedTestSuiteLines;

    // The boilerplate copybooks for cobol-check test code inserted into Working-Storage and Procedure.
    // The names are a throwback to the proof-of-concept project, cobol-unit-test. Might change in future.
    private static final String workingStorageCopybookFilename = "CCHECKWS.CPY";
    private static final String procedureDivisionResultCopybookFilename = "CCHECKRESULTPD.CPY";
    private static final String procedureDivisionParagraphCopybookFilename = "CCHECKPARAGRAPHSPD.CPY";

    private static final String[] copyBookTokensWithPeriodAsDecimalPoint = new String[]{"ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9"};

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
        beforeAfterRepo = new BeforeAfterRepo();
        testSuiteErrorLog = new TestSuiteErrorLog();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository, beforeAfterRepo, testSuiteErrorLog);
        mockGenerator = new MockGenerator();
        testCodePrefix = Config.getTestCodePrefix();
        whenOtherGenerator = new WhenOtherGenerator();
    }

    //Used for testing only
    public TestSuiteParserController(BufferedReader reader) {
        testSuiteReader = reader;
        mockRepository = new MockRepository();
        beforeAfterRepo = new BeforeAfterRepo();
        testSuiteErrorLog = new TestSuiteErrorLog();
        testSuiteParser = new TestSuiteParser(new KeywordExtractor(), mockRepository, beforeAfterRepo, testSuiteErrorLog);
        mockGenerator = new MockGenerator();
        testCodePrefix = Config.getString(Constants.COBOLCHECK_PREFIX_CONFIG_KEY, Constants.DEFAULT_COBOLCHECK_PREFIX);
    }

    public boolean hasWorkingStorageTestCodeBeenInserted() {
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

        CobolGenerator.addStartAndEndTags(lines);

        workingStorageTestCodeHasBeenInserted = true;

        return lines;
    }

    /**
     * Gets the Working-Storage part of the generated code for mocks
     *
     * @return A list of the lines generated
     */
    public List<String> getWorkingStorageMockCode() {
        List<String> lines = new ArrayList<>();
        //Generates the variables used for counting
        lines.addAll(generateMockCountingFields());
        CobolGenerator.addStartAndEndTags(lines);
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

        // Inject boilerplate test code from cobol-check Result Procedure Division copybook
        lines.addAll(getBoilerplateCodeFromCopybooks(procedureDivisionResultCopybookFilename));

        //Paragraphs generated in between boilerplate code
        lines.addAll(generateBeforeParagraph());
        lines.add("");
        lines.addAll(generateAfterParagraph());
        lines.add("");
        lines.addAll(generateBeforeAfterBranchParagraphs(true));
        lines.addAll(generateMockCountInitializer());
        lines.add("");
        lines.addAll(generateMockSections(true));

        // Inject boilerplate test code from cobol-check Paragraph Procedure Division copybook
        lines.addAll(getBoilerplateCodeFromCopybooks(procedureDivisionParagraphCopybookFilename));

        CobolGenerator.addStartAndEndTags(lines);
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
        return mockGenerator.generateMockParagraphs(mockRepository.getMocks(), withComments);
    }

    /**Generates the lines for BEFORE-EACH paragraph,
     * @return The generated lines
     */
    public List<String> generateBeforeParagraph(){
        return beforeAfterRepo.getBeforeEachParagraphLines();
    }

    /**Generates the lines for AFTER-EACH paragraph,
     * @return The generated lines
     */
    public List<String> generateAfterParagraph(){
        return beforeAfterRepo.getAfterEachParagraphLines();
    }

    /**Generates the lines for the paragraphs that branches out before/after,
     * @param withComments determines if a comment should be included in each paragraph
     * @return The generated lines
     */
    public List<String> generateBeforeAfterBranchParagraphs(boolean withComments){
        return beforeAfterRepo.getAllBranchingParagraphs(withComments);
    }

    public boolean mockExistsFor(String identifier, String type, List<String> arguments){
        return mockRepository.mockExistsFor(identifier, type, arguments);
    }

    /**Generates the lines for 'Evaluate when' to perform the correct generated SECTION
     * for a specific identifier.
     * @param identifier - The identifier of the SECTION, PARAGRAPH etc. that is mocked.
     * @return The generated lines
     */
    public List<String> generateMockPerformCalls(String identifier, String type, List<String> arguments){
        List<String> lines = mockGenerator.generateMockPerformCalls(identifier, type, arguments, mockRepository.getMocks());
        CobolGenerator.addStartAndEndTags(lines);
        return lines;
    }
    /**This line should be inserted at the end of a mocked component,
     * to end the EVALUATE started in the beginning of the mock.
     * Becomes three lines, if start- and end-tags are inserted.
     * @return END-EVALUATE line
     */
    public List<String> getEndEvaluateLine(){
        List<String> lines = new ArrayList<>();
        lines.add(mockGenerator.getEndEvaluateLine());
        CobolGenerator.addStartAndEndTags(lines);
        return lines;
    }

    public List<String> getContinueLine(){
        List<String> lines = new ArrayList<>();
        lines.add(mockGenerator.getContinueLine());
        CobolGenerator.addStartAndEndTags(lines);
        return lines;
    }

    public void logUnusedMocks(){
        testSuiteErrorLog.logUnusedMocks(mockRepository.getMocks());
    }

    /**
     * Gets the lines from the cobol-check boilerplate copybooks, one for Working-Storage
     * and one for Procedure Division.
     *
     * @param copybookFilename The name of the copybook to get lines from
     * @return A list of the gathered lines
     * @throws IOException - pass any IOExceptions to the caller.
     */
    public List<String> getBoilerplateCodeFromCopybooks(String copybookFilename) throws IOException {
        List<String> lines = new ArrayList<>();
        boolean isComma = Config.isDecimalPointComma();
        String path = Constants.COBOLCHECK_COPYBOOK_DIRECTORY + copybookFilename;
        InputStream is = this.getClass().getResourceAsStream(path);
        BufferedReader secondarySourceBufferedReader = new BufferedReader(new InputStreamReader(is));
        String secondarySourceLine = Constants.EMPTY_STRING;
        while ((secondarySourceLine = secondarySourceBufferedReader.readLine()) != null) {
            secondarySourceLine = secondarySourceLine
                    .replaceAll(Constants.TEST_CODE_PREFIX_PLACEHOLDER, testCodePrefix);
            if (Config.isDecimalPointComma()){
                for (String token : copyBookTokensWithPeriodAsDecimalPoint){
                    if (secondarySourceLine.toUpperCase(Locale.ROOT).contains(token.toUpperCase(Locale.ROOT))){
                        String replacement = StringHelper.swapChars(token, ',', '.');
                        secondarySourceLine = secondarySourceLine.replaceAll(token, replacement);
                    }

                }
            }
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

    public void prepareNextParse() {
        Config.setDecimalPointIsCommaFromFile();
    }

    public List<String> generateWhenOtherBlock(String type, List<String> blocklines, String sourceLine, String identifier, boolean withComments)  throws IOException{
        List<String> lines = new ArrayList<>();
        WhenOther whenOther = testSuiteParser.getWhenOtherBlock(type, blocklines, identifier, true);
        lines.add(whenOtherGenerator.generateWhenOtherCall(whenOther));
        lines.addAll(this.getEndEvaluateLine());
        lines.add(sourceLine);
        lines.add("");
        lines.addAll(whenOtherGenerator.generateWhenOther(whenOther, withComments));
        return lines;
    }
    

}
