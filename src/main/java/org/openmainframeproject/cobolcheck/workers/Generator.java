package org.openmainframeproject.cobolcheck.workers;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.List;

import org.openmainframeproject.cobolcheck.exceptions.CobolSourceCouldNotBeReadException;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.features.interpreter.InterpreterController;
import org.openmainframeproject.cobolcheck.features.prepareMerge.PrepareMergeController;
import org.openmainframeproject.cobolcheck.features.testSuiteParser.TestSuiteParserController;
import org.openmainframeproject.cobolcheck.features.writer.WriterController;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.RunInfo;
import org.openmainframeproject.cobolcheck.services.log.Log;

/**
 * This class merges a Test Suite (a text file) with the source of the Cobol program to be tested,
 * producing a Cobol program with the unit test cases embedded in it.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Generator {

    private InterpreterController interpreter;
    private WriterController writerController;
    private TestSuiteParserController testSuiteParserController;
    private boolean workingStorageHasEnded;

    List<String> matchingTestDirectories;
    
    private String currentIdentifier;
    private  String currentMockType;

    public Generator() { }

    /**
     * For testing only
     */
    public Generator(InterpreterController interpreter, WriterController writerController,
                     TestSuiteParserController testSuiteParserController) {
        this.interpreter = interpreter;
        this.writerController = writerController;
        this.testSuiteParserController = testSuiteParserController;
        this.currentMockType=null;
        mergeTestSuite();
        
    }



    /**
     * Finds any test-directory that matches the given program name. For each of these directories, all test
     * files are concatenated into one and the source program are merged with all the test-suites and cobol-
     * check's boilerplate copybooks into one test-program.
     * @param programName The name of the source program.
     * @param testFileNames The names of every test for the source program, separated by a space.
     */
    public void prepareAndRunMerge(String programName, String testFileNames) {
        RunInfo.setCurrentProgramName(new File(programName).getName());
        RunInfo.setCurrentProgramPath(new File(programName).getAbsolutePath());
        matchingTestDirectories = PrepareMergeController.getMatchingTestDirectoriesForProgram(programName);
        for (String matchingDirectory : matchingTestDirectories) {

            Reader sourceReader = PrepareMergeController.getSourceReader(programName);
            interpreter = new InterpreterController(new BufferedReader(sourceReader));

            testSuiteParserController = new TestSuiteParserController(testFileNames);
            testSuiteParserController.concatenateTestSuites(matchingDirectory);

            Writer testSourceWriter = PrepareMergeController.getTestSourceWriter(programName);
            writerController = new WriterController(testSourceWriter);

            String testSourceOutPath = PrepareMergeController.getTestSourceOutPath();
            Log.debug("Initializer.runTestSuites() testSourceOutPath: <" + testSourceOutPath + ">");

            workingStorageHasEnded = false;

            mergeTestSuite();
            Log.info(Messages.get("INF012", programName));

            closeReadersAndWriters(programName);
        }
    }

    /**
     * Handles the merge of the source program, the test-suites and cobol-check's boilerplate copybooks.
     * This is done by reading the source file line by line, and writing the appropriate lines to the
     * output file. Each line is interpreted, in order to know if, it should be commented out, ignored
     * or if the test-suite or boilerplate code should be inserted.
     */
    private void mergeTestSuite() {
        String sourceLine;
        try {
        while ((sourceLine = interpreter.interpretNextLine()) != null) {
                processingBeforeEchoingSourceLineToOutput();
                echoingSourceLineToOutput(sourceLine);
                processingAfterEchoingSourceLineToOutput();
            }
            testSuiteParserController.logUnusedMocks();
            testSuiteParserController.prepareNextParse();
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
        catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
    }

    /**
     * processingBeforeEchoingSourceLineToOutput
     * Perform appropriate processing of the current input line from the program under test prior to echoing that
     * line to the test program (that is, the copy of the program under test that has test code injected into it).
     *
     * @throws IOException - pass any IOExceptions up to the caller
     */
    private void processingBeforeEchoingSourceLineToOutput() throws IOException {

        if (!workingStorageHasEnded && interpreter.isCurrentLineEndingWorkingStorageSection()) {
            if (!testSuiteParserController.hasWorkingStorageTestCodeBeenInserted()) {
                writerController.writeLine(testSuiteParserController.getWorkingStorageHeader());
                writerController.writeLines(testSuiteParserController.getWorkingStorageTestCode(
                        interpreter.getFileSectionStatements()));
            }
            writerController.startStoringLines();
            workingStorageHasEnded = true;
        }
        if (interpreter.didLineJustEnter(Constants.PROCEDURE_DIVISION) && interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)){
            writerController.stopStoringLines();
            testSuiteParserController.parseTestSuites(interpreter.getNumericFields());
            writerController.writeLines(testSuiteParserController.getWorkingStorageMockCode());
            writerController.releaseStoredLines();
        }
    }

    private String tryInsertEndEvaluateAtMockedCompomentEnd(String sourceLine) throws IOException {
        if (interpreter.isInsideSectionOrParagraphMockBody()){
            if (interpreter.isCurrentLineEndingSectionOrParagraph()){
                if (interpreter.canWriteEndEvaluateBeforeCurrentLine()){
                    writeWhenOtherMockedSection(sourceLine);
                    interpreter.removeSectionLines();
                    interpreter.setInsideSectionOrParagraphMockBody(false);
                    return "";
                }
                else
                    return sourceLine.replace(".", "");
            }
        }
        return sourceLine;
    }

    /**
     * Writes the given line to the output file. If a multiline statement has been read, this will
     * be written instead (this statement will include the line). Comments out lines and statements
     * when needed.
     *
     * @param sourceLine - The line to write
     */
    private void writeToSource(String sourceLine) throws IOException {
        if (interpreter.shouldCurrentLineBeParsed()) {
            if (interpreter.hasStatementBeenRead()){
                if (interpreter.shouldCurrentStatementBeStubbed()){
                    writerController.writeStubbedLines(interpreter.getCurrentStatement());
                }
                else {
                    writerController.writeLines(interpreter.getCurrentStatement());
                }
            }

            else {
                if (interpreter.shouldCurrentLineBeStubbed()){
                    writerController.writeStubbedLine(sourceLine);
                }
                else {
                    writerController.writeLine(sourceLine);
                }
            }
        }
    }

    /**
     * Perform appropriate processing after echoing (or skipping) the current source line from the program under test.
     *
     * @throws IOException - pass any IOExceptions to the caller
     */
    private void processingAfterEchoingSourceLineToOutput() throws IOException {

        if (interpreter.currentLineContains(Constants.WORKING_STORAGE_SECTION)) {
            writerController.writeLines(testSuiteParserController.getWorkingStorageTestCode(
                    interpreter.getFileSectionStatements()));
        }

        if (interpreter.currentLineContains(Constants.PROCEDURE_DIVISION)) {
            writerController.writeLines(testSuiteParserController.getProcedureDivisionTestCode());
        }

        if (interpreter.isCurrentComponentMockable()){
            String identifier = interpreter.getPossibleMockIdentifier();
            String type = interpreter.getPossibleMockType();
            List<String> arguments = interpreter.getPossibleMockArgs();
            if (testSuiteParserController.mockExistsFor(identifier, type, arguments)){
                if(interpreter.isInsideSectionOrParagraphMockBody()){
                    interpreter.addMockedSectionLines(testSuiteParserController.generateMockPerformCalls(identifier, type, arguments));
                    if (type.equals(Constants.CALL_TOKEN)){
                        interpreter.addMockedSectionLine("            CONTINUE");
                }
                }else writerController.writeLines(testSuiteParserController.generateMockPerformCalls(identifier, type, arguments));
                if (type.equals(Constants.SECTION_TOKEN) || type.equals(Constants.PARAGRAPH_TOKEN)){
                    this.currentIdentifier = identifier;
                    this.currentMockType=interpreter.getPossibleMockType();
                    interpreter.setInsideSectionOrParagraphMockBody(true);
                }
            }
        }
    }

    private void closeReadersAndWriters(String programName) {
        interpreter.closeReader();
        testSuiteParserController.closeTestSuiteReader();
        writerController.closeWriter(programName);
    }

    private void writeWhenOtherMockedSection(String sourceLine)  throws IOException{
        writerController.writeLine(String.format("               PERFORM %s-WHEN-OTHER", currentIdentifier));
        writerController.writeLines(testSuiteParserController.getEndEvaluateLine());
        writerController.writeLine(sourceLine);
        writerController.writeLine("");;
        if(currentMockType.equals(Constants.SECTION_TOKEN)) 
            writerController.writeLine(String.format("       %s-WHEN-OTHER SECTION.", currentIdentifier));
        else writerController.writeLine(String.format("       %s-WHEN-OTHER.", currentIdentifier));
        writerController.writeLines(interpreter.getSectionLines());
    }

    private void echoingSourceLineToOutput(String sourceLine){
        try{
            if(interpreter.isInsideSectionOrParagraphMockBody()){
                interpreter.addMockedSectionLine();
            }
            sourceLine = tryInsertEndEvaluateAtMockedCompomentEnd(sourceLine);
            if(!interpreter.isInsideSectionOrParagraphMockBody()){
                writeToSource(sourceLine);
            }
        } catch (IOException ioEx) {
            throw new CobolSourceCouldNotBeReadException(ioEx);
        }
        catch (Exception ex) {
            throw new PossibleInternalLogicErrorException(ex);
        }
    }
}
