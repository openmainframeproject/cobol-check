package org.openmainframeproject.cobolcheck.features.interpreter;

import org.openmainframeproject.cobolcheck.exceptions.CopybookCouldNotBeExpanded;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringTuple;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LineRepository {

    //Used for setting status in 'fileIdentifiersAndStatuses'
    private String currentExpectFileIdentifier;

    // All lines from original Environment Division / Input-Output Section / File Control
    private List<String> fileControlStatements;

    // All lines from original Data Division / File Section
    private List<String> fileSectionStatements;

    // Internal file identifiers and status field names
    private final Map<String, String> fileIdentifiersAndStatuses;

    // Tokens collected from COPY statements that may span multiple lines
    private List<String> copyTokens;

    LineRepository() {
        fileSectionStatements = new ArrayList<>();
        this.fileIdentifiersAndStatuses = new HashMap<>();
    }

    List<String> getFileControlStatements() {
        return fileControlStatements;
    }

    List<String> getFileSectionStatements() {
        return fileSectionStatements;
    }

    Map<String, String> getFileIdentifiersAndStatuses() {
        return fileIdentifiersAndStatuses;
    }

    List<String> getCopyTokens() {
        return copyTokens;
    }

    void addFileControlStatement(String statement){
        if (fileControlStatements == null){
            fileControlStatements = new ArrayList<>();
        }
        fileControlStatements.add(statement);
    }

    void addFileSectionStatement(String statement){
        if (fileSectionStatements == null){
            fileSectionStatements = new ArrayList<>();
        }
        fileSectionStatements.add(statement);
    }

    void addFileIdentifierWithNoStatus(String identifier){
        fileIdentifiersAndStatuses.put(identifier, Constants.EMPTY_STRING);
        currentExpectFileIdentifier = identifier;
    }
    void addStatusForLastSetIdentifier(String status){
        fileIdentifiersAndStatuses.put(currentExpectFileIdentifier, status);
    }

    void addAccumulatedTokensFromCopyStatementToCopyTokens(String line) {
        // warn on the number og tokens collected
        Log.warn("addAccumulatedTokensFromCopyStatementToCopyTokens: " + line);
        if (copyTokens == null) {
            copyTokens = new ArrayList<>();
        }
        // If this is the start of a new COPY statement (line contains "COPY"), clear previous tokens
        String trimmedLine = line.trim();
        if (trimmedLine.toUpperCase().startsWith("COPY") || trimmedLine.toUpperCase().startsWith("INCLUDE")) {
            copyTokens.clear();
        }
        String[] lineTokens = line.trim().split(Constants.SPACE);
        for (String lineToken : lineTokens) {
            if (lineToken != null && !lineToken.equals(Constants.EMPTY_STRING)) {
                copyTokens.add(lineToken);
            }
        }
    }

    List<String> addExpandedCopyStatementsToFileSectionStatements() {
        // warn on the number og tokens collected
        Log.warn("addExpandedCopyStatementsToFileSectionStatements: " + copyTokens.size() + " tokens collected");

        for (int i = 0 ; i < copyTokens.size() ; i++) {
            if (copyTokens.get(i).equals(Constants.EMPTY_STRING)) {
                copyTokens.remove(i);
            }
        }
        if (copyTokens.isEmpty()
                || !copyTokens.get(0).equalsIgnoreCase(Constants.COPY_TOKEN)
                || copyTokens.size() < 2) {
            throw new PossibleInternalLogicErrorException(Messages.get("ERR024"));
        }

        // 2nd entry is the name of the copybook. The value might end with a period.
        String copybookName = copyTokens.get(1).replace(Constants.PERIOD, Constants.EMPTY_STRING);

        // 3rd entry might be the word "REPLACING" followed by "x" "BY" "y"
        StringTuple replacingValues = new StringTuple(null, null);
        if (copyTokens.size() > 4) {
            if (copyTokens.get(2).equalsIgnoreCase(Constants.REPLACING_KEYWORD)
                    || copyTokens.get(4).equalsIgnoreCase(Constants.BY_KEYWORD)) {
                replacingValues = new StringTuple(copyTokens.get(3), copyTokens.get(5));
            }
        }

        List<String> copyLines = new ArrayList<>();
        CopybookExpander copybookExpander = new CopybookExpander();
        try {
            copyLines = copybookExpander.expand(copyLines, copybookName, replacingValues);
        } catch (IOException ioException) {
            throw new PossibleInternalLogicErrorException("addExpandedCopyStatementsToFileSectionStatements",ioException);
        }
        fileSectionStatements.addAll(copyLines);
        return copyLines;
    }

    /**
     * Expands an 'EXEC SQL' statement into multiple lines
     * by locating and reading the INCLUDED copybook.
     * @param line
     * @return
     * @throws IOException
     */
    List<String> getExpandedCopyDB2Statements(String line) throws IOException {
        List<String> copyLines;
        CopybookExpander copybookExpander = new CopybookExpander();

        try {
            copyLines = copybookExpander.getIncludedLines(line);
        } catch (IOException ioEx) {
            throw new CopybookCouldNotBeExpanded(ioEx);
        }
        return copyLines;
    }


    public void addAllLinesFromCopyDB2StatementsToFileSectionStatements(List<String> extractedCopyBook) {
        this.fileSectionStatements.addAll(extractedCopyBook);
    }
}
