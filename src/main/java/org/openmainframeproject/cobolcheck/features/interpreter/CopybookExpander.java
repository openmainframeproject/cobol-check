package org.openmainframeproject.cobolcheck.features.interpreter;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringTuple;
import org.openmainframeproject.cobolcheck.services.filehelpers.PathHelper;
import org.openmainframeproject.cobolcheck.services.cobolLogic.CobolLine;
import org.openmainframeproject.cobolcheck.services.cobolLogic.Interpreter;
import org.openmainframeproject.cobolcheck.services.cobolLogic.TokenExtractor;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Expand copybooks referenced by the code under test.
 * <p>
 * In the general use case, COPY statements are left alone and the compiler handles expansion.
 * <p>
 * Cobol-check runs as a precompiler and does not "see" the expanded Cobol source code.
 * There are two cases in which cobol-check may need to "see" the expanded code.
 * <p>
 * 1. If source statements pertinent to a mock are contained in copybooks, cobol-check needs to
 * be able to comment-out those statements in the merged test program.
 * <p>
 * 2. IBM z/OS compilers support nested COPY statements with the REPLACING option. Off-platform
 * compilers written by others do not support this, as it is an IBM extension to Cobol. When the
 * code under test uses this feature, and cobol-check is running on a platform other than z/OS,
 * cobol-check needs to expand the copybooks recursively during preprocessing so the compilation will work.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class CopybookExpander {

    private final String pathToCopybooks;
    private final List<String> copybookFilenameSuffixes;

    public CopybookExpander() {
        pathToCopybooks = getPathToCopybooks();
        copybookFilenameSuffixes = Config.getCopybookFilenameSuffixes();

    }

    public List<String> expand(List<String> expandedLines, String copybookFilename,
            StringTuple... textReplacement) throws IOException {
        String fullPath = PathHelper.findFilePath(pathToCopybooks, copybookFilename, copybookFilenameSuffixes);
        if (fullPath == null)
            throw new IOException("could not find copybook " + copybookFilename + " in " + pathToCopybooks);
        try (BufferedReader copybookReader = new BufferedReader(new FileReader(new File(fullPath)))) {
            String sourceLine;
            while ((sourceLine = copybookReader.readLine()) != null) {
                // Nested COPY
                if (copyStatementIsPresentIn(sourceLine)) {
                    String copybookName = extractCopybookNameFrom(sourceLine);
                    sourceLine = commentOut(sourceLine);
                    expandedLines = expand(expandedLines, copybookName, textReplacement);
                }
                // COPY REPLACING
                if (!textReplacement[0].isEmpty()) {
                    for (StringTuple replace : textReplacement) {
                        for (String pseudoTextDelimiter : Arrays.asList(
                                Constants.PSEUDO_TEXT_DELIMITER_EQUALS, Constants.PSEUDO_TEXT_DELIMITER_COLON)) {
                            if (sourceLine.contains(pseudoTextDelimiter)) {
                                sourceLine = sourceLine.replaceAll(replace.getFirst(), replace.getSecond());
                            }
                        }
                        for (String followingCharacter : Arrays.asList(Constants.PERIOD, Constants.SPACE)) {
                            String textToReplace = Constants.SPACE + replace.getFirst() + followingCharacter;
                            String replacementText = Constants.SPACE + replace.getSecond() + followingCharacter;
                            if (sourceLine.contains(textToReplace)) {
                                sourceLine = sourceLine.replaceAll(textToReplace, replacementText);
                            }
                        }
                    }
                }
                expandedLines.add(sourceLine);
            }
        }
        return expandedLines;
    }

    /**
     * Comments out a source line by placing an asterisk in column 7.
     * If the line is shorter than 7 characters, it is returned unchanged.
     * @param sourceLine
     * @return
     */
    String commentOut(String sourceLine) {
        // Create StringBuilder from sourceLine ensuring it is at least 7 characters long
        if (sourceLine == null || sourceLine.length() < 7) {
            return sourceLine;
        }
        StringBuilder tempLine = new StringBuilder(sourceLine);
        // IF the column 7 is not space, then we are probably dealing with a
        // non-standard source format. In that case, we will throw an exception.
        if (tempLine.charAt(6) != ' '  && tempLine.charAt(6) != '*') {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR034",
                            sourceLine));
        }

        // put an asterisk in column 7 (index 6)
        tempLine.setCharAt(6, '*');
        return tempLine.toString();
    }

    private boolean copyStatementIsPresentIn(String sourceLine) {
        String trimmedLine = sourceLine.trim();
        return (trimmedLine.startsWith("COPY ") || trimmedLine.startsWith("copy "));
    }

    private String extractCopybookNameFrom(String sourceLine) {
        String copybookName;
        StringTokenizer st = new StringTokenizer(sourceLine);
        String copyVerb = st.nextToken();
        if (st.hasMoreTokens()) {
            copybookName = st.nextToken();
            if (copybookName.endsWith(Constants.PERIOD)) {
                copybookName = copybookName.substring(0, copybookName.length() - 1);
            }
        } else {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR008",
                            sourceLine,
                            "CopybookExpander.extractCopybookNameFrom(sourceLine)",
                            sourceLine));
        }
        return copybookName;
    }

    private String getPathToCopybooks() {
        return PathHelper.endWithFileSeparator(Config.getCopyBookSourceDirectoryPathString());
    }

    public List<String> expandDB2(List<String> expandedLines, String copybookFilename) throws IOException {
        String fullPath = PathHelper.findFilePath(pathToCopybooks, copybookFilename, copybookFilenameSuffixes);
        if (fullPath == null)
            throw new IOException("could not find copybook " + copybookFilename + " in " + pathToCopybooks);
        try (BufferedReader copybookReader = new BufferedReader(new FileReader(new File(fullPath)))) {
            String sourceLine;
            while ((sourceLine = copybookReader.readLine()) != null) {
                if (sourceLine.contains(Constants.EXEC_SQL_TOKEN)) {
                    while (!sourceLine.contains(Constants.END_EXEC_TOKEN)) {
                        sourceLine = copybookReader.readLine();
                    }
                sourceLine = copybookReader.readLine();
                }
                if(!sourceLine.isEmpty()) {
                    if(!Interpreter.isComment(sourceLine)) {
                        expandedLines.add(sourceLine);
                    }
                }
            }
        }
        return expandedLines;
    }

    /**
     * Returns the lines included by the INCLUDE statement
     * @param 'exec sql' statement with an INCLUDE clause
     * @return list of lines included by the INCLUDE statement
     * @throws IOException
     */
    public List<String> getIncludedLines(String line) throws IOException {
        List<String> expandedLines = new ArrayList<>();
        String copybookName = extractCopybookNameFromCopyStatement(line);
        return this.expandDB2(expandedLines, copybookName);
    }


    String extractCopybookNameFromCopyStatement(String line) {
        String copybookName ;
        int copybookNameStartPosition = findCopybookNamePositionInCopyStatement(line);
        int copybookNameEndPosition = findCopyBookNameEndPositionInCopyStatement(line, copybookNameStartPosition);
        if (copybookNameStartPosition >= 0 && copybookNameEndPosition > copybookNameStartPosition) {
            copybookName = line.substring(copybookNameStartPosition, copybookNameEndPosition).trim();
            if (copybookName.endsWith(Constants.PERIOD)) {
                copybookName = copybookName.substring(0, copybookName.length() - 1);
            }
        } else {
            throw new PossibleInternalLogicErrorException(
                    Messages.get("ERR008",
                            line,
                            "LineRepository.extractCopybookNameFromCopyStatement(line)",
                            line));
        }
        return copybookName;
    }

    int findCopybookNamePositionInCopyStatement(String line) {
        int copybookNamePosition = -1;
        int copyWordPosition = line.indexOf(Constants.INCLUDE);
        if (copyWordPosition >= 0) {
            copybookNamePosition = copyWordPosition + Constants.INCLUDE.length() + 1;
        }
        return copybookNamePosition;
    }

    int findCopyBookNameEndPositionInCopyStatement(String line, int copybookNameStartPosition) {
        int copybookNameEndPosition = -1;
        if (copybookNameStartPosition >= 0) {
            int periodPosition = line.indexOf(Constants.PERIOD, copybookNameStartPosition);
            int spacePosition = line.indexOf(Constants.SPACE, copybookNameStartPosition);
            if (periodPosition >= 0 && spacePosition >= 0) {
                copybookNameEndPosition = Math.min(periodPosition, spacePosition);
            } else if (periodPosition >= 0) {
                copybookNameEndPosition = periodPosition;
            } else if (spacePosition >= 0) {
                copybookNameEndPosition = spacePosition;
            } else {
                copybookNameEndPosition = line.length();
            }
        }
        return copybookNameEndPosition;
    }

}
