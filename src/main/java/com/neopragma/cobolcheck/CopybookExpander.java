package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Path;
import java.util.StringTokenizer;

/**
 * Expand copybooks referenced by the code under test.
 *
 * In the general use case, COPY statements are left alone and the compiler handles expansion.
 *
 * Cobol-check runs as a precompiler and does not "see" the expanded Cobol source code.
 * There are two cases in which cobol-check may need to "see" the expanded code.
 *
 * 1. If source statements pertinent to a mock are contained in copybooks, cobol-check needs to
 * be able to comment-out those statements in the merged test program.
 *
 * 2. IBM z/OS compilers support nested COPY statements with the REPLACING option. Off-platform
 * compilers written by others do not support this, as it is an IBM extension to Cobol. When the
 * code under test uses this feature, and cobol-check is running on a platform other than z/OS,
 * cobol-check needs to expand the copybooks recursively during preprocessing so the compilation will work.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class CopybookExpander implements Constants, StringHelper {

    private final Config config;
    private Messages messages;
    private final String pathToCopybooks;

    public CopybookExpander(Config config, Messages messages) {
        this.config = config;
        this.messages = messages;
        pathToCopybooks = getPathFor("application.copybook.directory", "testcobolcopybooks");
    }

    public Writer expand(Writer expandedSource,
                         String copybookFilename,
                         String copybookFilenameSuffix) throws IOException {
        return expand(expandedSource,
                copybookFilename,
                copybookFilenameSuffix,
                EMPTY_STRING,
                EMPTY_STRING);
    }

    public Writer expand(Writer expandedSource,
                         String copybookFilename,
                         String copybookFilenameSuffix,
                         String textToReplace,
                         String replacementText) throws IOException {
        BufferedReader copybookReader
                = new BufferedReader(new FileReader(
                        Path.of(pathToCopybooks
                                + copybookFilename
                                + copybookFilenameSuffix)
                                .toFile()));
        String sourceLine = EMPTY_STRING;
        while ((sourceLine = copybookReader.readLine()) != null) {
            if (copyStatementIsPresentIn(sourceLine)) {
                String copybookName = extractCopybookNameFrom(sourceLine);
                StringBuilder tempLine = new StringBuilder(sourceLine);
                tempLine.setCharAt(6, '*');
                sourceLine = tempLine.toString();
                if (textToReplace != EMPTY_STRING & replacementText != EMPTY_STRING) {
                    sourceLine = sourceLine.replaceAll(textToReplace, replacementText);
                }
                expandedSource = expand(expandedSource, copybookName, copybookFilenameSuffix, textToReplace, replacementText);
            }
            sourceLine = fixedLength(sourceLine);
            expandedSource.write(sourceLine);
        }
        copybookReader.close();
        return expandedSource;
    }

    private boolean copyStatementIsPresentIn(String sourceLine) {
        String trimmedLine = sourceLine.trim();
        return (trimmedLine.startsWith("COPY ") || trimmedLine.startsWith("copy "));
    }

    private String extractCopybookNameFrom(String sourceLine) {
        String copybookName = EMPTY_STRING;
        StringTokenizer st = new StringTokenizer(sourceLine);
        String copyVerb = st.nextToken();
        if (st.hasMoreTokens()) {
            copybookName = st.nextToken();
            if (copybookName.endsWith(PERIOD)) {
                copybookName = copybookName.substring(0, copybookName.length() - 1);
            }
        } else {
            throw new PossibleInternalLogicErrorException(
                    messages.get("ERR008",
                            sourceLine,
                            "CopybookExpander.extractCopybookNameFrom(sourceLine)",
                            sourceLine));
        }
        return copybookName;
    }

    private String getPathFor(String configPropertyName, String defaultValue) {
        String pathString = EMPTY_STRING;
        String directoryName =
                config.getString(configPropertyName,
                        "testcobolsources");
        if (directoryName.startsWith(FILE_SEPARATOR)) {
            pathString = directoryName;
        } else {
            pathString =
                    config.getString("resources.directory")
                            + FILE_SEPARATOR
                            + this.getClass().getPackageName().replace(".", FILE_SEPARATOR)
                            + FILE_SEPARATOR
                            + directoryName
                            + FILE_SEPARATOR;
        }
        return pathString;
    }

}
