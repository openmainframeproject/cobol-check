/*
Copyright 2020 David Nicolette

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.io.*;
import java.nio.file.Path;
import java.util.List;
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
    private final Messages messages;
    private final String pathToCopybooks;

    public CopybookExpander(Config config, Messages messages) {
        this.config = config;
        this.messages = messages;
        pathToCopybooks = getPathToCopybooks();
    }

    public Writer expand(Writer expandedSource,
                         String copybookFilename,
                         String copybookFilenameSuffix) throws IOException {
        return expand(expandedSource,
                copybookFilename,
                copybookFilenameSuffix,
                new StringTuple(null, null));
    }

    public Writer expand(Writer expandedSource,
                         String copybookFilename,
                         String copybookFilenameSuffix,
                         StringTuple... textReplacement) throws IOException {

        BufferedReader copybookReader
                = new BufferedReader(new FileReader(
                        Path.of(pathToCopybooks
                                + copybookFilename
                                + copybookFilenameSuffix)
                                .toFile()));
        String sourceLine;
        while ((sourceLine = copybookReader.readLine()) != null) {
            // Nested COPY
            if (copyStatementIsPresentIn(sourceLine)) {
                String copybookName = extractCopybookNameFrom(sourceLine);
                sourceLine = commentOut(sourceLine);
                expandedSource = expand(expandedSource, copybookName, copybookFilenameSuffix, textReplacement);
            }
            // COPY REPLACING
            if (!textReplacement[0].isEmpty()) {
                for (StringTuple replace : textReplacement) {
                    for (String pseudoTextDelimiter : List.of (PSEUDO_TEXT_DELIMITER_EQUALS, PSEUDO_TEXT_DELIMITER_COLON)) {
                        if (sourceLine.contains(pseudoTextDelimiter)) {
                            sourceLine = sourceLine.replaceAll(replace.getFirst(), replace.getSecond());
                        }
                    }
                    for (String followingCharacter : List.of(PERIOD, SPACE)) {
                        String textToReplace = SPACE + replace.getFirst() + followingCharacter;
                        String replacementText = SPACE + replace.getSecond() + followingCharacter;
                        if (sourceLine.contains(textToReplace)) {
                            sourceLine = sourceLine.replaceAll(textToReplace, replacementText);
                        }
                    }
                }
            }
            sourceLine = fixedLength(sourceLine);
            expandedSource.write(sourceLine);
        }
        copybookReader.close();
        return expandedSource;
    }

    private String commentOut(String sourceLine) {
        StringBuilder tempLine = new StringBuilder(sourceLine);
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

    private String getPathToCopybooks() {
        StringBuilder directoryName = new StringBuilder();
        directoryName.append(new File("./").getAbsolutePath());
        directoryName.append(FILE_SEPARATOR);
        directoryName.append(config.getString("application.copybook.directory", "src/main/cobol/copy"));
        if (!directoryName.toString().endsWith(FILE_SEPARATOR)) {
            directoryName.append(FILE_SEPARATOR);
        }
        return directoryName.toString();
    }

}
