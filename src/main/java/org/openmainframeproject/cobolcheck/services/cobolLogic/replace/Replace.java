package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.filehelpers.FilePermission;
import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.log.LogLevel;
import org.openmainframeproject.cobolcheck.services.filehelpers.EncodingIO;

import java.io.*;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to handle the COBOL REPLACE statement on the test suite/test case source code.
 * <p>
 * Method inspect() should be called on every line of the COBOL source code to set the REPLACE statements.
 * The source must at least contain a char in the beginning of the line to be considered a valid source line.
 * This char is the comment indicator.
 * The REPLACE statement must be in the standard (IBM) format: REPLACE ==FROM-KEYWORD== BY ==TO-KEYWORD==.
 * <p>
 * Method replace() is called on every line of the unit test source code to replace the strings.
 * <p>
 * <b>There are two main public methods:</b>
 * <ol>
 * <li>replace() - Used on Inspects the test source for replacing; replaceFrom to replaceTo strings.</li>
 * <li>inspect() - Examines the source line for the REPLACE statement and sets the replaceFrom and replaceTo strings.</li>
 * </ol>
 * <p>
 * <b>And two convenience methods:</b>
 * <ol>
 * <li>isReplaceOn() - Returns the current state of the replaceOn flag.</li>
 * <li>2. reset() - Resets the state of the Replace class.</li>
 * </ol>
 */
public class Replace {
    private static final String COBOL_COMMENT_INDICATOR = "*";

    /**
     * Look for the comment indicator in the source line.
     * Capture group description:
     * 1. 0-6 digits/spaces             (group 1) the line numbers (if present)
     * 2. * or space                    (group 2) comment indicator (other markers are not supported)
     * 3.                               (group 3) remainder of the line
     */
    private static final Pattern sourceIsCommentPattern = Pattern.compile("^([\\s|\\d]{0,6})(\\"
            + COBOL_COMMENT_INDICATOR + ")(.+)");
    private static final int SOURCE_COMMENT_INDICATOR = 2;


    /**
     * The state of the REPLACE statement.
     */
    private static boolean replaceOn = false;
    private static final LinkedList<ReplaceSet> replaceMap = new LinkedList<>();

    private static boolean inspect_performed = false;
    private static boolean inspect_performed_warned = false;

    /**
     * Looks in the source line for the replace-key and replaces is with the replace-to-value.
     *
     * @param source a line of cobol-check unit test code
     * @param lineNumber the line number of the source line
     * @return the source line there the appropriate replacement has been made
     */
    public static String replace(String source, int lineNumber) {
        if (!inspect_performed) {
            if (!inspect_performed_warned) {
                inspect_performed_warned = true;
                Log.warn("Replace.replace() called before inspect");
            }
        }
        // if there are no REPLACE statements, return the source line as is
        if (!replaceOn) return source;

        // avoid null pointer exception
        if (source == null || source.isEmpty()) {
            return source;
        }

        // is the source line a comment? quit now...
        if (sourceLineIsComment(source)) {
            return source;
        }

        String replacesString = source;

        for (ReplaceSet replaceSet : replaceMap) {
            replacesString = replaceSet.replaceInline(replacesString, lineNumber);
            if ((Log.level() == LogLevel.TRACE) && (!replacesString.equals(source))) {
                Log.trace("Replace.replace(): Key: <" + replaceSet.getFrom() + ">, result: " + replacesString);
            }
        }
        return replacesString;
    }

    public static String replace(String source) {
        return replace(source, 0);
    }


    public static void inspectProgram(File cobolProgram) {
        reset();

        // Use the statement locator to find the REPLACE statements in the COBOL program
        ReplaceStatementLocator rsl = new ReplaceStatementLocator(cobolProgram);
        replaceMap.addAll(rsl.getReplaceSets());

        if (replaceMap.isEmpty()) {
            replaceOn = false;
        } else {
            replaceOn = true;
        }
        inspect_performed = true;
        Log.info("Replace.inspectProgram(): Inspecting the COBOL program file: " + cobolProgram + " found "
                + replaceMap.size() + " REPLACE statements");
    }

    /**
     * Examines the source line for the COBOL commment indicator
     *
     * @param source a line of cobol-check unit test code
     * @return true if the source line is a comment, false otherwise
     */
    private static boolean sourceLineIsComment(String source) {
        Matcher sourceElements = sourceIsCommentPattern.matcher(source);
        if (sourceElements.find()) {
            // Is the line a comment? true or false
            return sourceElements.group(SOURCE_COMMENT_INDICATOR).equals(COBOL_COMMENT_INDICATOR);
        }
        return false;
    }

    /**
     * Returns the current state of the replaceOn flag.
     *
     * @return true if there is an active REPLACE statement, false otherwise.
     */
    public static boolean isReplaceOn() {
        return replaceOn;
    }

    /**
     * Return the number of replace sets
     * @return the number of replace sets
     */
    public static int getReplaceSetsSize() {
        return replaceMap.size();
    }

    /**
     * Resets the state of the Replace class to the initial state.
     */
    private static void reset() {
        replaceOn = false;
        replaceMap.clear();
        inspect_performed = false;
        inspect_performed_warned = false;
    }

    public static String replaceInProgram(File program) {
        // was there replace statements in the source, if not, return the original filename
        if (!replaceOn) {
            Log.debug("Replace.replaceInProgram(): No REPLACE statements found in the COBOL program file: " + program);
            return program.getAbsolutePath();
        }

        // write the replaced program back to disk
        String newFileName = program.getAbsolutePath()+"_rpl";
        boolean fileForReplacedSourceExisted = fileForReplacedSourceExists(newFileName);
        Log.info("Replace.replaceInProgram(): Writing the COBOL program file: " + newFileName);
        try {
            BufferedWriter writer = (BufferedWriter) EncodingIO.getWriterWithCorrectEncoding(newFileName);
            // read the program one line at the time
            BufferedReader reader = (BufferedReader) EncodingIO.getReaderWithCorrectEncoding(String.valueOf(program));
            //for every line in the program, replace and write to output file
            String line;
            int lineCount = 0;
            while ((line = reader.readLine()) != null) {
                writer.write(Replace.replace(line, lineCount++));
                writer.newLine();
            }
            writer.close();
            reader.close();

            if (!fileForReplacedSourceExisted) {
                updateFilePermissions(newFileName);
            }

        } catch (IOException e) {
            Log.error("Replace.replaceInProgram(): Error writing the COBOL program file: " + program);
            throw new RuntimeException("Replace.replaceInProgram(): Error writing the COBOL program file: " + program, e);
        }
        return newFileName;
    }

    /**
     * Check if the file for the replaced source exists
     * @param newFileName the name of the file to check
     * @return true if the file exists, false otherwise
     */
    private static boolean fileForReplacedSourceExists(String newFileName) {
        File file = new File(newFileName);
        return file.exists();
    }

    /**
     * Update the file permissions for the replaced source file
     * @param newFileName the name of the file to update
     */
    private static void updateFilePermissions(String newFileName) {
        String permissions = Config.getGeneratedFilesPermissionAll();
        FilePermission.setFilePermissionForAllUsers(new File(newFileName), permissions);
    }

    public static void showReplaceSets() {
        for (ReplaceSet replaceSet : replaceMap) {
            Log.info("Replace.showReplaceSets():" + replaceSet.toString());
        }
    }
}
