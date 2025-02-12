package org.openmainframeproject.cobolcheck.services.cobolLogic;

import org.openmainframeproject.cobolcheck.services.log.Log;
import org.openmainframeproject.cobolcheck.services.log.LogLevel;

import java.io.*;
import java.util.HashMap;
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
    private static final String COBOL_WORD_REPLACE = "REPLACE";
    private static final String COBOL_COMMENT_INDICATOR = "*";
    /**
     * RegEx pattern to match the REPLACE statement in the source code.
     * It 'parses' the entire replace statement in one go and does not handle multiple line statement.
     * Capture group description:
     * 1. 0-6 digits/spaces             (group 1) the line numbers (if present)
     * 2. * or space                    (group 2) comment indicator (other markers are not supported)
     * 3. 0 or more spaces              (group 3) spaces between comment indicator and the REPLACE keyword
     * 4. REPLACE                       (group 4) the REPLACE keyword
     * 5. 0 or more spaces              (group 5) spaces between REPLACE keyword and the keyword marker (==) and the
     * first == (indicating the beginning of replace-from-keyword)
     * 6. 1 or more characters          (group 6) the replace-from-keyword
     * 7. ==                            (group 7) the second == (indicating the end of replace-from-keyword)
     * 8. by                            (group 8) the BY keyword
     * 9. 0 or more spaces              (group 9) spaces between BY keyword and the next key word
     * 10. ==                           (group 10) the third == (indicating the beginning of replace-to-keyword)
     * 11. 1 or more characters         (group 11) the replace-to-keyword
     * 12. ==                           (group 12) the fourth == (indicating the end of replace-to-keyword)
     * 13. .                            (group 13) the end of the statement
     * <p>
     * ^ indicates we look from the beginning of the line
     * the pattern is case insensitivé
     */
    private static final Pattern replacePattern = Pattern.compile("^([\\s|\\d]{0,6})([\\"
            + COBOL_COMMENT_INDICATOR + "|\\s])(\\s*)("
            + COBOL_WORD_REPLACE + ")(\\s*==)(.+)(==\\s*)(by)(\\s*)(==)(.+)(==\\s*)(.)", Pattern.CASE_INSENSITIVE);
    private static final int GROUP_COMMENT_INDICATOR = 2;
    private static final int GROUP_REPLACE_KEYWORD = 4;
    private static final int GROUP_REPLACE_FROM = 6;
    private static final int GROUP_REPLACE_TO = 11;
    private static final int GROUP_END_OF_STATEMENT = 13;

    /**
     * RegEx pattern to match the REPLACE OFF statement in the source code.
     * It 'parses' the entire replace statement in one go and does not handle multiple line statement.
     * Capture group description:
     * 1. 0-6 digits/spaces             (group 1) the line numbers (if present)
     * 2. * or space                    (group 2) comment indicator (other markers are not supported)
     * 3. REPLACE                       (group 3) the REPLACE keyword
     * 4. 0 or more spaces              (group 4) spaces between REPLACE keyword and the keyword off
     * 5. OFF                           (group 5) the OFF keyword
     * 6. 0 or more spaces              (group 6) spaces between OFF keyword and the end of the statement
     * 7. .                             (group 7) the end of the statement
     * ^ indicates we look from the beginning of the line
     * the pattern is case insensitivé
     */
    private static final Pattern commentOffPattern = Pattern.compile("^([\\s|\\d]{0,6})([\\"
            + COBOL_COMMENT_INDICATOR + "|\\s])("
            + COBOL_WORD_REPLACE + ")(\\s*)(OFF)(\\s*)(.)", Pattern.CASE_INSENSITIVE);
    private static final int GROUP_OFF_COMMENT_INDICATOR = 2;
    private static final int GROUP_OFF_END_OF_STATEMENT = 7;

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
    private static final HashMap<String, String> replaceMap = new HashMap<>();
    /*
     * Private constructor to prevent instantiation.
     */
    private static boolean inspect_performed = false;
    private static boolean inspect_performed_warned = false;

    /**
     * Looks in the source line for the replace-key and replaces is with the replace-to-value.
     *
     * @param source a line of cobol-check unit test code
     * @return the source line there the appropriate replacement has been made
     */
    public static String replace(String source) {
        if (!inspect_performed) {
            if (!inspect_performed_warned) {
                inspect_performed_warned = true;
                Log.warn("Replace.replace() called before inspect");
            }
        }

        // avoid null pointer exception
        if (source == null || source.isEmpty()) {
            return source;
        }


        if (replaceOn) {
            // is the source line a comment? then stop processing
            if (sourceLineIsComment(source)) {
                return source;
            }
            String alteredString = source;
            String value;
            for (String key : replaceMap.keySet()) {
                value = replaceMap.get(key);
                Log.trace("Replace.replace(): Key: <" + key + ">, Value: <" + replaceMap.get(key) + ">");
                alteredString = alteredString.replaceAll(key, value);
                if ((Log.level() == LogLevel.TRACE) && (!alteredString.equals(source))) {
                    Log.trace("Replace.replace(): Key: <" + key + ">, result: " + alteredString);
                }
            }
            return alteredString;
        }


        // if replace is not in effect, return the source line as is
        return source;
    }

    private static boolean sourceLineIsComment(String source) {
        Matcher sourceElements = sourceIsCommentPattern.matcher(source);
        if (sourceElements.find()) {
            // Is the line a comment? true or false
            return sourceElements.group(SOURCE_COMMENT_INDICATOR).equals(COBOL_COMMENT_INDICATOR);
        }
        return false;
    }

    public static void inspectProgram(File cobolProgram) {
        Log.trace("Replace.inspectProgram(): Inspecting the COBOL program file: " + cobolProgram);
        //Iterate over the file and inspect each line
        try (BufferedReader reader = new BufferedReader(new FileReader(cobolProgram))) {
            String line;
            while ((line = reader.readLine()) != null) {
                inspect(line);
            }
        } catch (FileNotFoundException e) {
            Log.error("Replace.inspectProgram(): File not found: " + e.getMessage());
            throw new RuntimeException(e);
        } catch (IOException e) {
            Log.error("Replace.inspectProgram(): Error reading the COBOL program file: " + e.getMessage());
            throw new RuntimeException(e);
        }
    }

    /**
     * Examines the source line for the REPLACE statement and register the replaceFrom and replaceTo
     * values.
     *
     * @param source code line to inspect
     */
    public static void inspect(String source) {
        // avoid null pointer exception
        if (source == null || source.isEmpty()) {
            return;
        }
        inspect_performed = true;

        Matcher replaceStatementElements = replacePattern.matcher(source);

        if (replaceStatementElements.find()) {
            // Is the line a comment? then stop processing
            if (replaceStatementElements.group(GROUP_COMMENT_INDICATOR).equals(COBOL_COMMENT_INDICATOR)) {
                return;
            }

            if (!replaceStatementElements.group(GROUP_REPLACE_KEYWORD).isEmpty() && !replaceStatementElements.group(GROUP_END_OF_STATEMENT).isEmpty()) {
                // Replace keywords found
                replaceOn = true;
                String replaceFrom = replaceStatementElements.group(GROUP_REPLACE_FROM);
                String replaceTo = replaceStatementElements.group(GROUP_REPLACE_TO);
                replaceMap.put(replaceFrom, replaceTo);
                Log.trace("Replace.inspect(): Keywords found, replace <" + replaceFrom + "> with <" + replaceTo + ">");

            }
        } else {
            Matcher replaceOffMarker = commentOffPattern.matcher(source);
            if (replaceOffMarker.find()) {
                // Is the line a comment? then stop processing
                if (replaceOffMarker.group(GROUP_OFF_COMMENT_INDICATOR).equals(COBOL_COMMENT_INDICATOR)) {
                    return;
                }
                // replace off - complete statement?
                if (!replaceOffMarker.group(GROUP_OFF_END_OF_STATEMENT).isEmpty()) {
                    replaceOn = false;
                }
            }
        }
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
     * <b>Mainly for unit testing</b>
     * Resets the state of the Replace class.
     * Should be used with care, as it will reset the replaceOn flag and the replaceFrom and replaceTo strings.
     */
    public static void reset() {
        replaceOn = false;
        replaceMap.clear();
    }
}
