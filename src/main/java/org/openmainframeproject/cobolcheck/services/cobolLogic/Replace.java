package org.openmainframeproject.cobolcheck.services.cobolLogic;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to handle the COBOL REPLACE statement on the test suite/test case source code.
 *
 * There are two main public methods:
 * 1. replace() - Used on Inspects the test source for replacing; replaceFrom to replaceTo strings.
 * 2. setReplaceStatement() - Examines the source line for the REPLACE statement and sets the replaceFrom and replaceTo strings.
 *
 * And two convenience methods:
 * 1. isReplaceOn() - Returns the current state of the replaceOn flag.
 * 2. reset() - Resets the state of the Replace class.
 *
 * Method setReplaceStatement() is called on every line of the COBOL source code to set up the REPLACE statements.
 * The source must at least contain a char in the beginning of the line to be considered a valid source line.
 * This char is the comment indicator.
 * The REPLACE statement must be in the standard (IBM) format: REPLACE ==FROM-KEYWORD== BY ==TO-KEYWORD==.
 *
 * Method replace() is called on every line of the ubit test source code to replace the strings.
 *
 *
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
     *                                            first == (indicating the beginning of replace-from-keyword)
     * 6. 1 or more characters          (group 6) the replace-from-keyword
     * 7. ==                            (group 7) the second == (indicating the end of replace-from-keyword)
     * 8. by                            (group 8) the BY keyword
     * 9. 0 or more spaces              (group 9) spaces between BY keyword and the next key word
     * 10. ==                           (group 10) the third == (indicating the beginning of replace-to-keyword)
     * 11. 1 or more characters         (group 11) the replace-to-keyword
     * 12. ==                           (group 12) the fourth == (indicating the end of replace-to-keyword)
     * 13. .                            (group 13) the end of the statement
     *
     * ^ indicates we look from the beginning of the line
     * the pattern is case insensitivé
     */
    private static final Pattern replacePattern = Pattern.compile( "^([\\s|\\d]{0,6})([\\" + COBOL_COMMENT_INDICATOR + "|\\s])(\\s*)(" + COBOL_WORD_REPLACE + ")(\\s*==)(.+)(==\\s*)(by)(\\s*)(==)(.+)(==\\s*)(.)", Pattern.CASE_INSENSITIVE);
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
    private static final Pattern commentOffPattern = Pattern.compile( "^([\\s|\\d]{0,6})([\\" + COBOL_COMMENT_INDICATOR + "|\\s])(" + COBOL_WORD_REPLACE + ")(\\s*)(OFF)(\\s*)(.)", Pattern.CASE_INSENSITIVE);
    private static final int GROUP_OFF_COMMENT_INDICATOR = 2;
    private static final int GROUP_OFF_END_OF_STATEMENT = 7;

    /**
     * The state of the REPLACE statement.
     */
    private static boolean replaceOn = false;
    /**
     * The string to find and replace.
     */
    private static String replaceFrom = "";
    /**
     * The string to replace with.
     */
    private static String replaceTo = "";

    /**
     * Inspects the source line for the REPLACE statement and sets the replaceFrom and replaceTo strings.
     * Investigates the source line for the replace-key and replaces is with the replace-to-value.
     *
     * @param source a line of cobol-check unit test code
     * @return the source line there the appropriate replacement has been made
     */
    public static String replace(String source) {

        // avoid null pointer exception
        if (source == null || source.isEmpty()) {
            return source;
        }

        if (replaceOn) {
            System.out.println("Replacing: " + replaceFrom + " with: " + replaceTo);
            return source.replaceAll(replaceFrom, replaceTo);
        } else {
            return source;
        }
    }

    /**
     * Examines the source line for the REPLACE statement and sets the replaceFrom and replaceTo strings.
     * @param source
     */
    public static void setReplaceStatement(String source) {
        // avoid null pointer exception
        if (source == null || source.isEmpty()) {
            return ;
        }

        Matcher replaceStatementElements = replacePattern.matcher(source);

        if (replaceStatementElements.find()) {
            // Is the line a comment? then stop processing
            if (replaceStatementElements.group(GROUP_COMMENT_INDICATOR).equals(COBOL_COMMENT_INDICATOR)) {
                return;
            }

            // Replace on?
            if (!replaceStatementElements.group(GROUP_REPLACE_KEYWORD).isEmpty() && !replaceStatementElements.group(GROUP_END_OF_STATEMENT).isEmpty()) {
                replaceOn = true;
                replaceFrom = replaceStatementElements.group(GROUP_REPLACE_FROM);
                replaceTo = replaceStatementElements.group(GROUP_REPLACE_TO);
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
     * @return true if there is an active REPLACE statement, false otherwise.
     *
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
        replaceFrom = "";
        replaceTo = "";
    }
}
