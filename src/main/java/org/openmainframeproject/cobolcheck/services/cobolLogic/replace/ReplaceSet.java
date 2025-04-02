package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;


import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to handle the COBOL REPLACE statement keys on the test suite/test case source code.
 * <p>
 * When fromSourceLine and untilSourceLine are set, the replace is only performed on the lines between these two lines.
 * When the values are zero, the replace key set is applied to all lines.
 */
public class ReplaceSet {
    private String from;
    private String to;
    private boolean trailing;
    private boolean leading;
    private int fromSourceLine;
    private int untilSourceLine;

    public ReplaceSet(String from, String to, boolean trailing, boolean leading,int fromSourceLine, int untilSourceLine) {
        if (trailing && leading) {
            throw new IllegalArgumentException("Cannot have both trailing and leading set to true");
        }

        this.from = from;
        this.to = to;
        this.trailing = trailing;
        this.leading = leading;
        this.fromSourceLine = fromSourceLine;
        this.untilSourceLine = untilSourceLine;
    }

    public ReplaceSet() {
        this.from = "";
        this.to = "";
        this.trailing = false;
        this.leading = false;
        this.fromSourceLine = 0;
        this.untilSourceLine = 0;
    }

    /**
     * Perform 'Replace' in the string (line param). Corresponding to the 'REPLACE' statement in COBOL program
     * And the values parsed from the statements are used to replace the values in the line.
     *
     * @param line The line to replace in
     * @return The line with the replacement
     */
    public String replaceInline(String line) {
        if (!trailing && !leading) {
            // We match with no case sensitivity by using the (?i) flag
            return line.replaceAll("(?i)"+from, to);
        }
        else  {
            // Leading or trailing is set to true
            String patternStr = "";
            if (trailing) {
                patternStr = "(?i)\\S+(" + from + ")";
            } else {
                patternStr = "(?i)(" + from + ")\\S+";
            }

            Pattern pattern = Pattern.compile(patternStr);
            Matcher matcher = pattern.matcher(line);
            //Iterate through the matches and replace the from with to
            while (matcher.find()) {
                String match = matcher.group();
                line = line.replace(match, match.replace(from, to));
            }
            return line;
        }
    }

    /**
     * Perform 'Replace' in the string (line param). Corresponding to the 'REPLACE' statement in COBOL program
     * And the values parsed from the statements are used to replace the values in the line.
     * @param line
     * @param sourceLine
     * @return
     */
    public String replaceInline(String line, int sourceLine) {
        // if the line is zero, the replace key set is applied
        if (sourceLine == 0) return replaceInline(line);

        // when fromSourceLine and untilSourceLine are zero, the replace key set is applied to all lines.
        if (fromSourceLine == 0 && untilSourceLine == 0) return replaceInline(line);

        // when the line number is between fromSourceLine and untilSourceLine, the replace is performed
        if ((sourceLine >= fromSourceLine && sourceLine <= untilSourceLine) ||
           ((sourceLine >= fromSourceLine && untilSourceLine == 0))) return replaceInline(line);

        // Otherwise, return the line as is
        return line;
    }

    public void setTrailing(boolean trailing) {
        if (trailing && this.leading) {
            throw new IllegalArgumentException("Cannot have both trailing and leading set to true");
        }
        this.trailing = trailing;
    }

    public void setLeading(boolean leading) {
        if (leading && this.trailing) {
            throw new IllegalArgumentException("Cannot have both trailing and leading set to true");
        }
        this.leading = leading;
    }

    public void setFrom(String value) {
        this.from = value;
    }

    public void setTo(String value) {
        this.to = value;
    }

    public String getFrom() {
        return from;
    }

    public String getTo() {
        return to;
    }

    public boolean isTrailing() {
        return trailing;
    }

    public boolean isLeading() {
        return leading;
    }

    public void setFromSourceLine(int sourceLineNumber) {
        this.fromSourceLine = sourceLineNumber;
    }
    public void setUntilSourceLine(int sourceLineNumber) {
        this.untilSourceLine = sourceLineNumber;
    }
    public int getFromSourceLine() {
        return fromSourceLine;
    }
    public int getUntilSourceLine() {
        return untilSourceLine;
    }
    public String toString() {
        return "From: " + from + ", To: " + to + ", Trailing: " + trailing + ", Leading: " + leading + ", FromSourceLine: " + fromSourceLine + ", UntilSourceLine: " + untilSourceLine;
    }
}
