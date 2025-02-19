package org.openmainframeproject.cobolcheck.services.cobolLogic.replace;

import jdk.nashorn.internal.runtime.regexp.joni.Regex;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ReplaceSet {
    private String from;
    private String to;
    private boolean trailing;
    private boolean leading;

    public ReplaceSet(String from, String to, boolean trailing, boolean leading) {
        if (trailing && leading) {
            throw new IllegalArgumentException("Cannot have both trailing and leading set to true");
        }

        this.from = from;
        this.to = to;
        this.trailing = trailing;
        this.leading = leading;
    }

    public ReplaceSet() {
        this.from = "";
        this.to = "";
        this.trailing = false;
        this.leading = false;
    }

    /**
     * Perform 'Replace' in the string (line param). Correponding to the 'REPLACE' statement in COBOL program
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
}
