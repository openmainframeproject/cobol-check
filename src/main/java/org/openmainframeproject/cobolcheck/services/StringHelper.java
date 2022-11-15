package org.openmainframeproject.cobolcheck.services;

import org.openmainframeproject.cobolcheck.services.log.Log;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

/**
 * Helper methods for handling String values specific to this project.
 */
public class StringHelper {

    /**
     * @param subject - reference to a String
     * @return true if the subject is null or empty
     */
    public static boolean isBlank(String subject) {
        return subject == null || subject.equals("");
    }

    /**
     * @param subject - reference to a String
     * @return true if the subject contains a value other than empty string
     */
    public static boolean notBlank(String subject) {
        return subject != null && !subject.equals("");
    }

    /**
     * Provide a default value for a String if the String is null or empty
     *
     * @param subject - reference to a String
     * @param defaultValue - the value to use if the String is null or empty
     * @return the original String value or the specified default value
     */
    public static String defaultIfBlank(String subject, String defaultValue) {
        return isBlank(subject) ? defaultValue : subject;
    }

    /**
     * Check for empty array
     *
     * @param subject - reference to the array
     * @return true if subject is null or subject array contains no entries
     */
    public static boolean isEmptyArray(String[] subject) {
        return subject == null || subject.length == 0;
    }

    /**
     * Ensure a string that represents a Cobol source line is exactly 80 bytes long
     *
     * @param sourceLine that may be less than 80 bytes long
     * @return the same source line padded to 80 bytes
     */
    public static String fixedLength(String sourceLine) {
        return String.format("%1$-80s", sourceLine).substring(0, 80) +
                System.getProperty("line.separator");
    }

    /**
     * Ensure a path string value contains platform-specific file separator characters
     *
     * @param pathString that may not have platform-specific file separator characters
     * @return path string value with platform-specific file separator characters
     */
    public static String adjustPathString(String pathString) {
        Log.debug("StringHelper.adjustPathString() original string: <" + pathString + ">");
        Log.debug("StringHelper.adjustPathString() Constants.FILE_SEPARATOR: <" + Constants.FILE_SEPARATOR + ">");
        Log.debug("StringHelper.adjustPathString() os.name is <" + System.getProperty("os.name") + ">");

        String result = pathString;
        if (System.getProperty("os.name").toLowerCase(Locale.ROOT).startsWith("win")) {
            result =  pathString.replace("/", "\\");
        } else {
            result = pathString.replace("\\", "/");
        }
        Log.debug("StringHelper.adjustStringPath() result is <" + result + ">");
        return result;
    }

    public static String changeFileExtension(String path, String extension){
        if (path.contains(".")){
            int dotIndex = path.indexOf(".");
            path = path.substring(0, dotIndex);
        }
        return path + "." + extension;
    }

    /**
     * Trims only the end of the string.
     * Ex.: "     Hey  \n" => "     Hey"
     *
     * @param line - original string
     * @return - string trimmed at the end.
     */
    public static String removeTrailingSpaces(String line){
        //Regex for trailing spaces
        String regex = "\\s+$";
        return line.replaceAll(regex, "");
    }

    /**
     * Checks if a character occurs before another
     *
     * @param expectedFirst - The char expected to be first
     * @param expectedSecond - The char expected to be second
     * @return - True if the first char occurs before the second.
     * False if second occurs first or string doesn't contain the chars
     */
    public static boolean occursFirst (String text, char expectedFirst, char expectedSecond){
        for (char c : text.toCharArray()){
            if (c == expectedFirst){
                return true;
            }
            if (c == expectedSecond){
                return false;
            }
        }
        return false;
    }

    /**
     * Swaps two characters in a given string.
     * Example: swapChars("1.000.000,00", '.', ',') => "1,000,000.00"
     * @param c1 - One of the chars to swap
     * @param c2 - One of the chars to swap
     * @return - The given string with the given char values swapped
     */
    public static String swapChars(String text, char c1, char c2){
        char[] textArray = text.toCharArray();
        for (int i = 0; i < text.length(); i++){
            if (textArray[i] == c1)
                textArray[i] = c2;
            else if(textArray[i] == c2)
                textArray[i] = c1;
        }
        return new String(textArray);
    }


    public static String commentOutLine(String line){
        if (line.startsWith("       ")){
            return "      *" + line.substring(Constants.COMMENT_SPACE_OFFSET);
        }
        if (line.startsWith("      *")){
            return line;
        }
        else {
            return "      *" + line;
        }

    }

    public static String stubLine(String line, String stubTag){
        if (stubTag == null || stubTag.isEmpty())
            return commentOutLine(line);

        int stubTagLength = stubTag.length();
        if (line.startsWith("       ")){
            int leadingSpaceIndex = getNumberOfLeadingSpaces(line.substring(Constants.COMMENT_SPACE_OFFSET));
            if (stubTagLength <= leadingSpaceIndex)
                return "      *" + stubTag + line.substring(Constants.COMMENT_SPACE_OFFSET + stubTagLength);
            else
                return "      *" + stubTag + line.substring(Constants.COMMENT_SPACE_OFFSET + leadingSpaceIndex);
        }
        if (line.startsWith("      *")){
            int starIndex = line.indexOf('*');
            int leadingSpaceIndex = getNumberOfLeadingSpaces(line.substring(starIndex + 1));
            if (stubTagLength <= leadingSpaceIndex)
                return line.substring(0, starIndex + 1) + stubTag + line.substring(starIndex + 1 + stubTagLength);
            else
                return line.substring(0, starIndex + 1) + stubTag + line.substring(starIndex + 1 + leadingSpaceIndex);
        }
        else {
            return "      *" + stubTag + line;
        }
    }

    public static int getNumberOfLeadingSpaces(String line){
        if (line.length() == 0)
            return 0;

        char[] characters = line.toCharArray();
        int index = 0;

        while (characters[index] == ' '){
            index++;
        }
        return index;
    }

    public static String ExcludeBetweenTags(String value, char startTag, char endTag){
        boolean insideTags = false;
        String output = "";
        char[] characters = value.toCharArray();
        for (char c : characters){
            if (insideTags){
                if (c == endTag){
                    insideTags = false;
                    output += c;
                }
            }
            else {
                if (c == startTag)
                    insideTags = true;

                output += c;
            }
        }
        return output;
    }

    public static boolean isStringContinuationLine(String line) {
        return line.startsWith("      -    '") || line.startsWith("      -    \"");
    }

    public static String moveToAreaB(String line){
        return "           " + line.trim();
    }

    public static List<String> moveToAreaB(List<String> lines){
        for (int i = 0; i < lines.size(); i++){
            lines.set(i, moveToAreaB(lines.get(i)));
        }
        return lines;
    }

    /**
     * Enclose a value in quotation marks.
     *
     * @param value - original string
     * @return - quoted string
     */
    public String quote(String value) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(Constants.QUOTE);
        buffer.append(value);
        buffer.append(Constants.QUOTE);
        return buffer.toString();
    }

    public static String removeLastIndex(String value){
        if (value != null && !isBlank(value)){
            return value.substring(0, value.length() - 2);
        }
        return value;
    }

    public static boolean equalsAny(String value, Collection<String> collection){
        if (value == null || value.isEmpty() || collection == null)
            return false;

        for (String c : collection){
            if (value.equals(c))
                return true;
        }
        return false;
    }

    public static boolean containsAny(String value, Collection<String> collection){
        if (value == null || value.isEmpty() || collection == null)
            return false;

        for (String c : collection){
            if (value.contains(c))
                return true;
        }
        return false;
    }

    public static boolean startsWithAny(String value, Collection<String> collection){
        if (value == null || value.isEmpty() || collection == null)
            return false;

        for (String c : collection){
            if (value.startsWith(c))
                return true;
        }
        return false;
    }
}
