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

import java.util.Locale;

/**
 * Helper methods for handling String values specific to this project.
 */
public interface StringHelper {

    /**
     * @param subject - reference to a String
     * @return true if the subject is null or empty
     */
    default boolean isBlank(String subject) {
        return subject == null || subject.equals("");
    }

    /**
     * @param subject - reference to a String
     * @return true if the subject contains a value other than empty string
     */
    default boolean notBlank(String subject) {
        return subject != null && !subject.equals("");
    }

    /**
     * Provide a default value for a String if the String is null or empty
     *
     * @param subject - reference to a String
     * @param defaultValue - the value to use if the String is null or empty
     * @return the original String value or the specified default value
     */
    default String defaultIfBlank(String subject, String defaultValue) {
        return isBlank(subject) ? defaultValue : subject;
    }

    /**
     * Check for empty array
     *
     * @param subject - reference to the array
     * @return true if subject is null or subject array contains no entries
     */
    default boolean isEmptyArray(String[] subject) {
        return subject == null || subject.length == 0;
    }

    /**
     * Ensure a string that represents a Cobol source line is exactly 80 bytes long
     *
     * @param sourceLine that may be less than 80 bytes long
     * @return the same source line padded to 80 bytes
     */
    default String fixedLength(String sourceLine) {
        return String.format("%1$-80s", sourceLine).substring(0, 80) +
                System.getProperty("line.separator");
    }

    /**
     * Ensure a path string value contains platform-specific file separator characters
     *
     * @param pathString that may not have platform-specific file separator characters
     * @return path string value with platform-specific file separator characters
     */
    default String adjustPathString(String pathString) {
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
}
