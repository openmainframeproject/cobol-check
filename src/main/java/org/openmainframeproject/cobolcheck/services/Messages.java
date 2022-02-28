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
package org.openmainframeproject.cobolcheck.services;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Loads and manages localized messages.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Messages {

    private static ResourceBundle messageBundle;

    static{
        loadResourceBundle();
    }

    public static String get(String messageId, String... substitutionValues) {
        if (substitutionValues.length < 1) {
            return messageBundle.getString(messageId);
        } else {
            return String.format(messageBundle.getString(messageId), (Object[]) substitutionValues);
        }
    }

    public static void setLocale(Locale locale) {
        Locale.setDefault(locale);
        loadResourceBundle();
    }

    public static void loadResourceBundle() {
        messageBundle = ResourceBundle.getBundle("org.openmainframeproject.cobolcheck.messages.messages",
                Locale.getDefault());
    }
}
