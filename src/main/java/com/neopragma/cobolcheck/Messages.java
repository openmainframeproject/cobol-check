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
import java.util.ResourceBundle;

/**
 * Loads and manages localized messages.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Messages {

    private ResourceBundle messageBundle;

    public Messages() {
        loadResourceBundle();
    }

    public String get(String messageId, String... substitutionValues) {
        if (substitutionValues.length < 1) {
            return messageBundle.getString(messageId);
        } else {
            return String.format(messageBundle.getString(messageId), (Object[]) substitutionValues);
        }
    }

    public void setLocale(Locale locale) {
        Locale.setDefault(locale);
        loadResourceBundle();
    }

    public void loadResourceBundle() {
        messageBundle = ResourceBundle.getBundle(this.getClass().getPackageName() + ".messages.messages",
                Locale.getDefault());
    }
}
