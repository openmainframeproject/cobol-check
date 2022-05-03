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
