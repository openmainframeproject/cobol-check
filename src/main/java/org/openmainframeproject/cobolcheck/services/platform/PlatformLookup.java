package org.openmainframeproject.cobolcheck.services.platform;

import org.openmainframeproject.cobolcheck.services.Constants;

import java.util.Locale;

/**
 * See what platform we're running on so we can select the appropriate script to compile and run tests.
 * Default is generic UNIX.
 *
 * @author Dave Nicolette
 * @since 1.5
 */
public class PlatformLookup {

    public static Platform get() {
        return get(System.getProperty("os.name"));
    }

    public static Platform get(String osNameValue) {
        String value = osNameValue == null ? Constants.EMPTY_STRING : osNameValue.toLowerCase(Locale.ROOT);
        if (value.startsWith("linux"))   return Platform.LINUX;
        if (value.startsWith("macosx"))  return Platform.OSX;
        if (value.startsWith("windows")) return Platform.WINDOWS;
        if (value.startsWith("z/os")) return Platform.ZOS;
        return Platform.UNIX;
    }
}
