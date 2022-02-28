package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.services.platform.Platform;
import org.openmainframeproject.cobolcheck.services.platform.PlatformLookup;
import org.junit.jupiter.api.Test;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class PlatformLookupTest {

    @Test
    public void it_identifies_linux() {
        assertEquals(Platform.LINUX, PlatformLookup.get("Linuxandotherstuff"));
    }

    @Test
    public void it_identifies_osx() {
        assertEquals(Platform.OSX, PlatformLookup.get("MacOSXandotherstuff"));
    }

    @Test
    public void it_identifies_windows() {
        assertEquals(Platform.WINDOWS, PlatformLookup.get("Windows 10 etc"));
    }

    @Test
    public void it_identifies_zos() {
        assertEquals(Platform.ZOS, PlatformLookup.get("ZOSandotherstuff"));
    }

    @Test
    public void it_defaults_to_generic_unix() {
        assertEquals(Platform.UNIX, PlatformLookup.get("Rupelstiltskin"));
    }

    @Test
    public void it_identifies_the_current_system() {
        Platform thisPlatform = PlatformLookup.get();
        String osName = System.getProperty("os.name").substring(0,3).toLowerCase(Locale.ROOT);
        switch (osName) {
            case "lin" : assertEquals(Platform.LINUX, thisPlatform);
                         break;
            case "osx" : assertEquals(Platform.OSX, thisPlatform);
                         break;
            case "win" : assertEquals(Platform.WINDOWS, thisPlatform);
                         break;
            case "zos" : assertEquals(Platform.ZOS, thisPlatform);
                         break;
            default     : assertEquals(Platform.UNIX, thisPlatform);
        }

    }
}
