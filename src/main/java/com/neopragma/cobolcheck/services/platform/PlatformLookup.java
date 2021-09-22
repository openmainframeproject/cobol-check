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
package com.neopragma.cobolcheck.services.platform;

import com.neopragma.cobolcheck.services.Constants;
import com.neopragma.cobolcheck.services.platform.Platform;

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
        if (value.startsWith("zos"))     return Platform.ZOS;
        return Platform.UNIX;
    }
}
