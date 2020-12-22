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

/**
 * Current version of the product.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class Version {
    private static final Integer MAJOR = 0;
    private static final Integer MINOR = 0;
    private static final String PATCH = "1";

    public static String current() {
        return String.format("Version: %s.%s.%s", MAJOR.toString(), MINOR.toString(), PATCH);
    }
}
