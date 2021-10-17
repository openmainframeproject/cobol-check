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

import com.neopragma.cobolcheck.services.StringHelper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class StringHelperTest implements StringHelper {

    @Test
    public void isBlank_recognizes_a_null_string() {
        assertTrue(isBlank(null));
    }

    @Test
    public void isBlank_recognizes_an_empty_string() {
        assertTrue(isBlank(""));
    }

    @Test
    public void isBlank_recognizes_a_non_empty_string() {
        assertFalse(isBlank("x"));
    }

    @Test
    public void notBlank_recognizes_a_non_empty_string() {
        assertTrue(notBlank("x"));
    }

    @Test
    public void notBlank_recognizes_a_null_string() {
        assertFalse(notBlank(null));
    }

    @Test
    public void notBlank_recognizes_an_empty_string() {
        assertFalse(notBlank(""));
    }

    @Test
    public void defaultIfBlank_does_not_change_original_value_if_present() {
        assertEquals("alpha", defaultIfBlank("alpha", "beta"));
    }

    @Test
    public void defaultIfBlank_replaces_original_value_if_null() {
        assertEquals("beta", defaultIfBlank(null, "beta"));
    }

    @Test
    public void defaultIfBlank_replaces_original_value_if_empty() {
        assertEquals("beta", defaultIfBlank("", "beta"));
    }

    @Test
    public void ifEmptyArray_returns_true_if_array_reference_is_null() {
        assertTrue(isEmptyArray(null));
    }

    @Test
    public void ifEmptyArray_returns_true_if_array_contains_no_entries() {
        assertTrue(isEmptyArray(new String[] {}));
    }

    @Test
    public void ifEmptyArray_returns_false_if_array_contains_any_entries() {
        assertFalse(isEmptyArray(new String[] { "x" }));
    }

    @Test
    public void fixedLength_adjusts_the_length_of_a_short_line() {
        String expected = "12345678901234567890                                                            "
                + System.getProperty("line.separator");
        assertEquals(expected, fixedLength("12345678901234567890"));
    }

    @Test
    public void fixedLength_adjusts_the_length_of_a_long_line() {
        String original = "12345678901234567890                                                            "
                + "text beyond 80 bytes";
        String expected = "12345678901234567890                                                            "
                + System.getProperty("line.separator");
        assertEquals(expected, fixedLength(original));
    }

    @Test
    public void adjustPathString_replaces_unix_file_separator_with_current_system_file_separator() {
        String fileSeparator = System.getProperty("file.separator");
        String expected = "src" + fileSeparator + "main" + fileSeparator + "cobol" + fileSeparator;
        String original = "src/main/cobol/";
        assertEquals(expected, adjustPathString(original));
    }
}
