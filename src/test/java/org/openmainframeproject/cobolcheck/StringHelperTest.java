package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class StringHelperTest{

    @Test
    public void isBlank_recognizes_a_null_string() {
        assertTrue(StringHelper.isBlank(null));
    }

    @Test
    public void isBlank_recognizes_an_empty_string() {
        assertTrue(StringHelper.isBlank(""));
    }

    @Test
    public void isBlank_recognizes_a_non_empty_string() {
        assertFalse(StringHelper.isBlank("x"));
    }

    @Test
    public void notBlank_recognizes_a_non_empty_string() {
        assertTrue(StringHelper.notBlank("x"));
    }

    @Test
    public void notBlank_recognizes_a_null_string() {
        assertFalse(StringHelper.notBlank(null));
    }

    @Test
    public void notBlank_recognizes_an_empty_string() {
        assertFalse(StringHelper.notBlank(""));
    }

    @Test
    public void defaultIfBlank_does_not_change_original_value_if_present() {
        assertEquals("alpha", StringHelper.defaultIfBlank("alpha", "beta"));
    }

    @Test
    public void defaultIfBlank_replaces_original_value_if_null() {
        assertEquals("beta", StringHelper.defaultIfBlank(null, "beta"));
    }

    @Test
    public void defaultIfBlank_replaces_original_value_if_empty() {
        assertEquals("beta", StringHelper.defaultIfBlank("", "beta"));
    }

    @Test
    public void ifEmptyArray_returns_true_if_array_reference_is_null() {
        assertTrue(StringHelper.isEmptyArray(null));
    }

    @Test
    public void ifEmptyArray_returns_true_if_array_contains_no_entries() {
        assertTrue(StringHelper.isEmptyArray(new String[] {}));
    }

    @Test
    public void ifEmptyArray_returns_false_if_array_contains_any_entries() {
        assertFalse(StringHelper.isEmptyArray(new String[] { "x" }));
    }

    @Test
    public void fixedLength_adjusts_the_length_of_a_short_line() {
        String expected = "12345678901234567890                                                            "
                + System.getProperty("line.separator");
        assertEquals(expected, StringHelper.fixedLength("12345678901234567890"));
    }

    @Test
    public void fixedLength_adjusts_the_length_of_a_long_line() {
        String original = "12345678901234567890                                                            "
                + "text beyond 80 bytes";
        String expected = "12345678901234567890                                                            "
                + System.getProperty("line.separator");
        assertEquals(expected, StringHelper.fixedLength(original));
    }

    @Test
    public void adjustPathString_replaces_unix_file_separator_with_current_system_file_separator() {
        String fileSeparator = System.getProperty("file.separator");
        String expected = "src" + fileSeparator + "main" + fileSeparator + "cobol" + fileSeparator;
        String original = "src/main/cobol/";
        assertEquals(expected, StringHelper.adjustPathString(original));
    }
}
