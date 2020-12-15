package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TupleTest {

    @Test
    public void it_handles_two_strings() {
        Tuple<String, String> tuple = new StringTuple("A", "B");
        assertEquals("A", tuple.getFirst());
        assertEquals("B", tuple.getSecond());
    }

    @Test
    public void it_knows_when_it_is_empty() {
        Tuple<String, String> tuple = new StringTuple(null, null);
        assertTrue(tuple.isEmpty());
    }
}
