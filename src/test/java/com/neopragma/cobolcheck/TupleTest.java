package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TupleTest {

    @Test
    public void it_handles_two_strings() {
        Tuple<String, String> tuple = new Tuple("A", "B");
        assertEquals("A", tuple.first);
        assertEquals("B", tuple.second);
    }

    @Test
    public void it_handles_two_integers() {
        Tuple<Integer, Integer> tuple = new Tuple(0, 100);
        assertEquals(0, tuple.first);
        assertEquals(100, tuple.second);
    }

    @Test
    public void it_handles_two_arbitrary_types() {
        Tuple<Class, List<Boolean>> tuple = new Tuple(this.getClass(), List.of(true, false));
        assertEquals(this.getClass(), tuple.first);
        assertEquals(false, tuple.second.get(1));
    }
}
