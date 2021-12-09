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

import com.neopragma.cobolcheck.services.StringTuple;
import com.neopragma.cobolcheck.services.Tuple;
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
