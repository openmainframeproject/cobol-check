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

import com.neopragma.cobolcheck.services.Messages;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MessagesTest {

    private Messages messages;

    @BeforeEach
    public void commonSetup() {
        messages = new Messages();
    }

    @Test
    public void it_retrieves_a_message_with_substitution_values() {
        assertEquals("ERR001: testSuite is null on entry to Generator.runSuite() method.",
                messages.get("ERR001", "testSuite", "Generator.runSuite()"));
    }

//    @Test
//    public void it_retrieves_a_message_with_substitution_values_in_a_different_order() {
//        Locale.setDefault(new Locale("ja", "JP"));
//        messages = new Messages();
//        assertEquals("ERR001: Generator.runSuite()メソッドへのエントリー時にtestSuiteがnullです。",
//                messages.get("ERR001", "testSuite", "Generator.runSuite()"));
//    }
}
