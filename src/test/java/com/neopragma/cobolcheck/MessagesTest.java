package com.neopragma.cobolcheck;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Locale;

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

    @Test
    public void it_retrieves_a_message_with_substitution_values_in_a_different_order() {
        Locale.setDefault(new Locale("ja", "JP"));
        messages = new Messages();
        assertEquals("ERR001: Generator.runSuite()メソッドへのエントリー時にtestSuiteがnullです。",
                messages.get("ERR001", "testSuite", "Generator.runSuite()"));
    }
}
