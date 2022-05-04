package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.services.Messages;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MessagesTest {

    @Test
    public void it_retrieves_a_message_with_substitution_values() {
        assertEquals("ERR001: testSuite is null on entry to Generator.runSuite() method.",
                Messages.get("ERR001", "testSuite", "Generator.runSuite()"));
    }


}
