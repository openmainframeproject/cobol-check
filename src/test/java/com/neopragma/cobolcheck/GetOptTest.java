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

import com.neopragma.cobolcheck.exceptions.CommandLineArgumentException;
import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class GetOptTest {

    private final String optionSpecsWithLongOptions = "c:l:h --long config-file:,log-level:,help";
    private final String optionSpecsWithOnlyShortOptions = "c:l:h";

    @Test
    public void it_throws_when_option_specification_string_is_null() {
        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> new GetOpt(new String[] { "--help" }, null, new Messages()));
        assertEquals("ERR005: Command line option specification string passed to GetOpt was null.",
                ex.getMessage());
    }

    @Test
    public void it_throws_when_required_argument_is_not_present() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> new GetOpt(new String[] { "-c", "-l", "warn" }, optionSpecsWithOnlyShortOptions, new Messages()));
        assertEquals("ERR004: Expecting an argument for command line option <-c> but got <-l>.",
                ex.getMessage());
    }

    @Test
    public void it_throws_when_an_option_is_not_present_when_expected() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> new GetOpt(new String[] { "-l", "warn", "c" }, optionSpecsWithOnlyShortOptions, new Messages()));
        assertEquals("ERR006: Expecting a command line option but got <c>.",
                ex.getMessage());
    }

    @Test
    public void it_does_not_throw_while_processing_a_valid_short_option_specification_string() {
        GetOpt getOpt = new GetOpt(new String[] { "--h" }, optionSpecsWithOnlyShortOptions, new Messages());
    }

    @Test
    public void it_does_not_throw_while_processing_a_valid_long_option_specification_string() {
        GetOpt getOpt = new GetOpt(new String[] { "--help" }, optionSpecsWithLongOptions, new Messages());
    }

    @Test
    public void it_correctly_marks_a_selected_option_as_set() {
        GetOpt getOpt = new GetOpt(new String[] { "--log-level", "info" }, optionSpecsWithLongOptions, new Messages());
        assertTrue(getOpt.isSet("log-level"), "log-level is expected to be set");
    }

    @Test
    public void it_correctly_stores_the_argument_that_goes_with_an_option() {
        GetOpt getOpt = new GetOpt(new String[] { "--log-level", "info" },
                optionSpecsWithLongOptions, new Messages());
        assertEquals("info", getOpt.getValueFor("log-level"));
    }

    @Test
    public void it_logs_and_throws_when_an_undefined_option_is_detected() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> {
            GetOpt getOpt = new GetOpt(new String[] { "-f"},
                    optionSpecsWithOnlyShortOptions, new Messages());
        });
    }

}
