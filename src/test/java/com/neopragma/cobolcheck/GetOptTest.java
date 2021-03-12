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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class GetOptTest {

    private static final String optionSpec = "c:l:p:t:v:h --long config-file:,log-level:,programs:,tests:,version:,help";
    private final Config config = new Config(new Messages());
    private final Messages messages = config.getMessages();

    @BeforeEach
    public void commonSetup() {
        config.load("testconfig.properties");
    }

    @Test
    public void it_throws_when_option_specification_string_is_null() {
        Throwable ex = assertThrows(PossibleInternalLogicErrorException.class, () -> new GetOpt(new String[] { "--help" }, null, config));
        assertEquals("ERR005: Command line option specification string passed to GetOpt was null.",
                ex.getMessage());
    }

    @Test
    public void it_throws_when_required_argument_is_not_present() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> new GetOpt(new String[] { "-c", "-l", "warn" }, optionSpec, config));
        assertEquals("ERR004: Expecting an argument for command line option <-c> but got <-l>.",
                ex.getMessage());
    }

    @Test
    public void it_throws_when_an_option_is_not_present_when_expected() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> new GetOpt(new String[] { "-l", "warn", "c" }, optionSpec, config));
        assertEquals("ERR006: Expecting a command line option but got <c>.",
                ex.getMessage());
    }

    @Test
    public void it_does_not_throw_while_processing_a_valid_short_option_specification_string() {
        GetOpt getOpt = new GetOpt(new String[] { "--h" }, optionSpec,config);
    }

    @Test
    public void it_does_not_throw_while_processing_a_valid_long_option_specification_string() {
        GetOpt getOpt = new GetOpt(new String[] { "--help" }, optionSpec, config);
    }

    @Test
    public void it_correctly_marks_a_selected_option_as_set() {
        GetOpt getOpt = new GetOpt(new String[] { "--log-level", "info" }, optionSpec, config);
        assertTrue(getOpt.isSet("log-level"), "log-level is expected to be set");
    }

    @Test
    public void it_correctly_stores_the_argument_that_goes_with_an_option() {
        GetOpt getOpt = new GetOpt(new String[] { "--log-level", "info" },
                optionSpec, config);
        assertEquals("info", getOpt.getValueFor("log-level"));
    }

    @Test
    public void it_handles_option_p_followed_by_one_full_program_name() {
        String expected = "src" + Constants.FILE_SEPARATOR + "main" + Constants.FILE_SEPARATOR
                + "cobol" + Constants.FILE_SEPARATOR + "PROG1";
        GetOpt getOpt = new GetOpt(new String[] { "-p", "PROG1", "-l", "err" },
                optionSpec, config);
        assertEquals(expected, getOpt.getValueFor("programs"));
    }

    @Test
    public void it_handles_option_programs_followed_by_two_full_program_names() {
        String expected = "src" + Constants.FILE_SEPARATOR + "main" + Constants.FILE_SEPARATOR
                        + "cobol" + Constants.FILE_SEPARATOR + "PROG1"
                        + Constants.COLON
                        + "src" + Constants.FILE_SEPARATOR + "main" + Constants.FILE_SEPARATOR
                        + "cobol" + Constants.FILE_SEPARATOR + "PROG2";
        GetOpt getOpt = new GetOpt(new String[] { "--programs", "PROG1", "PROG2", "-l", "err" },
                optionSpec, config);
        assertEquals(expected, getOpt.getValueFor("programs"));
    }

    @Test
    public void it_throws_when_an_undefined_option_is_passed() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> {
            GetOpt getOpt = new GetOpt(new String[] { "-f" },
                    optionSpec, config);
        });
    }

    @Test
    public void it_throws_when_no_value_is_passed_for_the_only_argument_and_it_requires_a_value() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> {
            GetOpt getOpt = new GetOpt(new String[] { "-c" },
                    optionSpec, config);
        });
    }

    @Test
    public void it_throws_when_no_value_is_passed_for_the_last_argument_and_it_requires_a_value() {
        Throwable ex = assertThrows(CommandLineArgumentException.class, () -> {
            GetOpt getOpt = new GetOpt(new String[] { "-l", "info", "-c" },
                    optionSpec, config);
        });
    }

}
