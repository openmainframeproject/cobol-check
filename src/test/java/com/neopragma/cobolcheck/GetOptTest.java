package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class GetOptTest {

    private final String optionSpecsWithLongOptions = "c:l:h --long config-file:,log-level:,help";
    private final String optionSpecsWithOnlyShortOptions = "c:l:h";

    @Test
    public void it_does_not_throw_while_processing_a_valid_short_option_specification_string() {
        GetOpt getOpt = new GetOpt(new String[] { "--h" }, optionSpecsWithOnlyShortOptions);
    }

    @Test
    public void it_does_not_throw_while_processing_a_valid_long_option_specification_string() {
        GetOpt getOpt = new GetOpt(new String[] { "--help" }, optionSpecsWithLongOptions);
    }

    @Test
    public void it_correctly_marks_a_selected_option_as_set() {
        GetOpt getOpt = new GetOpt(new String[] { "--log-level", "info" }, optionSpecsWithLongOptions);
        assertTrue(getOpt.isSet("log-level"), "log-level is expected to be set");
    }

    @Test
    public void it_correctly_stores_the_argument_that_goes_with_an_option() {
        GetOpt getOpt = new GetOpt(new String[] { "--log-level", "info" }, optionSpecsWithLongOptions);
        assertEquals("info", getOpt.getValueFor("log-level"));
    }

}
