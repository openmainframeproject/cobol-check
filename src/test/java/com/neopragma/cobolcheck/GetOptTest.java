package com.neopragma.cobolcheck;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class GetOptTest {

    private final String optionSpecsWithLongOptions = "c:l:h --long config-file:,log-level:,help";
    private final String optionSpecsWithOnlyShortOptions = "c:l:h";

    @Test
    public void when_arg_list_is_null_it_does_not_store_the_option_specs() {
        GetOpt getOpt = new GetOpt(null, optionSpecsWithOnlyShortOptions);
        assertFalse(getOpt.isSet("c"), "Option c should not be set");
    }

    @Test
    public void when_short_option_c_is_passed_it_handles_the_argument() {
        GetOpt getOpt = new GetOpt(new String[] {"-c", "arg-for-c"}, optionSpecsWithOnlyShortOptions);
        assertTrue(getOpt.isSet("c"), "Option c should be set");
        assertEquals("arg-for-c", getOpt.getArgument("c"));
    }

//    @Test
//    public void foo() {
//
//        String options = "c:l:h --long config-file:,log-level:,help";
//        String[] args = { "-c", "tempconfig.properties", "-l", "info"};
//        GetOpt getOpt = new GetOpt(args, options);
//    }
}
