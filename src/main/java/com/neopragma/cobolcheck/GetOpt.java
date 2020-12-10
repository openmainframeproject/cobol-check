package com.neopragma.cobolcheck;

/**
 * Process command line options.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class GetOpt {
    /**
     * Parse command-line options using the optionsString to validate.
     *
     * @param args - String[] - options from the command line.
     * @param optionsString - String - Bash-style options specification,
     *                      e.g. "abc:d: --long alpha,bravo,charlie:,delta:"
     */
    public GetOpt(String[] args, String optionsString) {

        if (args == null || args.length < 1) {
            return;
        }
//        storeOptionSettings(optionsString);
//        processCommandLineArgumentArray(args);
    }

}
