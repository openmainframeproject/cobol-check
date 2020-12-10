package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;

import java.util.HashMap;
import java.util.Map;

/**
 * Process command line options.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class GetOpt {

    private Map<String, String> longToShort;
    private Map<String, Option> optionSettings;

    /**
     * Parse command-line options using the optionsString to validate.
     *
     * @param args - String[] - options from the command line.
     * @param optionsString - String - Bash-style options specification,
     *                      e.g. "abc:d: --long alpha,bravo,charlie:,delta:"
     */
    public GetOpt(String[] args, String optionsString) {
        longToShort = new HashMap();
        optionSettings = new HashMap();

        if (args == null || args.length < 1) {
            return;
        }
        storeOptionSettings(optionsString);

        processCommandLineArgumentArray(args);
    }

    private void storeOptionSettings(String optionsString) {
        String[] optionSpecs = optionsString.split(" ");
        if (optionSpecs == null || optionSpecs.length < 1) {
            throw new PossibleInternalLogicErrorException("x");
        }

        boolean longOptionsWereSpecified = optionSpecs.length > 1 ? true : false;

        if (longOptionsWereSpecified) {
            if (!optionSpecs[1].equals("--long")) {
                throw new PossibleInternalLogicErrorException("x");
            }
            if (optionSpecs.length < 3) {
                throw new PossibleInternalLogicErrorException("x");
            }
        }

        String shortOptionSpecs = optionSpecs[0];
        String[] longOptionSpecs = longOptionsWereSpecified ? optionSpecs[2].split(",") : null;

        int longIndex = 0;
        for (int shortIndex = 0; shortIndex < shortOptionSpecs.length(); shortIndex++) {
            String shortOption = String.valueOf(shortOptionSpecs.charAt(shortIndex));
            String longOption = Constants.EMPTY_STRING;
            if (shortIndex < shortOptionSpecs.length()-1 && shortOptionSpecs.charAt(shortIndex+1) == ':') {
                longOption = longOptionsWereSpecified
                        ? longOptionSpecs[longIndex].substring(0, longOptionSpecs[longIndex].length() - 1)
                        : Constants.EMPTY_STRING;
                optionSettings.put(shortOption, new Option(true));
                shortIndex += 1;
            } else {
                longOption = longOptionsWereSpecified
                        ? longOptionSpecs[longIndex]
                        : Constants.EMPTY_STRING;
                optionSettings.put(shortOption, new Option(false));
            }
            if (longOptionsWereSpecified) {
                longToShort.put(longOption, shortOption);
                longIndex += 1;
            }
        }
    }

    private void processCommandLineArgumentArray(String[] args) {

    }

    public boolean isSet(String optionKey) {
        if (optionKey == null) {
            throw new PossibleInternalLogicErrorException("x");
        }
        if (optionKey.length() == 1) {
            return optionSettings.containsKey(optionKey);
        } else {
            return optionSettings.containsKey(longToShort.get(optionKey));
        }
    }

    public String getArgument(String optionKey) {
        return Constants.EMPTY_STRING;
    }

    class Option {
        public boolean hasArgument;
        public String argument = Constants.EMPTY_STRING;
        public Option(boolean hasArgument) {
            this.hasArgument = hasArgument;
        }
    }
}
