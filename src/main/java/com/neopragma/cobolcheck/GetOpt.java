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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Process command line options.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class GetOpt implements Constants, StringHelper {

    private Map<OptionKey, OptionValue> options;
    private static final String LONG_OPT_PREFIX = "--";
    private static final String SHORT_OPT_PREFIX = "-";
    private static final String LONG_OPT_KEYWORD = "--long";
    private static final char ARGUMENT_REQUIRED_INDICATOR = ':';

    private Messages messages;

    /**
     * Parse command-line options using the optionsString to validate.
     *
     * @param args - String[] - options from the command line.
     * @param optionsString - String - Bash-style options specification,
     *                      e.g. "abc:d: --long alpha,bravo,charlie:,delta:"
     */
    public GetOpt(String[] args, String optionsString, Messages messages) {
        if (isEmptyArray(args)) return;
        this.messages = messages;
        storeOptionSettings(optionsString);
        processCommandLineArgumentArray(args);
    }

    public String getValueFor(String key) {
        return Objects.requireNonNull(lookupOption(key)).argumentValue;
    }

    public boolean isSet(String key) {
        return Objects.requireNonNull(lookupOption(key)).isSet;
    }

    /**
     * Convert an option specification string into a map of option keys and values.
     *
     * @param optionsString (String) - looks like "a:bc: --long alpha:,beta,charlie:"
     */
    private void storeOptionSettings(String optionsString) {
        if (isBlank(optionsString))
            throw new PossibleInternalLogicErrorException(messages.get("ERR005"));

        options = new HashMap<>();

        // "a:bg: --long alpha:,beta,gamma:" -> [0] "a:bg:", [1] "--long", [2] "alpha:,beta,gamma:"
        String[] optionSpecs = optionsString.split(SPACE);
        boolean longOptionsWereSpecified = optionSpecs.length > 2 && optionSpecs[1].equals(LONG_OPT_KEYWORD);
        String[] longOptionSpecs = new String[]{ EMPTY_STRING };
        if (longOptionsWereSpecified) longOptionSpecs = optionSpecs[2].split(COMMA);

        String shortOptions = optionSpecs[0];
        int optionOffset = 0;
        int optionIndex = 0;
        while (optionOffset < shortOptions.length()) {
            OptionKey optionKey = new OptionKey();
            optionKey.shortKey = String.valueOf(shortOptions.charAt(optionOffset));
            optionKey.longKey = longOptionsWereSpecified ? longOptionSpecs[optionIndex] : EMPTY_STRING;
            OptionValue optionValue = new OptionValue();
            if (optionOffset+1 < shortOptions.length() && shortOptions.charAt(optionOffset+1) == ARGUMENT_REQUIRED_INDICATOR) {
                optionValue.hasArgument = true;
                optionOffset += 2;
                optionKey.longKey = longOptionsWereSpecified
                        ? optionKey.longKey.substring(0, optionKey.longKey.length()-1)
                        : optionKey.longKey;
            } else {
                optionOffset += 1;
            }
            options.put(optionKey, optionValue);
            optionIndex += 1;
        }
    }

    private void processCommandLineArgumentArray(String[] args) {
        boolean expectValueNext = false;
        OptionValue optionValue = new OptionValue();
        String lastOption = EMPTY_STRING;
        for (String argValue : args) {
            if (isKey(argValue)) {
                if (expectValueNext) throw new CommandLineArgumentException(
                        messages.get("ERR004", lastOption, argValue)
                );
                optionValue = lookupOption(stripPrefix(argValue));
                optionValue.isSet = true;
                expectValueNext = optionValue.hasArgument;
                lastOption = argValue;
            } else {
                if (!expectValueNext) throw new CommandLineArgumentException(
                        messages.get("ERR006", argValue)
                );
                optionValue.argumentValue = argValue;
                expectValueNext = false;
            }
        }
    }

    private OptionValue lookupOption(String requestedOption) {
        for (OptionKey optionKey : options.keySet()) {
            if (optionKey.shortKey.equals(requestedOption) || optionKey.longKey.equals(requestedOption)) {
                return options.get(optionKey);
            }
        }
        return null; //TODO improve this
    }

    private boolean isKey(String argValue) {
        return argValue.startsWith(SHORT_OPT_PREFIX) || argValue.startsWith(LONG_OPT_PREFIX);
    }

    private String stripPrefix(String argValue) {
        int offset = argValue.startsWith(LONG_OPT_PREFIX) ? 2 : 1;
        return argValue.substring(offset);
    }

    static class OptionKey {
        public String shortKey = EMPTY_STRING;
        public String longKey = EMPTY_STRING;
    }

    static class OptionValue {
        public boolean hasArgument = false;
        public boolean isSet = false;
        public String argumentValue = EMPTY_STRING;
    }

}
