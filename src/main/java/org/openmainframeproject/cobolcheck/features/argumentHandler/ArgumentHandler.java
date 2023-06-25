package org.openmainframeproject.cobolcheck.features.argumentHandler;

import org.openmainframeproject.cobolcheck.exceptions.CommandLineArgumentException;
import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Config;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.services.Messages;
import org.openmainframeproject.cobolcheck.services.StringHelper;
import org.openmainframeproject.cobolcheck.services.log.Log;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Process command line options.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class ArgumentHandler {

    private Map<OptionKey, OptionValue> options;
    private static final String LONG_OPT_PREFIX = "--";
    private static final String SHORT_OPT_PREFIX = "-";
    private static final String LONG_OPT_KEYWORD = "--long";
    private static final char ARGUMENT_REQUIRED_INDICATOR = ':';
    private static final List<String> canTakeMultipleArguments =
            Arrays.asList("t", "tests", "p", "programs");

    /**
     * Parse command-line options using the optionsString to validate.
     *
     * @param args - String[] - options from the command line.
     * @param optionsString - String - Bash-style options specification,
     *                      e.g. "abc:d: --long alpha,bravo,charlie:,delta:"
     */
    public ArgumentHandler(String[] args, String optionsString) {
        options = new HashMap<>();
        Log.debug("Running with arguments: " + Arrays.toString(args));
        if (StringHelper.isEmptyArray(args))
            return;

        storeOptionSettings(optionsString);
        processCommandLineArgumentArray(args);
    }

    public String getValueFor(String key) {
        OptionValue optionValue = lookupOption(key);
        return optionValue == null ? Constants.EMPTY_STRING : optionValue.argumentValue;
    }

    public boolean isSet(String key) {
        OptionValue optionValue = lookupOption(key);
        return optionValue != null && optionValue.isSet;
    }

    /**
     * Convert an option specification string into a map of option keys and values.
     *
     * @param optionsString (String) - looks like "a:bc: --long alpha:,beta,charlie:"
     */
    private void storeOptionSettings(String optionsString) {
        if (StringHelper.isBlank(optionsString))
            throw new PossibleInternalLogicErrorException(Messages.get("ERR005"));

        // "a:bg: --long alpha:,beta,gamma:" -> [0] "a:bg:", [1] "--long", [2] "alpha:,beta,gamma:"
        String[] optionSpecs = optionsString.split(Constants.SPACE);
        boolean longOptionsWereSpecified = optionSpecs.length > 2 && optionSpecs[1].equals(LONG_OPT_KEYWORD);
        String[] longOptionSpecs = new String[]{ Constants.EMPTY_STRING };
        if (longOptionsWereSpecified) longOptionSpecs = optionSpecs[2].split(Constants.COMMA);

        String shortOptions = optionSpecs[0];
        int optionOffset = 0;
        int optionIndex = 0;
        while (optionOffset < shortOptions.length()) {
            OptionKey optionKey = new OptionKey();
            optionKey.shortKey = String.valueOf(shortOptions.charAt(optionOffset));
            optionKey.longKey = longOptionsWereSpecified ? longOptionSpecs[optionIndex] : Constants.EMPTY_STRING;
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
        boolean multipleArgumentsPossible = false;
        boolean atLeastOneArgumentWasPassed = false;
        boolean processingProgramNames = false;
        OptionValue optionValue = new OptionValue();
        String lastOption = Constants.EMPTY_STRING;
        for (String argValue : args) {
            if (isKey(argValue)) {
                if (!atLeastOneArgumentWasPassed && expectValueNext) throw new CommandLineArgumentException(
                        Messages.get("ERR004", lastOption, argValue)
                );
                multipleArgumentsPossible = false;
                atLeastOneArgumentWasPassed = false;
                optionValue = lookupOption(stripPrefix(argValue));
                if (optionValue == null) {
                    throw new CommandLineArgumentException(
                            Messages.get("ERR025", argValue)
                    );
                }
                optionValue.isSet = true;
                expectValueNext = optionValue.hasArgument;
               lastOption = argValue;
                if (canTakeMultipleArguments.contains(stripPrefix(argValue))) {
                    multipleArgumentsPossible = true;
                }
            } else {
                atLeastOneArgumentWasPassed = true;
                if (multipleArgumentsPossible) {
                    if (optionValue.argumentValue.length() > 0) {
                        optionValue.argumentValue += ":";
                    }
                    optionValue.argumentValue += argValue;
                } else {
                    if (!expectValueNext) throw new CommandLineArgumentException(
                            Messages.get("ERR006", argValue)
                    );
                    optionValue.argumentValue = argValue;
                    expectValueNext = false;
                }
            }
        }
        if (expectValueNext && !atLeastOneArgumentWasPassed) throw new CommandLineArgumentException(
                Messages.get("ERR004", lastOption, "(none)")
        );
    }

    public void loadArgProgramPaths(){
        String applicationSourceDirectory = Config.getApplicationSourceDirectoryPathString();
        if(options.isEmpty()){
            // return error when no program is passed
            throw new PossibleInternalLogicErrorException(Messages.get("ERR005"));
        }
        for (OptionKey optionKey : options.keySet()) {
            if (optionKey.shortKey.equals(Constants.PROGRAMS_OPTION) || optionKey.longKey.equals(Constants.PROGRAMS_OPTION)) {
                String programArgs = options.get(optionKey).argumentValue;
                String newValue = "";
                for (String program : programArgs.split(Constants.COLON)){
                    newValue += StringHelper.adjustPathString(applicationSourceDirectory +
                            Constants.FILE_SEPARATOR + program);
                    newValue += "|";
                }
                options.get(optionKey).argumentValue = newValue.substring(0, newValue.length()-1);
            }
        }
    }

    private OptionValue lookupOption(String requestedOption) {
        for (OptionKey optionKey : options.keySet()) {
            if (optionKey.shortKey.equals(requestedOption) || optionKey.longKey.equals(requestedOption)) {
                return options.get(optionKey);
            }
        }
        return null;
    }

    private boolean isKey(String argValue) {
        if (argValue.startsWith(SHORT_OPT_PREFIX) || argValue.startsWith(LONG_OPT_PREFIX)) {
            if (getValueFor(argValue) != null) {
                return true;
            }
        }
        return false;
    }

    private String stripPrefix(String argValue) {
        int offset = argValue.startsWith(LONG_OPT_PREFIX) ? 2 : 1;
        return argValue.substring(offset);
    }

    static class OptionKey {
        public String shortKey = Constants.EMPTY_STRING;
        public String longKey = Constants.EMPTY_STRING;
    }

    static class OptionValue {
        public boolean hasArgument = false;
        public boolean isSet = false;
        public String argumentValue = Constants.EMPTY_STRING;
    }

}
