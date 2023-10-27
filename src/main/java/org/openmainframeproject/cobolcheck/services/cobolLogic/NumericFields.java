package org.openmainframeproject.cobolcheck.services.cobolLogic;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Messages;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Stores field type information for Data Division items in the program under test so the TestSuiteParser can
 * generate the appropriate Cobol statements for EXPECT specifications.
 *
 * This is populated by Generator for each program to be tested, and consumed by TestSuiteParser.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class NumericFields {

    private Map<String, DataType> fieldTypes;

    public DataType dataTypeOf(String fieldName) {
        argumentCheck(fieldName, "ERR027");
        if (fieldTypes == null) reset();

        // We assume the fieldName has defined a datastructure.
        fieldName=getKeyBasedOnAssumedDataStructure(fieldName);

        return fieldTypes.getOrDefault(fieldName, DataType.ALPHANUMERIC);
    }

    public void setDataTypeOf(String fieldName, DataType dataType) {
        argumentCheck(fieldName, "ERR028");
        argumentCheck(dataType, "ERR029");
        if (fieldTypes == null) reset();
        fieldTypes.put(fieldName, dataType);
    }

    public void reset() {
        fieldTypes = new HashMap<>();
    }

    private void argumentCheck(Object argumentValue, String messageId) {
        if (argumentValue == null ||
                (argumentValue.getClass().getSimpleName().equals("String") && ((String) argumentValue).length() == 0)) {
            throw new PossibleInternalLogicErrorException(
                    Messages.get(messageId)
            );
        }

    }

    /**
     * Based on the given value of the parsed fieldName, we want to make sure we 
     * find the correct key based on the datastructre that were referenced.
     * If we cannot find the key, we return null
     */
    private String getKeyBasedOnAssumedDataStructure(String line) {
        // We will attempt to split the line on any " IN " and " OF " statements, to isolate the names
        // in the referenced data structure.
        String[] nameTokens = line.toUpperCase().split("(?:^|\\W)OF(?:$|\\W)|(?:^|\\W)IN(?:$|\\W)");
        Boolean found=false;
        for (String key : fieldTypes.keySet()) {

            String[] keyParts = key.split(",");

            if (keyParts[0].equalsIgnoreCase(nameTokens[0])) {
                int previousNameIndex = 0;
                int currentSearchIteration = 1;
                found = true;
                while (found && currentSearchIteration < nameTokens.length) {
                    int currentNameIndex = key.toUpperCase().indexOf(nameTokens[currentSearchIteration]);
                    if (currentNameIndex > previousNameIndex) {
                        currentSearchIteration++;
                        previousNameIndex = currentNameIndex;
                    }
                    else
                        found = false;
                }
                if (found) {
                    return key;
                }
            }
        }
        return null;
    }

}
