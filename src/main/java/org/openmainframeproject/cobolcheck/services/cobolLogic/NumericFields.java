package org.openmainframeproject.cobolcheck.services.cobolLogic;

import org.openmainframeproject.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import org.openmainframeproject.cobolcheck.services.Messages;

import java.util.HashMap;
import java.util.Map;

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

}
