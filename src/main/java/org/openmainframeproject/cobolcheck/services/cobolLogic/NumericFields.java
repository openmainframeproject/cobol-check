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
