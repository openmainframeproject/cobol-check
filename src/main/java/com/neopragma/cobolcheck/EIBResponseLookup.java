package com.neopragma.cobolcheck;

import java.util.HashMap;
import java.util.Map;

/**
 * Test cases specify CICS conditions to be set for mocked resources using the condition name.
 * This class provides a table lookup to get the values for EIBFN, EIBRCODE, and EIBRESP to inject into the program under test.
 *
 * @author Dave Nicolette (neopragma)
 * @since 1.7
 */
public class EIBResponseLookup {

    private static Map<String, EIBResponseCodes> EIBResponsesTable;

    static {
        EIBResponsesTable = new HashMap<>();
        EIBResponsesTable.put("INVREQ", new EIBResponseCodes((byte) 0x02, (byte) 0xE0, 16));
        EIBResponsesTable.put("EODS", new EIBResponseCodes((byte) 0x04, (byte) 0x10, 5));
        EIBResponsesTable.put("EOF", new EIBResponseCodes((byte) 0x04, (byte) 0xC1, 4));
        EIBResponsesTable.put("ENDINPT", new EIBResponseCodes((byte) 0x04, (byte) 0xC2, 8));
        EIBResponsesTable.put("SYSIDERR", new EIBResponseCodes((byte) 0x04, (byte) 0xD0, 53));
        EIBResponsesTable.put("SESSIONERR", new EIBResponseCodes((byte) 0x04, (byte) 0xD2, 58));
        EIBResponsesTable.put("SYSBUSY", new EIBResponseCodes((byte) 0x04, (byte) 0xD3, 59));
        EIBResponsesTable.put("SESSBUSY", new EIBResponseCodes((byte) 0x04, (byte) 0xD4, 60));
        EIBResponsesTable.put("NOTALLOC", new EIBResponseCodes((byte) 0x04, (byte) 0xD5, 61));
        EIBResponsesTable.put("NOTFND", new EIBResponseCodes((byte) 0x06, (byte) 0x81, 13));
    }

    public static EIBResponseCodes lookup(String conditionName) {
        return EIBResponsesTable.get(conditionName);
    }
}
