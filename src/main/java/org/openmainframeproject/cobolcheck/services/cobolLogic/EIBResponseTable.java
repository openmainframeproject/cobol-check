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

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Test cases specify CICS conditions to be set for mocked resources using the condition name. For example:
 *
 * MOCK DATASET 'SOMEFILENAME'
 *     ON READ CONDITION IS NOTFND
 * END-MOCK
 *
 * This class provides a table lookup to get the values for EIBFN, EIBRCODE, and EIBRESP to inject into the program under test.
 *
 * In the example above, we set EIBFN = 0x06, EIBRCODE = 0x81, and EIBRESP = 13.
 *
 * Some conditions are produced by more than one CICS module. In those cases, we use the codes for the most common situation in application programs.
 * In other situations, do not count on the values of EIBFN and EIBRCODE to be relevant. The value of EIBRESP will be consistent.
 *
 * <ul>
 *     <li>INVREQ, DISABLED, IOERR, ISCINVREQ, NOTAUTH, LENGERR, NOTOPEN, NOSPACE, NOTFND can be produced from multiple CICS modules. File Control is assumed (EIBFN - 0x06..)</li>
 *     <li>EOC, IGREQCD, RDATT, SYSIDERR, SESSIONERR, SYSBUSY, SESSBUSY, TERMERR, TERMIDERR, WRBRK can be produced from multiple CICS modules. We assume EIBFN 0x04..</li>
 *     <li>QIDERR can be produced from multiple CICS modules. We assume EIBFN 0x08..</li>
 *     <li>NOSTG can be produced from multiple CICS modules. We assume EIBFN 0x0C..</li>
 *     <li>RESUNAVAIL, CHANNELERR can be produced from multiple CICS modules. We assume EIBFN 0x0E..</li>
 *     <li>UNEXPIN can be produced from multiple CICS modules. We assume EIBFN 0x18..</li>
 * </ul>
 *
 * For INVMPSZ, we do not put anything in byte 3 of EIBRCODE. In a real CICS system, it would have the terminal code. In a unit test there is no terminal.
 *
 * See <a href="https://www.ibm.com/support/knowledgecenter/SSGMCP_5.1.0/com.ibm.cics.ts.applicationprogramming.doc/topics/dfhp4_eibfields.html">IBM Knowledge Center: EIB Fields</a>
 *
 * @author Dave Nicolette (neopragma)
 * @since 1.7
 */
public class EIBResponseTable {

    private static Map<String, EIBResponseCodes> EIBResponsesTable;

    static {
        EIBResponsesTable = new HashMap<>();
        EIBResponsesTable.put("EODS", new EIBResponseCodes((byte) 0x04, (byte) 0x10, 0, 5));
        EIBResponsesTable.put("EOF", new EIBResponseCodes((byte) 0x04, (byte) 0xC1, 0, 4));
        EIBResponsesTable.put("ENDINPT", new EIBResponseCodes((byte) 0x04, (byte) 0xC2, 0, 8));
        EIBResponsesTable.put("SYSIDERR", new EIBResponseCodes((byte) 0x04, (byte) 0xD0, 0, 53));
        EIBResponsesTable.put("SESSIONERR", new EIBResponseCodes((byte) 0x04, (byte) 0xD2, 0, 58));
        EIBResponsesTable.put("SYSBUSY", new EIBResponseCodes((byte) 0x04, (byte) 0xD3, 0, 59));
        EIBResponsesTable.put("SESSBUSY", new EIBResponseCodes((byte) 0x04, (byte) 0xD4, 0, 60));
        EIBResponsesTable.put("NOTALLOC", new EIBResponseCodes((byte) 0x04, (byte) 0xD5, 0, 61));
        EIBResponsesTable.put("LENGERR", new EIBResponseCodes((byte) 0x04, (byte) 0xE1, 0, 22));
        EIBResponsesTable.put("WRBRK", new EIBResponseCodes((byte) 0x04, (byte) 0xE3, 0, 03));
        EIBResponsesTable.put("RDATT", new EIBResponseCodes((byte) 0x04, (byte) 0xE4, 0, 02));
        EIBResponsesTable.put("SIGNAL", new EIBResponseCodes((byte) 0x04, (byte) 0xE5, 0, 24));
        EIBResponsesTable.put("TERMIDERR", new EIBResponseCodes((byte) 0x04, (byte) 0xE6, 0, 11));
        EIBResponsesTable.put("NOPASSBKRD", new EIBResponseCodes((byte) 0x04, (byte) 0xE7, 0, 50));
        EIBResponsesTable.put("NOPASSBKWR", new EIBResponseCodes((byte) 0x04, (byte) 0xE8, 0, 51));
        EIBResponsesTable.put("IGREQCD", new EIBResponseCodes((byte) 0x04, (byte) 0xEA, 0, 57));
        EIBResponsesTable.put("CBIDERR", new EIBResponseCodes((byte) 0x04, (byte) 0xEB, 0, 62));
        EIBResponsesTable.put("PARTNERIDERR", new EIBResponseCodes((byte) 0x04, (byte) 0xEC, 0, 97));
        EIBResponsesTable.put("NETNAMEIDERR", new EIBResponseCodes((byte) 0x04, (byte) 0xED, 0, 99));
        EIBResponsesTable.put("TERMERR", new EIBResponseCodes((byte) 0x04, (byte) 0xF1, 0, 81));
        EIBResponsesTable.put("EOC", new EIBResponseCodes((byte) 0x04, (byte) 0x20, 1, 6));
        EIBResponsesTable.put("INBFMH", new EIBResponseCodes((byte) 0x04, (byte) 0x40, 1, 7));
        EIBResponsesTable.put("NOSTART", new EIBResponseCodes((byte) 0x04, (byte) 0xF6, 3, 10));
        EIBResponsesTable.put("NONVAL", new EIBResponseCodes((byte) 0x04, (byte) 0xF7, 3, 9));
        EIBResponsesTable.put("FILENOTFOUND", new EIBResponseCodes((byte) 0x06, (byte) 0x01, 0, 12));
        EIBResponsesTable.put("ILLOGIC", new EIBResponseCodes((byte) 0x06, (byte) 0x02, 0, 21));
        EIBResponsesTable.put("LOCKED", new EIBResponseCodes((byte) 0x06, (byte) 0x03, 0, 100));
        EIBResponsesTable.put("RECORDBUSY", new EIBResponseCodes((byte) 0x06, (byte) 0x05, 0, 101));
        EIBResponsesTable.put("INVREQ", new EIBResponseCodes((byte) 0x06, (byte) 0x08, 0, 16));
        EIBResponsesTable.put("NOTOPEN", new EIBResponseCodes((byte) 0x06, (byte) 0x0C, 0, 19));
        EIBResponsesTable.put("DISABLED", new EIBResponseCodes((byte) 0x06, (byte) 0x0D, 0, 16));
        EIBResponsesTable.put("ENDFILE", new EIBResponseCodes((byte) 0x06, (byte) 0x0F, 0, 84));
        EIBResponsesTable.put("IOERR", new EIBResponseCodes((byte) 0x06, (byte) 0x80, 0, 17));
        EIBResponsesTable.put("NOTFND", new EIBResponseCodes((byte) 0x06, (byte) 0x81, 0, 13));
        EIBResponsesTable.put("DUPREC", new EIBResponseCodes((byte) 0x06, (byte) 0x82, 0, 14));
        EIBResponsesTable.put("NOSPACE", new EIBResponseCodes((byte) 0x06, (byte) 0x83, 0, 18));
        EIBResponsesTable.put("DUPKEY", new EIBResponseCodes((byte) 0x06, (byte) 0x84, 0, 15));
        EIBResponsesTable.put("SUPPRESSED", new EIBResponseCodes((byte) 0x06, (byte) 0x85, 0, 72));
        EIBResponsesTable.put("LOADING", new EIBResponseCodes((byte) 0x06, (byte) 0x86, 0, 94));
        EIBResponsesTable.put("ISCINVREQ", new EIBResponseCodes((byte) 0x06, (byte) 0xD1, 0, 54));
        EIBResponsesTable.put("NOTAUTH", new EIBResponseCodes((byte) 0x06, (byte) 0xD6, 0, 70));
        EIBResponsesTable.put("LENGERR", new EIBResponseCodes((byte) 0x06, (byte) 0xE1, 0, 22));
        EIBResponsesTable.put("QZERO", new EIBResponseCodes((byte) 0x08, (byte) 0x01, 0, 23));
        EIBResponsesTable.put("QIDERR", new EIBResponseCodes((byte) 0x08, (byte) 0x02, 0, 44));
        EIBResponsesTable.put("QBUSY", new EIBResponseCodes((byte) 0x08, (byte) 0xC0, 0, 25));
        EIBResponsesTable.put("NOSTG", new EIBResponseCodes((byte) 0x0C, (byte) 0xE2, 0, 42));
        EIBResponsesTable.put("PGMIDERR", new EIBResponseCodes((byte) 0x0E, (byte) 0x01, 0, 27));
        EIBResponsesTable.put("RESUNAVAIL", new EIBResponseCodes((byte) 0x0E, (byte) 0xD9, 0, 121));
        EIBResponsesTable.put("CHANNELERR", new EIBResponseCodes((byte) 0x0E, (byte) 0xDA, 0, 122));
        EIBResponsesTable.put("ENDDATA", new EIBResponseCodes((byte) 0x10, (byte) 0x01, 0, 29));
        EIBResponsesTable.put("TRANSIDERR", new EIBResponseCodes((byte) 0x10, (byte) 0x11, 0, 28));
        EIBResponsesTable.put("EXPIRED", new EIBResponseCodes((byte) 0x10, (byte) 0x20, 0, 31));
        EIBResponsesTable.put("USERIDERR", new EIBResponseCodes((byte) 0x10, (byte) 0xD8, 0, 69));
        EIBResponsesTable.put("ENVDEFERR", new EIBResponseCodes((byte) 0x10, (byte) 0xE9, 0, 56));
        EIBResponsesTable.put("ENQBUSY", new EIBResponseCodes((byte) 0x12, (byte) 0x32, 0, 55));
        EIBResponsesTable.put("JIDERR", new EIBResponseCodes((byte) 0x14, (byte) 0x01, 0, 43));
        EIBResponsesTable.put("NOJBUFSP", new EIBResponseCodes((byte) 0x14, (byte) 0x09, 0, 45));
        EIBResponsesTable.put("ROLLEDBACK", new EIBResponseCodes((byte) 0x16, (byte) 0x01, 0, 82));
        EIBResponsesTable.put("RETPAGE", new EIBResponseCodes((byte) 0x18, (byte) 0x02, 0, 32));
        EIBResponsesTable.put("MAPFAIL", new EIBResponseCodes((byte) 0x18, (byte) 0x04, 0, 36));
        EIBResponsesTable.put("INVMPSZ", new EIBResponseCodes((byte) 0x18, (byte) 0x08, 0, 38));
        EIBResponsesTable.put("INVERRTERM", new EIBResponseCodes((byte) 0x18, (byte) 0x20, 0, 37));
        EIBResponsesTable.put("RTESOME", new EIBResponseCodes((byte) 0x18, (byte) 0x40, 0, 34));
        EIBResponsesTable.put("RTEFAIL", new EIBResponseCodes((byte) 0x18, (byte) 0x80, 0, 33));
        EIBResponsesTable.put("PARTNFAIL", new EIBResponseCodes((byte) 0x18, (byte) 0x02, 1, 66));
        EIBResponsesTable.put("INVPARTN", new EIBResponseCodes((byte) 0x18, (byte) 0x04, 1, 65));
        EIBResponsesTable.put("INVPARTNSET", new EIBResponseCodes((byte) 0x18, (byte) 0x08, 1, 64));
        EIBResponsesTable.put("INVLDC", new EIBResponseCodes((byte) 0x18, (byte) 0x10, 1, 41));
        EIBResponsesTable.put("UNEXPIN", new EIBResponseCodes((byte) 0x18, (byte) 0x20, 1, 49));
        EIBResponsesTable.put("INVEXITREQ", new EIBResponseCodes((byte) 0x22, (byte) 0x80, 0, 63));
        EIBResponsesTable.put("NOSPOOL", new EIBResponseCodes((byte) 0x56, (byte) 0x50, 3, 63));
        EIBResponsesTable.put("ALLOCERR", new EIBResponseCodes((byte) 0x56, (byte) 0x55, 3, 85));
        EIBResponsesTable.put("OPENERR", new EIBResponseCodes((byte) 0x56, (byte) 0x57, 3, 87));
        EIBResponsesTable.put("SPOLBUSY", new EIBResponseCodes((byte) 0x56, (byte) 0x58, 3, 88));
        EIBResponsesTable.put("SPOLERR", new EIBResponseCodes((byte) 0x56, (byte) 0x59, 3, 89));
        EIBResponsesTable.put("NODEIDERR", new EIBResponseCodes((byte) 0x56, (byte) 0x5A, 3, 90));
    }

    /**
     * Looks up EIB field values based on a condition name (e.g., NOTFND).
     *
     * When the condition name is not a key in the lookup table, code values of zero are returned.
     * The caller is expected to handle this case. We do not throw from here.
     *
     * @param conditionName
     * @return
     */
    public static EIBResponseCodes lookup(String conditionName) {
        EIBResponseCodes codes = EIBResponsesTable.get(conditionName.toUpperCase(Locale.ROOT));
        if (codes == null) {
            codes = new EIBResponseCodes((byte) 0x00, (byte) 0x00, 0, 0);
        }
        return codes;
    }
}
