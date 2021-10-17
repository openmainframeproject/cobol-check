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
package com.neopragma.cobolcheck.services.cobolLogic;

/**
 * CICS EIB values for EIBFN, EIBRCODE, and EIBRESP. This data structure describes the value returned from a Map lookup on EIBValues where the condition name is the key. For example:
 *
 * key = "NOTFND"
 * value =
 *     EIBFN = 0x06 (this goes into the first byte of the EIBFN field)
 *     EIBRCODE = 0x81 (this goes into a byte of the EIBRCODE field indicated by EIBRCODEOffset, relative to zero)
 *     EIBRESP = 13
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public class EIBResponseCodes {
    private final byte EIBFN;
    private final byte EIBRCODE;
    private final int EIBRCODEOffset;
    private final int EIBRESP;

    public EIBResponseCodes(byte EIBFN, byte EIBRCODE, int EIBRCODEOffset, int EIBRESP) {
        this.EIBFN = EIBFN;
        this.EIBRCODE = EIBRCODE;
        this.EIBRCODEOffset = EIBRCODEOffset;
        this.EIBRESP = EIBRESP;
    }

    public byte EIBFN()  { return EIBFN; }
    public byte EIBRCODE() { return EIBRCODE; }
    public int EIBRCODEOffset() { return EIBRCODEOffset; }
    public int EIBRESP() { return EIBRESP; }
}
