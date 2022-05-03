package org.openmainframeproject.cobolcheck.services.cobolLogic;

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
