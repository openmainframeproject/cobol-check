package com.neopragma.cobolcheck;

/**
 * CICS EIB values for EIBFN, EIBRCODE, and EIBRESP. This data structure describes the value returned from a Map lookup on EIBValues where the condition name is the key. For example:
 *
 * key = "NOTFND"
 * value =
 *     EIBFN = 0x06 (this goes into the first byte of the EIBFN field)
 *     EIBRCODE = 0x81 (this goes into the first byte of the EIBRCODE field)
 *     EIBRESP = 13
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public record EIBResponseCodes(byte EIBFN, byte EIBRCODE, int EIBRESP
    ) {}
