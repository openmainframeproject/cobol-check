package com.neopragma.cobolcheck;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class EIBResponseLookupTest {

    @ParameterizedTest
    @MethodSource("EIBResponseCodesProvider")
    public void it_finds_EIB_codes_for_defined_condition_names(
            String conditionName,
            byte expectedEIBFN,
            byte expectedEIBRCODE,
            int expectedEIBRESP) {
        EIBResponseCodes actual = EIBResponseLookup.lookup(conditionName);
        assertEquals(expectedEIBFN, actual.EIBFN());
        assertEquals(expectedEIBRCODE, actual.EIBRCODE());
        assertEquals(expectedEIBRESP, actual.EIBRESP());
    }

    private static Stream<Arguments> EIBResponseCodesProvider() {
        return Stream.of(
                Arguments.of("INVREQ", (byte) 0x02, (byte) 0xE0, 16),
                Arguments.of("NOTFND", (byte) 0x06, (byte) 0x81, 13)
        );
    }



}
