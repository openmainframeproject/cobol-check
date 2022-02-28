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
package org.openmainframeproject.cobolcheck;

import org.openmainframeproject.cobolcheck.services.cobolLogic.EIBResponseCodes;
import org.openmainframeproject.cobolcheck.services.cobolLogic.EIBResponseTable;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class EIBResponseTableTest {

    @ParameterizedTest
    @MethodSource("EIBResponseCodesProvider")
    public void it_finds_EIB_codes_for_defined_condition_names(
            String conditionName,
            byte expectedEIBFN,
            byte expectedEIBRCODE,
            int expectedEIBRCODEOffset,
            int expectedEIBRESP) {
        EIBResponseCodes actual = EIBResponseTable.lookup(conditionName);
        assertEquals(expectedEIBFN, actual.EIBFN());
        assertEquals(expectedEIBRCODE, actual.EIBRCODE());
        assertEquals(expectedEIBRCODEOffset, actual.EIBRCODEOffset());
        assertEquals(expectedEIBRESP, actual.EIBRESP());
    }

    private static Stream<Arguments> EIBResponseCodesProvider() {
        return Stream.of(
                Arguments.of("INVREQ", (byte) 0x06, (byte) 0x08, 0, 16),
                Arguments.of("NOTFND", (byte) 0x06, (byte) 0x81, 0, 13),
                Arguments.of("EOC", (byte) 0x04, (byte) 0x20, 1, 6)
        );
    }

    @ParameterizedTest
    @MethodSource("EIBResponseCodesMixedCaseProvider")
    public void EIB_codes_lookup_is_case_agnostic(
            String conditionName,
            byte expectedEIBFN,
            byte expectedEIBRCODE,
            int expectedEIBRCODEOffset,
            int expectedEIBRESP) {
        EIBResponseCodes actual = EIBResponseTable.lookup(conditionName);
        assertEquals(expectedEIBFN, actual.EIBFN());
        assertEquals(expectedEIBRCODE, actual.EIBRCODE());
        assertEquals(expectedEIBRCODEOffset, actual.EIBRCODEOffset());
        assertEquals(expectedEIBRESP, actual.EIBRESP());
    }

    private static Stream<Arguments> EIBResponseCodesMixedCaseProvider() {
        return Stream.of(
                Arguments.of("FileNotFound", (byte) 0x06, (byte) 0x01, 0, 12),
                Arguments.of("nostart", (byte) 0x04, (byte) 0xF6, 3, 10),
                Arguments.of("noTFnd", (byte) 0x06, (byte) 0x81, 0, 13)
        );
    }

    @Test
    public void it_returns_zeroes_when_the_condition_name_is_not_found() {
        EIBResponseCodes actual = EIBResponseTable.lookup("this is not a valid condition name");
        assertEquals((byte) 0x00, actual.EIBFN());
        assertEquals((byte) 0x00, actual.EIBRCODE());
        assertEquals(0, actual.EIBRCODEOffset());
        assertEquals(0, actual.EIBRESP());
    }
}
