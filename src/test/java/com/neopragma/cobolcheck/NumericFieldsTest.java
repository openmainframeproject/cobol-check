package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.features.parser.NumericFields;
import com.neopragma.cobolcheck.services.cobolLogic.DataType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class NumericFieldsTest {

    private NumericFields numericFields;

    @BeforeEach
    public void commonSetup() {
        numericFields = new NumericFields();
    }

    @Test
    public void it_returns_ALPHANUMERIC_when_a_field_name_is_not_found() {
        assertEquals(DataType.ALPHANUMERIC, numericFields.dataTypeOf("WS-FIELDNAME"));
    }

    @Test
    public void it_returns_PACKED_DECIMAL_when_a_field_is_listed_as_packed_decimal() {
        numericFields.reset();
        numericFields.setDataTypeOf("WS-FIELD-1", DataType.PACKED_DECIMAL);
        assertEquals(DataType.PACKED_DECIMAL, numericFields.dataTypeOf("WS-FIELD-1"));
    }

    @Test
    public void it_returns_FLOATING_POINT_when_a_field_is_listed_as_floating_point() {
        numericFields.reset();
        numericFields.setDataTypeOf("WS-FIELD-1", DataType.FLOATING_POINT);
        assertEquals(DataType.FLOATING_POINT, numericFields.dataTypeOf("WS-FIELD-1"));
    }

    @Test
    public void it_throws_when_fieldname_is_null_on_dataTypeOf() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                numericFields.dataTypeOf(null));
        assertTrue(ex.getMessage().contains("ERR027:"));
    }

    @Test
    public void it_throws_when_fieldname_is_empty_on_dataTypeOf() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                numericFields.dataTypeOf(""));
        assertTrue(ex.getMessage().contains("ERR027:"));
    }

    @Test
    public void it_throws_when_fieldname_is_null_on_setDataTypeOf() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                numericFields.setDataTypeOf(null, DataType.PACKED_DECIMAL));
        assertTrue(ex.getMessage().contains("ERR028:"));
    }

    @Test
    public void it_throws_when_fieldname_is_empty_on_setDataTypeOf() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                numericFields.setDataTypeOf("", DataType.PACKED_DECIMAL));
        assertTrue(ex.getMessage().contains("ERR028:"));
    }

    @Test
    public void it_throws_when_datatype_is_null_on_setDataTypeOf() {
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () ->
                numericFields.setDataTypeOf("WS-FIELD-NAME", null));
        assertTrue(ex.getMessage().contains("ERR029:"));

    }
}
