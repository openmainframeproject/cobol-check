package com.neopragma.cobolcheck;

import java.io.Reader;
import java.io.StringReader;

public class EmptyTestSuite implements TestSuite {
    @Override
    public Reader getDataAsReader() {
        return new StringReader(Constants.EMPTY_STRING);
    }
}
