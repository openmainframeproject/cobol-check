package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.exceptions.PossibleInternalLogicErrorException;
import com.neopragma.cobolcheck.testhelpers.SystemErr;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
public class GeneratorGeneralTest {

    private StringBuilder cobolSourceData;
    private StringWriter testProgramSource;
    private Generator generator;
    private SystemErr systemErr = new SystemErr();

    @Mock
    TestSuite mockTestSuite;

    @Mock
    File fakeInputFile;

    @BeforeEach
    void commonSetup() {
        cobolSourceData = new StringBuilder();
        testProgramSource = new StringWriter();
        generator = new Generator(new Messages());
    }

    @Test
    void TestSuite_cannot_be_null__probable_internal_logic_error() {
        loadInputData(Constants.EMPTY_STRING);
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () -> {
            mergeTestSuiteAndVerifyResults(null, cobolSourceData, testProgramSource);
        });
    }

    @Test
    void cobol_source_cannot_be_null__probable_internal_logic_error() {
        assertThrows(PossibleInternalLogicErrorException.class, () -> {
            generator.mergeTestSuite(mockTestSuite, null, testProgramSource);
        });
    }

    @Test
    void cobol_source_cannot_be_empty__probable_internal_logic_error() {
        cobolSourceData = new StringBuilder();
        Exception ex = assertThrows(PossibleInternalLogicErrorException.class, () -> {
            mergeTestSuiteAndVerifyResults(mockTestSuite, cobolSourceData, testProgramSource);
        });
        assertTrue(ex.getMessage().contains("empty input stream"));
    }

    private void loadInputData(String... lines) {
        for (String line : lines) {
            cobolSourceData.append(line);
        }
    }

    private Writer mergeTestSuiteAndVerifyResults(TestSuite testSuite,
                                                  StringBuilder cobolSourceData,
                                                  Writer testProgramSource) {
        StringReader cobolProgramSource = new StringReader(cobolSourceData.toString());
        Writer mergedCobolData = generator.mergeTestSuite(testSuite, cobolProgramSource, testProgramSource);
        assertEquals(cobolSourceData.toString(), mergedCobolData.toString());
        return mergedCobolData;
    }
}

