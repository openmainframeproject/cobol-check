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
public class GeneratorTest {

    private TestSuite emptyTestSuite = new EmptyTestSuite();
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
        generator = new Generator();
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

    @Test
    void when_TestSuite_is_empty_it_skips_processing_and_logs() {
        loadInputData(Constants.EMPTY_STRING);
        Log.set(LogLevel.INFO);
        systemErr.record();

        Writer testProgramSource = mergeTestSuiteAndVerifyResults(emptyTestSuite, cobolSourceData, new StringWriter());

        Log.set(LogLevel.OFF);
        assertTrue(systemErr.playback().contains("TestSuite is empty")
                && systemErr.playback().contains("nothing to do here"));
        assertEquals(Constants.EMPTY_STRING, testProgramSource.toString());
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

