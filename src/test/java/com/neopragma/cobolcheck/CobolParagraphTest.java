package com.neopragma.cobolcheck;

import com.neopragma.cobolcheck.structure.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CobolParagraphTest {

    @ParameterizedTest
    @MethodSource("paragraphTestProvider")
    public void paragraph_knows_its_name_and_whether_it_is_mandatory(
            CobolParagraph paragraph,
            String expectedName,
            boolean expectedMandatory) {
        assertEquals(expectedName, paragraph.getName());
        assertEquals(expectedMandatory, paragraph.isMandatory());
    }

    private static Stream<Arguments> paragraphTestProvider() {
        return Stream.of(
                Arguments.of(new AuthorParagraph(), "AUTHOR", false),
                Arguments.of(new DateWrittenParagraph(), "DATE-WRITTEN", false),
                Arguments.of(new InstallationParagraph(), "INSTALLATION", false),
                Arguments.of(new ObjectComputerParagraph(), "OBJECT-COMPUTER", false),
                Arguments.of(new ProgramIdParagraph(), "PROGRAM-ID", true),
                Arguments.of(new SourceComputerParagraph(), "SOURCE-COMPUTER", false),
                Arguments.of(new SpecialNamesParagraph(), "SPECIAL-NAMES", false)
        );
    }
}
