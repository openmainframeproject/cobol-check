package org.openmainframeproject.cobolcheck;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;
import org.openmainframeproject.cobolcheck.services.Constants;
import org.openmainframeproject.cobolcheck.workers.CobolTestRunner;
import org.openmainframeproject.cobolcheck.workers.Generator;
import org.openmainframeproject.cobolcheck.workers.Initializer;

public class ExitCodeTest {
    @Test
    public void no_error_exit_code() throws InterruptedException {
        String[] args={"-p","ALPHA"};
        Initializer initializer = new Initializer(args);
        initializer.run();
        Generator generator = new Generator();
        String programName = initializer.getSourceProgramNames()[0];
        generator.prepareAndRunMerge(programName, initializer.getTestFileNames());
        CobolTestRunner testRunner = new CobolTestRunner();
        testRunner.run(programName, initializer.isLastSourceProgram(programName));
        assertEquals(testRunner.getReturnCode(), Constants.STATUS_NORMAL);
    }

    @Test
    public void has_error_exit_code() throws InterruptedException {
        String[] args={"-p","MOCKTEST"};
        Initializer initializer = new Initializer(args);
        initializer.run();
        Generator generator = new Generator();
        String programName = initializer.getSourceProgramNames()[0];
        generator.prepareAndRunMerge(programName, initializer.getTestFileNames());
        CobolTestRunner testRunner = new CobolTestRunner();
        testRunner.run(programName, initializer.isLastSourceProgram(programName));
        assertEquals(testRunner.getReturnCode(), Constants.STATUS_TEST_ERROR);
    }
}
