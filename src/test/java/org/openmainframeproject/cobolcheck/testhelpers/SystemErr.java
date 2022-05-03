package org.openmainframeproject.cobolcheck.testhelpers;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public class SystemErr {
    private final PrintStream originalSystemErr = System.err;
    private ByteArrayOutputStream tempSystemErr;

    public void record() {
        tempSystemErr = new ByteArrayOutputStream();
        System.setErr(new PrintStream(tempSystemErr));
    }

    public String playback() {
        System.setErr(originalSystemErr);
        System.setErr(originalSystemErr);
        return tempSystemErr.toString();
    }
}
