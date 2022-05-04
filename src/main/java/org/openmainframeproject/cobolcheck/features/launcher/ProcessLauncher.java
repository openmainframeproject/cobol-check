package org.openmainframeproject.cobolcheck.features.launcher;

/**
 * Abstraction of platform-dependent process launchers.
 *
 * @author Dave Nicolette
 * @since 1.5
 */
public interface ProcessLauncher {
    String getProcessConfigKeyPrefix();
    Process run(String programName);


}
