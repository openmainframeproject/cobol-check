package org.openmainframeproject.cobolcheck.services.filehelpers;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

import static java.nio.file.FileVisitResult.CONTINUE;

/**
 * Given the standard test suite directory structure:
 *
 * testsuites/
 *   programname/
 *       testname
 *       testname
 *       testname
 *
 *  this class matches filenames with a glob within a programname subdirectory only.
 *
 * @author Dave Nicolette based on Oracle sample
 * @since 1.7
 */
public class FileNameMatcher extends SimpleFileVisitor<Path> {
        private final PathMatcher matcher;
        private List<String> matchingFiles;
        private boolean defaultToAllFiles = false;

        public FileNameMatcher(String pattern) {
            if (pattern.length() == 0 || pattern.equals("null")) {
                defaultToAllFiles = true;
            }
            matcher = FileSystems.getDefault().getPathMatcher("glob:" + pattern);
            matchingFiles = new ArrayList<>();
        }

        public List<String> getMatchingFiles() {
            return matchingFiles;
        }

        void find(Path file) {
            Path name = file.getFileName();
            if (name != null) {
                if (defaultToAllFiles || matcher.matches(name)) {
                    if (new File(String.valueOf(file)).isFile()) {
                        matchingFiles.add(file.toAbsolutePath().toString());
                    }
                }
            }
        }

        @Override
        public FileVisitResult visitFile(Path file,
                                         BasicFileAttributes attrs) {
            find(file);
            return CONTINUE;
        }

        @Override
        public FileVisitResult preVisitDirectory(Path dir,
                                                 BasicFileAttributes attrs) {
            find(dir);
            return CONTINUE;
        }

        @Override
        public FileVisitResult visitFileFailed(Path file, IOException exc) {
            System.err.println(exc);
            return CONTINUE;
        }
    }

