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
package com.neopragma.cobolcheck.services.filehelpers;

import com.neopragma.cobolcheck.services.Messages;
import com.neopragma.cobolcheck.services.log.Log;

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
 *  this class matches directory names with a glob to identify the programname subdirectories.
 *  Then FileNameMatcher is used to match testsuite filenames within each subdirectory.
 *
 * @author Dave Nicolette based on Oracle sample
 * @since 1.7
 */
public class DirectoryNameMatcher extends SimpleFileVisitor<Path> {
        private final PathMatcher matcher;
        private final List<String> matchingDirectories;

        public DirectoryNameMatcher(String pattern) {
            matcher = FileSystems.getDefault().getPathMatcher("glob:" + pattern);
            matchingDirectories = new ArrayList<>();
        }

        public List<String> getMatchingDirectories() {
            return matchingDirectories;
        }

        public void find(Path path) {
            Path name = path.getFileName();
            if (name != null && matcher.matches(name)) {
                if (new File(String.valueOf(path)).isDirectory()) {
                    matchingDirectories.add(path.toString());
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
            Log.warn(Messages.get(String.format("WRN003", file.getFileName())));
            return CONTINUE;
        }
    }

