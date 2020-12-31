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
package com.neopragma.cobolcheck;

import java.time.Instant;

/**
 * Empty logging implementation - don't yet know
 * how we will handle logging in the application.
 * This class might serve as a generic wrapper so
 * we don't have to change a lot of code once we've
 * decided how to handle logging.
 */
public class Log {
    private static LogLevel currentLogLevel = LogLevel.INFO;

    public static void off() {
        currentLogLevel = LogLevel.OFF;
    }
    public static void set(LogLevel level) {
        currentLogLevel = level;
    }
    public static LogLevel level() { return currentLogLevel; }
    public static void fatal(String message) {
        if (currentLogLevel.ordinal() >= LogLevel.FATAL.ordinal()) {
            write(message);
        }
    }
    public static void error(String message) {
        if (currentLogLevel.ordinal() >= LogLevel.ERROR.ordinal()) {
            write(message);
        }
    }
    public static void warn(String message){
        if (currentLogLevel.ordinal() >= LogLevel.WARN.ordinal()) {
            write(message);
        }
    }
    public static void info(String message) {
        if (currentLogLevel.ordinal() >= LogLevel.INFO.ordinal()) {
            write(message);
        }
    }
    public static void debug(String message) {
        if (currentLogLevel.ordinal() >= LogLevel.DEBUG.ordinal()) {
            write(message);
        }
    }
    public static void trace(String message) {
        if (currentLogLevel.ordinal() >= LogLevel.TRACE.ordinal()) {
            write(message);
        }
    }
    private static void write(String message) {
        System.err.println("CobolCheck: "+ currentLogLevel.toString() + " " + Instant.now() + " " + message);
    }
}
