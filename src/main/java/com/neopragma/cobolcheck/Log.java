package com.neopragma.cobolcheck;

/**
 * Empty logging implementation - don't yet know
 * how we will handle logging in the application.
 * This class might serve as a generic wrapper so
 * we don't have to change a lot of code once we've
 * decided how to handle logging.
 */
public class Log {
    private static LogLevel currentLogLevel = LogLevel.OFF;

    public static void off() {
        currentLogLevel = LogLevel.OFF;
    }
    public static void set(LogLevel level) {
        currentLogLevel = level;
    }
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
        System.err.println("Generator Log: " + message);
    }
}
