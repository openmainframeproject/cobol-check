package com.neopragma.cobolcheck;

public interface StringHelper {

    default boolean isBlank(String subject) {
        return subject == null || subject.equals("");
    }

    default String defaultIfBlank(String subject, String defaultValue) {
        return isBlank(subject) ? defaultValue : subject;
    }

    default boolean isEmptyArray(String[] subject) {
        return subject == null || subject.length == 0;
    }

    default String fixedLength(String sourceLine) {
        return String.format("%1$-80s", sourceLine.stripTrailing()) + "\n";
    }

}
