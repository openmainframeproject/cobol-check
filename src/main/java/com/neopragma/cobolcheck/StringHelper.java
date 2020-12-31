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

public interface StringHelper {

    default boolean isBlank(String subject) {
        return subject == null || subject.equals("");
    }

    default boolean notBlank(String subject) {
        return subject != null && !subject.equals("");
    }

    default String defaultIfBlank(String subject, String defaultValue) {
        return isBlank(subject) ? defaultValue : subject;
    }

    default boolean isEmptyArray(String[] subject) {
        return subject == null || subject.length == 0;
    }

    default String fixedLength(String sourceLine) {
        return String.format("%1$-80s", sourceLine.stripTrailing()) + System.getProperty("line.separator");
    }

}
