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

import java.util.List;

/**
 * This record encapsulates information about a cobol-check keyword.
 *
 * value = the value of the keyword as a string (note that some keywords have embedded spaces, like "TO BE")
 * validNextKey = list of keys in Keywords for tokens that may follow the current keyword in the test suite.
 * keywordAction = special handling for this keyword, if any.
 *
 * @author Dave Nicolette (neopragma)
 * @since 14
 */
public record Keyword(
        String value,
        List<String> validNextKey,
        KeywordAction keywordAction) { }
