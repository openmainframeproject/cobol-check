# cobol-unit-test-language-extension README
[![Build](https://github.com/Bankdata/cobol-unit-test-language-extension/actions/workflows/build.yml/badge.svg)](https://github.com/Bankdata/cobol-unit-test-language-extension/actions/workflows/build.yml)

[![Feature test](https://github.com/Bankdata/cobol-unit-test-language-extension/actions/workflows/features.yml/badge.svg)](https://github.com/Bankdata/cobol-unit-test-language-extension/actions/workflows/features.yml)

A VSCode package for the Cobol unit test language(CUT). At this time, the extension has the following features:
1. Syntax highlighting
2. Auto completion using snippets
3. Error reporting of mismatched delimiters

The extension is meant as a quality of life improvement for unit testing in COBOL. The unit test language, for which the extension is designed, can be found here:
https://github.com/neopragma/cobol-unit-test.

The extension client is implemented for VScode, while the server is entirely decoupled, such that it can be 'attached' to an arbitrary client and used with any IDE.

Testing:
Press F5 to open a new window with the extension loaded.
Set breakpoints in the code inside src/extension.ts to debug the extension.
Find output from the extension in the debug console.
You can relaunch the extension from the debug toolbar after changing code in client/src/extension.ts.
You can also reload (Ctrl+R or Cmd+R on Mac) the VS Code window with the extension to load the changes.

Automated tests:
There is already a few tests for desired behavior, two of which fail due to missing implementation. The currently failing tests correspond to the missing
features described in CONTRIBUTING.md. 

Logging:
There is a dedicated logger for both client and server that gets its level of detail from settings. The logger in client can be used at arbitrary positions, while
the server logger must be used in the context of an open textdocument. For additional logging needs, server side, use connection.console.log().

Installation:
There are two options:
1. Drag and drop the entire sourcecode into the vscode extensions folder C:\Users\'youruser'\.vscode\extensions.
2. Build the extension to a .vsix file and drop it into the extension tab of your VScode window.

## Requirements

The cobol-unit-test-syntax package references the COBOL grammar, defined in IBM Z Open Editor, for inline COBOL.
Additionally, tab behavior is handled by referencing the custom tab function defined in aforementioned extension.

## Extension Settings

This extension contributes the following settings:

* `cut.tabstops`: Set tab stop locations for the CUT language.
* `cut.trace.server`: Determines the level of detail of communication with the language server displayed in the output channel.
* `cut.loglevel`: Desired level of detail in logging.
* `cut.maxNumberOfProblems`: Controls the maximum number of problems produced by the server for debugging purposes. Limit to be removed on first release.
