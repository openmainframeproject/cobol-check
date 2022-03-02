# Cobol Check Extension

Cobol Check provides fine-grained unit testing/checking for Cobol at the same conceptual level of detail as unit testing frameworks for other languages, such as Python, Ruby, C#, and Java.

This package delivers the functionality of Cobol Check(not yet implemented) along with the Cobol unit test language(CUT). This is the language used to write unit tests for Cobol Check.

Currently the extension has the following features pertaining the the CUT langauge:
1. Syntax highlighting
2. Auto completion using snippets
3. Error reporting of mismatched delimiters

## Requirements

The cobol-unit-test-syntax package references the COBOL grammar, defined in IBM Z Open Editor, for inline COBOL.
Additionally, tab behavior is handled by referencing the custom tab function defined in aforementioned extension.

## Extension Settings

This extension contributes the following settings:

* `cut.tabstops`: Set tab stop locations for the CUT language.
* `cut.trace.server`: Determines the level of detail of communication with the language server displayed in the output channel.
* `cut.loglevel`: Desired level of detail in logging.
* `cut.maxNumberOfProblems`: Controls the maximum number of problems produced by the server for debugging purposes. Limit to be removed on first release.

## Contributing

If you are interested in contributing to Cobol Check or this extension, start here: https://github.com/openmainframeproject/cobol-check

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
