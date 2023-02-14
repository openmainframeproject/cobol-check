# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## \[1.0.0\]

### Planned

- Before/After All
- Parameterized tests

## \[0.3.0\]

### Planned

- Mock CICS resources
- Mock SQL tables
- Mock batch file I/O

## \[0.2.4\] 2022-11-02
### Implemented
- EXPECT accepts COBOL keywords again as the last keyword 
- EXPECT handles negative numbers again
 
## \[0.2.3\] 2022-10-28
### Implemented
- Implemented context aware syntax check
- Minor bug fixes

## \[0.2.2\] 2022-09-30
### Implemented
- Improved text output formatting on failed tests
- Minor improvement for syntax error messages
- Minor bug fixes

## \[0.2.1\] 2022-08-18
### Implemented
- Fixed links in test results HTML
- Made it possible to have sequence numbers in unit tests
- Made error messages in the error log better


## \[0.2.0\] 2022-07-15
### Implemented
- Mock paragraph
- Mock section
- Mock Call statement
- Mocks can be global or local
- Verification of the amount of times a mock was accessed
- Before/After Each
- Start- and end tags for injected code
- Support for DECIMAL-POINT IS COMMA
- Test output in XML JUnit format
- Test output as HTML
- Simple syntax and runtime error log for the cobol unit test language (cut)
- Configuration for file encoding
- Configuration for file access
- Configuration for running or simply generating the test program
- Configuration for including rules as the first part of the generated program

### Known issues



## \[0.1.0\] 2021-03-15

### Implemented

- Basic test case functionality 

### Known issues 

- On Microsoft Windows, the -p or --programs command-line option only works with a single program name at a time.

## \[0.0.0\]

### Added 2020-12-08

- Started the cobol-check project as a follow-on to https://github.com/neopragma/cobol-unit-test, which was a proof-of-concept effort to produce a unit testing tool for Cobol that could exercise individual Cobol paragraphs in isolation.

