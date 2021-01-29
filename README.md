# Cobol Check 

https://travis-ci.com/neopragma/cobol-check.svg?branch=main

[![Sonarcloud Status](https://sonarcloud.io/api/project_badges/measure?project=com.neopragma.cobolcheck:cobol-check&metric=alert_status)](https://sonarcloud.io/dashboard?id=com.neopragma.cobolcheck:cobol-check)

Cobol Check provides fine-grained unit testing/checking for Cobol at the same conceptual level of detail as unit testing frameworks for other languages, such as Python, Ruby, C#, and Java. 

## Immediate needs 

As of January 2021 we could use help with: 

- access to a z/OS system including MVS and USS for testing cobol-check on-platform
- review of localized messages by native speakers and creation of additional localized message files

## Why?

The industry is experiencing a resurgence in interest in Cobol, both to support existing applications and to take advantage of the continuing evolution of the zSeries platform. Commercial unit testing tools for Cobol are able to exercise code at the level of a whole load module, but cannot exercise individual Cobol paragraphs in isolation. That limitation means we cannot achieve the same degree of granularity in microtests as we can when working in other languages, such as Java, Kotlin, C#, Python, or Ruby.

Given that capability, we might anticipate some of the same effects on the design of Cobol code as we see in the design of code in other languages, when fine-grained microtesting is used. These include factors like modularity, smaller source units, closer alignment with basic software design guidelines such as separation of concerns and single responsibility, and so forth.

As much of the work in this space will involve support for existing code bases, we might also anticipate that incremental refactoring will become easier once we have a safety net of fine-grained examples in place. We can apply the same techniques we use to improve the design of existing code in other languages.

## Installing, Using, Contributing

Please see [the wiki](https://github.com/neopragma/cobol-check/wiki/) for more information.

## Target platforms 

We want to enable Cobol developers to write and run fine-grained unit checks on: 

| Platform | Version | Tested? | Other |
| -------- | ------- | ------- | ----- |
| Ubuntu Linux | 18.04.05 | Jan 21 | Note 1 |
/ Ubuntu Linux | 20.04 | No | Note 2 |
| IBM z/OS | 2.4 | No | Note 3 |
/ MS Windows | 10 | No | Note 3 |
/ Fedora Linux / 33 | No | Note 4 |
/ Fedora Linux / 34 | No | Note 4 |
| FreeBSD | 12 | No | Note 4 |
| OpenBSD | 68 | No | Note 4 |
/ Apple OSX / 10 | No | Note 4 |
| Apple OSX | 11 | No | Note 4

Note 1: Current development platform 
Note 2: New development platform (coming) 
Note 3: Primary target platform 
Note 4: Secondary (nice to have) target platform

## Related projects

- The original proof-of-concept project: https://github.com/neopragma/cobol-unit-test
- A fork with enhancements to work on zOS, and bug fixes: https://github.com/Rune-Christensen/cobol-unit-test

## Governance

This project is part of the [Open Mainframe Project](https://www.openmainframeproject.org/). 

## Reporting Issues

To report a problem or request a feature, please open an [issue](https://github.com/neopragma/cobol-check/issues).
