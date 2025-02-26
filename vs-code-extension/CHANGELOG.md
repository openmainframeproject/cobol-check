# Change Log

All notable changes to the "cobol-unit-test" extension will be documented in this file. Versioning according to SemVer: https://semver.org/ 



## [0.4.10] 05.12.2024
- Now using COBOL Check version 0.2.16

## [0.4.9] 09.12.2024
- Now CORRECTLY using COBOL Check version 0.2.14

## [0.4.8] 05.12.2024
- Now using COBOL Check version 0.2.14

## [0.4.7] 08.10.2024
- Now using COBOL Check version 0.2.13

## [0.4.6] 08.10.2024
- Fixed windows run test script in the extension to only run the executable once.

## [0.4.5] 08.10.2024
- Now using COBOL Check version 0.2.12 

## [0.4.4] 04.04.2024
- Now using COBOL Check version 0.2.9 

## [0.4.3] 29.12.2023
- Create alternative temp file for cobol check extension
- Unit test can now run with or without input file

## [0.4.2] 21.09.2023
- Using VScode build in Testing control framework
- Removed Cobol Check view in view container

## [0.4.1] 16.05.2023
- Now using COBOL Check version 0.2.8

## [0.4.0] 22.03.2023
- Now using COBOL Check version 0.2.7

## [0.3.9] 02.01.2023
- Bugfix: Fixed an issue where the configure-command would remove a line from the configuration file, everytime you would overwrite an existing value.

## [0.3.8] 21.12.2022
- Fix for last release

## [0.3.7] 21.12.2022
- Now using Cobol Check version 0.2.5
- Prepared for GnuCOBOL compiler options in config and scripts

## [0.3.6] 02.11.2022
- Now using Cobol Check version 0.2.4

## [0.3.5] 28.10.2022
- Now using Cobol Check version 0.2.3

## [0.3.4] 30.09.2022
- Now using Cobol Check version 0.2.2
- Improved error handling and info
- Minor bug fixes

## [0.3.3] 18.08.2022
- Now using Cobol Check version 0.2.1
- Fixed links in test results HTML
- Made it possible to have sequence numbers in unit tests

## [0.3.2] 15.07.2022

- Now using Cobol Check version 0.2.0
- Made Cobol Check loading window, show up as a notification
- Fixed test report panel, not showing the correct name

## [0.3.1] 04.07.2022

- Fixed bug where syntax check would not allow a string followed by a string

## [0.3.0] 22.06.2022

- Added support for subfolders inside source cobol folder and copybook folder
- Added better logs

## [0.2.9] 22.06.2022

- Fixed bug, where mocks would not recognize sections in source code, if it contained sequence numbers
- Fixed bug, where the unit test path would not show, if the testsuite was named with single quatations

## [0.2.8] 22.06.2022

- Cobol Check can now take source code with sequence numbers
- Fixed bug, where the html-formatter could not handle DISPLAY lines from the source code

## [0.2.7] 16.06.2022

- Fixed bugs in Cobol Check and updated jar
- Added support for running tests from outside your current workspace
- Drastically sped up file look-up, when standing in .cut-file and looking for cobol source 

## [0.2.6] 07.06.2022

- Fixed bugs in Cobol Check and updated jar

## [0.2.5] 01.06.2022

- Moved test results to its own WebViewPanel
- Added html formatted test results

## [0.2.4] 20.05.2022

- Retrying to do what is specified for 0.2.3

## [0.2.3] 20.05.2022

- Removed ignore statement that prevented Cobol Check jar from being included in release

## [0.2.2] 20.05.2022

- Fixed bug where the path to Cobol Check would be incorrect

## [0.2.1] 19.05.2022

- Added error handling and messages

## [0.2.0] 19.05.2022

- Integrated Cobol Check into extension
- Added Cobol Check view
- Added command for running Cobol Check
- Added command for configuring Cobol Check
- Added command for resetting the configurations

## [0.1.1] 04.03.2022

- Updated Readme, as not all information was correct
- Added icon
- Cleaned up some unnecessary files

## [0.1.0] 02.03.2022

- Initial release