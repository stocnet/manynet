## Test environments

* local R installation, aarch64-apple-darwin20, R 4.4.0
* macOS 14.5 (on Github), R 4.4.1
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.4.1
* Ubuntu 22.04.4 (on Github), R 4.4.1

## R CMD check results

0 errors | 0 warnings | 0 notes

* Among other things, this major release consolidates a number of functions that were in migraph,
* We are simultaneously submitting a new version of migraph that is without these
functions to avoid conflicts.
* migraph still Depends on manynet, so there should be no impact for users
downloading migraph and expecting to use the functions in question.
* Any function name changes are listed as deprecated in manynet
