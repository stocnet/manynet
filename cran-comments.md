## Test environments

* local R installation, aarch64-apple-darwin20, R 4.4.1
* macOS 14.5 (on Github), R 4.4.1
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.4.1
* Ubuntu 22.04.4 (on Github), R 4.4.1

## R CMD check results

0 errors | 0 warnings | 0 notes

* Re errors on CRAN, this version fixes errors on Linux versions for which the {oaqc} package is not available
* Re previous submission, this version fixes a DOI and avoids an issue with {igraph}
* Fixed errors occurring in tests and examples when suggested packages excluded