## Test environments

* local R installation, x86_64-apple-darwin20, R 4.3.0
* Mac OS X 12.6.5 (on Github), R 4.3.0
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.3.0
* Ubuntu 22.04.2 (on Github), R 4.3.0
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2022 R-devel, 64 bit
    - Gave following notes, which are either unrelated to the package or can be ignored according to R-hub issues [#503](https://github.com/r-hub/rhub/issues/503) and [#560](https://github.com/r-hub/rhub/issues/560):
      ```
      * checking CRAN incoming feasibility ... [20s] NOTE
      New submission
      Maintainer: 'James Hollway <james.hollway@graduateinstitute.ch>'
      * checking HTML version of manual ... NOTE
      Skipping checking math rendering: package 'V8' unavailable
      * checking for non-standard things in the check directory ... NOTE
      Found the following files/directories:
      ''NULL''
      * checking for detritus in the temp directory ... NOTE
      Found the following files/directories:
      'lastMiKTeXException'
      ```
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
    - Gave following notes, are either unrelated to the package or can be ignored:
      ```
      * checking CRAN incoming feasibility ... [7s/30s] NOTE
      Maintainer: ‘James Hollway <james.hollway@graduateinstitute.ch>’
      New submission
      * checking HTML version of manual ... NOTE
      Skipping checking HTML validation: no command 'tidy' found
      Skipping checking math rendering: package 'V8' unavailable
      ```

## R CMD check results

0 errors | 0 warnings | 0 notes

Since original submission and in response to CRAN requests:
- There are no specific references at the package level at present;
methods underlying functions are referenced in the respective documentation.
- All missing \value tags have now been added.
- Commented out examples have been uncommented or deleted.
- In the make_read.R tests, we are using tempfile() to create a temporary file paths
for testing purposes. 
We understand that tempfile() calls tempdir(), so the effect should be the same?
