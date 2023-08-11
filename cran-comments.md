## Test environments

* local R installation, x86_64-apple-darwin20, R 4.3.0
* Mac OS X 12.6.5 (on Github), R 4.3.0
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.3.0
* Ubuntu 22.04.2 (on Github), R 4.3.0
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2022 R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 0 notes

* Some functions use parts of the BioConductor package 'Rgraphviz'; we have created a helper function for users to download 'BiocManager' and 'Rgraphviz' if necessary and not already installed, and all tests pass on e.g. R-hub, but we wanted to flag our solution here.