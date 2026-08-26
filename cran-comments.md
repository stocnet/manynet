## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.0
* macOS 15.7.7 (on Github), R 4.6.0
* Microsoft Windows Server 2025 10.0.26100 (on Github), R 4.6.0
* Ubuntu 24.04.4 (on Github), R 4.6.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

The auto-check of 2.3.0 reported new failures in `autograph` and `netrics`.
I maintain both packages.

This version fixes the three causes that were bugs in `manynet`:

* `tie_attribute()` and `node_attribute()` aborted on a stocnet object where
  the caller named no attribute, which the `{igraph}` method allows.
* A mark inside `filter_ties()` read the outer network, not the filtered one.
* `is_longitudinal()` marked a network whose ties carry no moment.

The remaining failures are calls in the released `autograph` 1.1.2 to
`to_no_isolates()`, which 2.3.0 deprecates, and to arguments that `autograph`
itself deprecates. `autograph` 1.2.0 removes them and passes against this
version with 0 failures. It is submitted separately.
