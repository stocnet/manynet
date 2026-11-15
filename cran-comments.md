## Release context

manynet 2.5.0 comes one month after 2.4.0 (14 October).
It is the first of a coordinated release with netrics 1.2.0,
autograph 1.3.0, infernet 0.7.0, and migraph 2.0.0,
which build on its new features and follow once it is on CRAN.
The functions, arguments, and datasets that 2.5.0 renames
remain under their old names as wrappers until 2.6.0.

## Test environments

* local macOS (aarch64-apple-darwin23), R 4.6.1

Current R versions: release 4.6.2, oldrel 4.5.3, devel 4.7.0.

## R CMD check results

0 errors | 0 warnings | 2 notes

* Number of updates in past 6 months: 8.
  This release is the base for the coordinated releases above,
  so it cannot wait without holding back all four of them.

* checking HTML version of manual: Skipping checking HTML validation:
  'tidy' doesn't look like recent enough HTML Tidy.
  This note comes from the local HTML Tidy, not from the package.

## Reverse dependencies

We checked the CRAN versions of the 4 reverse dependencies
with R CMD check, against this version and against 2.4.0.

* goldfish 1.6.12: no new problems.
* migraph 1.7.2: no new problems.
  It stays on CRAN until migraph 2.0.0 follows the release of infernet,
  so this version keeps the old names that it uses.
* netrics 1.1.0: 2 example pages and 13 tests fail.
* autograph 1.2.8: 1 tutorial test fails.

These failures are expected, and are all in the examples and tests.
This version makes more of its functions return its own network class,
corrects a bug in `create_core()`, and converts several datasets to that
class. The released netrics and autograph test for the old classes and
values, and two netrics examples plot a network of the new class
without loading autograph.

We maintain both packages. netrics 1.2.0 and autograph 1.3.0 pass their
checks against this version, and need it, so we submit them as soon as
this version is on CRAN.
