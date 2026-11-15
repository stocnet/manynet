# The console interface divides into calls that change what a function does,
# which always fire, and calls that only report, which the verbosity silences.

test_that("a guard aborts under the default verbosity", {
  op <- options(snet_verbosity = "quiet")
  on.exit(options(op), add = TRUE)
  # it used to return invisibly, letting the code it guards run on
  expect_error(snet_unavailable("Not yet."), "Not yet.")
  expect_error(snet_abort("No."), "No.")
  # a guard given no message still says something
  expect_error(snet_unavailable())
})

test_that("a warning raises a condition under the default verbosity", {
  op <- options(snet_verbosity = "quiet")
  on.exit(options(op), add = TRUE)
  expect_warning(snet_warn("Some values were dropped."), "dropped")
})

test_that("the verbosity levels order the reports", {
  op <- options(snet_verbosity = "quiet")
  on.exit(options(op), add = TRUE)
  expect_silent(snet_info("chose a default"))
  expect_silent(snet_minor_info("a detail"))
  expect_silent(snet_success("done"))

  options(snet_verbosity = "normal")
  expect_message(snet_info("chose a default"), "default")
  expect_message(snet_success("done"), "done")
  expect_silent(snet_minor_info("a detail"))

  # a progress bar reassures that a long task is running
  options(snet_verbosity = "verbose")
  expect_message(snet_info("chose a default"), "default")
  expect_silent(snet_minor_info("a detail"))

  # bookkeeping is exhaustive, so it waits for the level that asks for it
  options(snet_verbosity = "debug")
  expect_message(snet_minor_info("a detail"), "detail")
  expect_message(snet_info("chose a default"), "default")
})

test_that("snet_deprecated() follows the stages of an old name", {
  expect_silent(snet_deprecated("old", "new", "superseded"))
  expect_warning(snet_deprecated("old", "new"), class = "deprecatedWarning")
  expect_warning(snet_deprecated("old", "new", type = "argument"),
                 "argument is deprecated")
  expect_warning(snet_deprecated("x = 1", "x = 2", type = "value",
                                 why = "Two is more."), "Two is more")
  expect_error(snet_deprecated("old", "new", "defunct"), class = "defunctError")
})

test_that("snet_moved() aborts or warns with where the function went", {
  expect_error(snet_moved("old", "new", "netrics", version = "2.0.0"),
               class = "defunctError")
  expect_error(snet_moved("old", "new", "netrics"), "netrics::new")
  expect_warning(snet_moved("old", "new", "netrics", stage = "deprecated"),
                 class = "deprecatedWarning")
})

test_that("snet_requires() aborts for a package that is not installed", {
  expect_invisible(snet_requires("stats"))
  expect_error(snet_requires("notapackageatall", ask = FALSE),
               "install.packages")
  expect_error(snet_requires("notapackageatall", "to test this", ask = FALSE),
               "required to test this")
})

test_that("a renamed argument of to_layer() warns as deprecated", {
  multi <- as_tidygraph(create_filled(5)) |>
    mutate_ties(type = rep(c("friend", "enemy"), 5))
  expect_warning(to_layer(multi, tie = "friend"), class = "deprecatedWarning")
})
