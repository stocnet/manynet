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

  options(snet_verbosity = "verbose")
  expect_message(snet_minor_info("a detail"), "detail")
})
