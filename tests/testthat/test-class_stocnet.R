test_that("stocnet validation", {
  skip_if(format(Sys.time(), "%H") >= "09", message = "Avoid verbose output tests during the day")
  options(snet_verbosity = "verbose")
  expect_s3_class(validate_stocnet(test_stocnet_obj), "stocnet")
  expect_error(exp_class(list(nodes = 1), "nodes", "tbl_df"))
  expect_message(
    reserved_cols(
      .data = list(nodes = tibble::tibble(id = 1)),
      component = "nodes",
      column = "label",
      class = "character",
      aka = c("name", "id")
    ),
    "Columns 'id' might be better called label."
  )
  expect_error(
    reserved_cols(
      .data = list(nodes = tibble::tibble(mode = 1)),
      component = "nodes",
      column = "mode",
      class = "character"
    ),
    "'mode' must be of class 'character'."
  )
  expect_error(
    required_cols(
      .data = list(nodes = tibble::tibble(nabel = 1)),
      component = "nodes",
      column = "label"
    ),
    "The 'nodes' component of a stocnet object must have the following columns: label."
  )
  options(snet_verbosity = "quiet")
})
