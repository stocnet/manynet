test_that("stocnet validation", {
  withr::local_options(snet_verbosity = "verbose")
  expect_s3_class(validate_stocnet(test_stocnet_obj), "stocnet")
  expect_error(exp_class(list(nodes = 1), "nodes", "tbl_df"))
  expect_message(
    res_cols(
      .data = list(nodes = tibble::tibble(id = 1)),
      component = "nodes",
      reserved_cols = "label",
      class = "character",
      aka = c("name", "id")
    ),
    "Columns 'id' might be better called label."
  )
  expect_error(
    res_cols(
      .data = list(nodes = tibble::tibble(mode = 1)),
      component = "nodes",
      reserved_cols = "mode",
      class = "character"
    ),
    "'mode' must be of class 'character'."
  )
  expect_error(
    req_cols(
      .data = list(nodes = tibble::tibble(nabel = 1)),
      component = "nodes",
      required_cols = "label"
    ),
    "The 'nodes' component of a stocnet object must have the following columns: label."
  )
})
