test_that("table_data returns a tibble with expected columns", {
  result <- table_data(pkg = "manynet")
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("dataset", "nodes", "ties", "directed") %in% names(result)))
})