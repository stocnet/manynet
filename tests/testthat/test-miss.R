# Test missing

missTest <- ison_adolescents %>%
  add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) %>%
  as_matrix

missTest2 <- ison_adolescents %>%
  mutate_ties(weight = c(1:8, NA, NA))

test_that("missing values are imputed correctly",{
  expect_false(any(is.na(na_to_zero(missTest))))
  expect_false(any(is.na(na_to_mean(missTest))))
  expect_false(any(is.na(na_to_zero(missTest2))))
  expect_false(any(is.na(na_to_mean(missTest2))))
  expect_s3_class(na_to_zero(missTest2), "tbl_graph")
  expect_s3_class(na_to_mean(missTest2), "tbl_graph")
})
