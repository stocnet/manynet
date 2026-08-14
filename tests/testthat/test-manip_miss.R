# Test missing

missTest <- ison_adolescents %>%
  add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) %>%
  as_matrix

missTest2 <- ison_adolescents %>%
  mutate_ties(weight = c(1:8, NA, NA))

test_that("missing values are imputed correctly",{
  expect_false(anyNA(na_to_zero(missTest)))
  expect_false(anyNA(na_to_mean(missTest)))
  expect_false(anyNA(na_to_zero(missTest2)))
  expect_false(anyNA(na_to_mean(missTest2)))
  expect_s3_class(na_to_zero(missTest2), "tbl_graph")
  expect_s3_class(na_to_mean(missTest2), "tbl_graph")
})

# Regression tests for the bugs the family carried ####

test_that("na_to_zero sets missing weights to zero instead of deleting ties", {
  # it used to filter out NA-weighted ties, which contradicted both its
  # documentation and its own matrix and edgelist methods
  expect_equal(c(net_ties(na_to_zero(missTest2))), c(net_ties(missTest2)))
  expect_equal(sum(tie_weights(na_to_zero(missTest2)) == 0),
               sum(is.na(tie_weights(missTest2))))
})

test_that("na_to_zero leaves an unweighted network alone", {
  # it used to raise "`..1` must be of size 10 or 1, not size 0" on any
  # network without a weight column, since there was nothing to test for NA
  expect_no_error(na_to_zero(ison_adolescents))
  expect_equal(as_matrix(na_to_zero(ison_adolescents)),
               as_matrix(ison_adolescents))
})

test_that("na_to_mean works on the example in its own documentation", {
  # any(tie_weights(.data) > 1) is NA when a weight is NA, so the if() raised
  # "missing value where TRUE/FALSE needed" whenever every weight was <= 1
  expect_no_error(na_to_mean(missTest))
  expect_no_error(na_to_mean(missTest2))
})

test_that("na_to_mean imputes binary networks rather than passing them over", {
  # the tbl_graph branch iterated over seq_len() of the weight vector and
  # tested is.na() on the index, so it never imputed anything
  binary <- ison_adolescents |>
    mutate_ties(weight = c(1, NA, 1, NA, 1, 1, 1, 1, 1, 1))
  set.seed(1234)
  out <- na_to_mean(binary)
  expect_false(anyNA(tie_weights(out)))
  expect_true(all(tie_weights(out) %in% c(0, 1)))
})

test_that("na_to_mean is reproducible given a seed", {
  binary <- ison_adolescents |>
    mutate_ties(weight = c(1, NA, 1, NA, 1, 1, 1, 1, 1, 1))
  set.seed(1234)
  first <- tie_weights(na_to_mean(binary))
  set.seed(1234)
  expect_equal(tie_weights(na_to_mean(binary)), first)
})

test_that("na_to_mean excludes the diagonal from the average", {
  # a node's tie to itself is not a tie that could have been observed, so
  # counting the n structural zeros biased the density down by (n-1)/n
  n <- nrow(missTest)
  offdiag <- missTest
  diag(offdiag) <- NA
  expect_equal(manynet:::.miss_average(missTest), mean(offdiag, na.rm = TRUE))
  expect_false(isTRUE(all.equal(manynet:::.miss_average(missTest),
                                mean(missTest, na.rm = TRUE))))
})

test_that("na_to_* leave a network without missing values alone", {
  expect_equal(na_to_zero(as_matrix(ison_adolescents)),
               as_matrix(ison_adolescents))
  expect_equal(na_to_mean(as_matrix(ison_adolescents)),
               as_matrix(ison_adolescents))
})
