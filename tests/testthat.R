library(testthat)
library(manynet)

# CRAN asks that a check use no more than two cores at once, and notes tests
# whose time on all cores is much more than the time they take. The tests
# therefore run one at a time there, and in parallel where NOT_CRAN is set,
# as devtools and the CI workflows set it.
if (!identical(Sys.getenv("NOT_CRAN"), "true") &&
    is.na(Sys.getenv("TESTTHAT_PARALLEL", NA)))
  Sys.setenv(TESTTHAT_PARALLEL = "FALSE")

test_check("manynet")
