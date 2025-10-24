test_that("net_dims works", {
  expect_equal(net_dims(matrix(0,3,4)), c(3,4))
  expect_equal(net_dims(as_network(matrix(0,3,4))), c(3,4))
})
