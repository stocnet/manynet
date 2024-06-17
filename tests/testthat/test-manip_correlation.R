# from Traxler et al 2020
fig2 <- manynet::create_explicit(A--B, A--C, B--C, B--D, B--E, B--F, D--E)

test_that("to_correlation works", {
  expect_equal(to_correlation(fig2)["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "diag")["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "recip")["A","F"], 0.6123724, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "all")["A","B"], -0.6324555, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "complex")["A","B"], 0.3162278, tolerance = 0.005)
})
