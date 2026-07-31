# from Traxler et al 2020
fig2 <- manynet::create_explicit(A--B, A--C, B--C, B--D, B--E, B--F, D--E)

test_that("to_correlation works", {
  expect_equal(to_correlation(fig2)["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "diag")["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "recip")["A","F"], 0.6123724, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "all")["A","B"], -0.6324555, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "complex")["A","B"], 0.3162278, tolerance = 0.005)
})

test_that("to_permuted works on matrices", {
  onemode <- matrix(c(0,1,1,0, 1,0,0,1, 0,0,0,1, 1,1,0,0), 4, 4)
  twomode <- matrix(c(1,0,0, 0,1,0, 1,1,0, 0,0,1), 4, 3)
  lab1 <- `dimnames<-`(onemode, list(LETTERS[1:4], LETTERS[1:4]))
  lab2 <- `dimnames<-`(twomode, list(LETTERS[1:4], letters[1:3]))
  # Matrices are permuted directly, but identically to their network form
  expect_identical({set.seed(4); to_permuted(onemode)},
                   {set.seed(4); as_matrix(to_permuted(as_tidygraph(onemode),
                                                       with_attr = FALSE))})
  expect_identical({set.seed(4); to_permuted(lab2)},
                   {set.seed(4); as_matrix(to_permuted(as_tidygraph(lab2),
                                                       with_attr = FALSE))})
  # Ties are shuffled, but their number, and any labels, stay put
  expect_equal(sum({set.seed(4); to_permuted(onemode)}), sum(onemode))
  expect_equal(sum({set.seed(4); to_permuted(twomode)}), sum(twomode))
  expect_null(dimnames({set.seed(4); to_permuted(onemode)}))
  expect_null(dimnames({set.seed(4); to_permuted(twomode)}))
  expect_identical(dimnames({set.seed(4); to_permuted(lab1)}), dimnames(lab1))
  expect_identical(dimnames({set.seed(4); to_permuted(lab2)}), dimnames(lab2))
  # One-mode networks are permuted with a single draw, two-mode with two
  expect_identical({set.seed(4); to_permuted(onemode)},
                   {set.seed(4); n <- sample(4); onemode[n,n]})
  expect_identical({set.seed(4); to_permuted(lab2)},
                   {set.seed(4); n <- sample(4); o <- sample(3)
                    `dimnames<-`(lab2[n,o], dimnames(lab2))})
})
