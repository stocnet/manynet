# from Traxler et al 2020
fig2 <- manynet::create_explicit(A--B, A--C, B--C, B--D, B--E, B--F, D--E)

test_that("to_correlation works", {
  expect_equal(to_correlation(fig2)["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "diag")["A","F"], 0.5773503, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "recip")["A","F"], 0.6123724, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "all")["A","B"], -0.6324555, tolerance = 0.005)
  expect_equal(to_correlation(fig2, "complex")["A","B"], 0.3162278, tolerance = 0.005)
})

test_that("to_correlation is unchanged by delegating to to_proximity", {
  # `to_proximity()` computes the "diag" method, but a correlation matrix
  # carries 1 on its diagonal where a network carries 0
  legacy <- function(M){
    diag(M) <- NA
    out <- suppressWarnings(stats::cor(M, use = "pairwise.complete.obs"))
    out[is.na(out)] <- 0
    diag(out) <- 1
    out
  }
  set.seed(7); n <- 12
  dir <- matrix(stats::rbinom(n * n, 1, 0.3), n, n); diag(dir) <- 0
  und <- ((dir | t(dir)) * 1); diag(und) <- 0
  cpx <- dir; diag(cpx) <- 1
  dimnames(dir) <- dimnames(und) <- dimnames(cpx) <-
    list(LETTERS[1:n], LETTERS[1:n])
  # each fixture takes a different branch, so all four are exercised
  expect_equal(.cor_method(und), "diag")
  expect_equal(.cor_method(dir), "recip")
  expect_equal(.cor_method(cpx), "complex")
  expect_equal(.cor_method(matrix(1, n, 7)), "all")
  for(M in list(und, dir, cpx)) expect_equal(to_correlation(M, "diag"), legacy(M))
  expect_true(all(diag(to_correlation(und)) == 1))
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

# to_proximity() ####

# A small directed network with no symmetry to hide behind.
set.seed(42)
prox_mat <- matrix(stats::rbinom(64, 1, 0.4), 8, 8)
diag(prox_mat) <- 0
dimnames(prox_mat) <- list(LETTERS[1:8], LETTERS[1:8])

test_that("to_proximity matches xUCINET's xStructuralEquivalence", {
  # Values transcribed from xUCINET 0.0.2.0020's `xStructuralEquivalence()` on
  # `prox_mat`, correcting two differences of convention:
  #   - its correlation branch seeds the output with 1s rather than 0s before
  #     adding the transpose, so its Pearson/Spearman/Kendall values are the
  #     coefficient plus 1;
  #   - manynet inverts a distance as 1/(1+d), so that larger always means
  #     more alike; the raw distance is 1/x - 1.
  d <- function(x) 1/x - 1
  # Method = "Euclidean", IncludeTransposed = FALSE, Choiceij = "Ignore"
  expect_equal(d(to_proximity(prox_mat, "euclidean", dyad = "exclude"))["B","A"],
               sqrt(sum((prox_mat[1,-c(1,2)] - prox_mat[2,-c(1,2)])^2)))
  # Method = "AbsDiff": the Manhattan distance over the same cells
  expect_equal(d(to_proximity(prox_mat, "manhattan", dyad = "exclude"))["B","A"],
               sum(abs(prox_mat[1,-c(1,2)] - prox_mat[2,-c(1,2)])))
  # Method = "MatchesN": the count of cells holding the same value
  expect_equal(to_proximity(prox_mat, "match", dyad = "exclude")["B","A"],
               sum(prox_mat[1,-c(1,2)] == prox_mat[2,-c(1,2)]))
  # Method = "Product": the cross-product
  expect_equal(to_proximity(prox_mat, "count", dyad = "exclude")["B","A"],
               sum(prox_mat[1,-c(1,2)] * prox_mat[2,-c(1,2)]))
  # Method = "Pearson", and its two rank counterparts
  for(m in c("pearson", "spearman", "kendall"))
    expect_equal(to_proximity(prox_mat, m, dyad = "exclude")["B","A"],
                 stats::cor(prox_mat[1,-c(1,2)], prox_mat[2,-c(1,2)],
                            method = m))
})

test_that("to_proximity's dyad argument selects the cells compared", {
  i <- 1; j <- 2; third <- -c(1,2)
  # "reciprocal" adds i's tie to j against j's tie to i
  expect_equal(to_proximity(prox_mat, "count", dyad = "reciprocal")["B","A"],
               sum(prox_mat[i,third] * prox_mat[j,third]) +
                 prox_mat[i,j] * prox_mat[j,i])
  # "complex" adds each node's self-tie against the other's
  pm <- prox_mat; diag(pm) <- 1
  expect_equal(to_proximity(pm, "count", dyad = "complex")["B","A"],
               sum(pm[i,third] * pm[j,third]) + pm[i,j] * pm[j,i] +
                 pm[i,i] * pm[j,j])
  # "include" compares every cell where it lies, and so is the vectorised path
  expect_equal(to_proximity(prox_mat, "count", dyad = "include")["B","A"],
               sum(prox_mat[i,] * prox_mat[j,]))
  # the default follows the network's format, as `to_correlation()` does
  expect_equal(to_proximity(prox_mat, "count"),
               to_proximity(prox_mat, "count", dyad = "reciprocal"))
  undir <- prox_mat | t(prox_mat); undir <- undir * 1
  expect_equal(to_proximity(undir, "count"),
               to_proximity(undir, "count", dyad = "exclude"))
})

test_that("to_proximity's across argument selects the profile compared", {
  # "columns" compares in-ties, so it is the row comparison of the transpose
  expect_equal(unname(to_proximity(prox_mat, "count", across = "columns")),
               unname(to_proximity(t(prox_mat), "count", across = "rows")))
  # "both" compares sent and received ties together
  expect_equal(to_proximity(prox_mat, "count", across = "both",
                            dyad = "exclude")["B","A"],
               sum(prox_mat[1,-c(1,2)] * prox_mat[2,-c(1,2)]) +
                 sum(prox_mat[-c(1,2),1] * prox_mat[-c(1,2),2]))
  # all three agree for an undirected network
  undir <- prox_mat | t(prox_mat); undir <- undir * 1
  rows <- to_proximity(undir, "pearson", across = "rows")
  expect_equal(rows, to_proximity(undir, "pearson", across = "columns"))
})

test_that("to_proximity shares to_mode1()'s measures and conventions", {
  expect_setequal(eval(formals(to_proximity)$similarity),
                  eval(formals(to_mode1)$similarity))
  # cosine reduces to ochiai wherever ochiai is defined. Node C sends no ties,
  # so ochiai divides by zero there, whereas cosine reports no similarity
  bin <- prox_mat
  cos <- to_proximity(bin, "cosine", dyad = "include")
  och <- to_proximity(bin, "ochiai", dyad = "include")
  expect_equal(unname(cos)[!is.nan(och)], unname(och)[!is.nan(och)])
  expect_true(all(cos[3, ] == 0) && all(is.nan(och[3, -3])))
  # hamming ranks dyads as rand does, stating disagreement as a count
  rk <- function(x) rank(x[lower.tri(x)])
  expect_equal(rk(to_proximity(bin, "hamming", dyad = "include")),
               rk(to_proximity(bin, "rand", dyad = "include")))
  # a valued network is dichotomised for the binary-only measures. snet_warn()
  # both the pairwise and the vectorised path say so
  expect_warning(to_proximity(prox_mat * 2, "jaccard"), "binary")
  expect_warning(to_proximity(prox_mat * 2, "jaccard", dyad = "include"),
                 "binary")
  expect_equal(suppressWarnings(to_proximity(prox_mat * 2, "jaccard")),
               to_proximity(prox_mat, "jaccard"))
  # the result is square, symmetric, and has a zeroed diagonal
  out <- to_proximity(prox_mat, "pearson")
  expect_equal(dim(out), c(8L, 8L))
  expect_equal(out, t(out))
  expect_true(all(diag(out) == 0))
})

test_that("to_proximity turns two-mode networks away", {
  expect_error(to_proximity(ison_southern_women), "one-mode")
})
