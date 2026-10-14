# Concept lattices

# Every intent is the intersection of some subset of the rows, the empty subset
# giving the set of all attributes, so listing every subset finds them all.
brute_intents <- function(X){
  n <- nrow(X)
  out <- list(rep(TRUE, ncol(X)))
  for(s in seq_len(2^n - 1)){
    rows <- which(bitwAnd(s, 2^(seq_len(n) - 1)) > 0)
    out[[length(out) + 1]] <- apply(X[rows, , drop = FALSE], 2, all)
  }
  unique(do.call(rbind, out))
}
intent_keys <- function(I) sort(apply(I * 1, 1, paste, collapse = ""))

test_that("to_concepts() finds every concept", {
  set.seed(1)
  for(r in 1:20){
    X <- matrix(stats::runif(8 * 6) < 0.4, 8, 6)
    expect_identical(intent_keys(.concept_intents(X)),
                     intent_keys(brute_intents(X)))
  }
})

test_that("to_concepts() finds the ties directly beneath each concept", {
  # every pair of three objects shares one attribute, which gives the
  # lattice of all subsets of three: 8 concepts and 12 ties
  pairs <- matrix(c(1,1,0, 0,1,1, 1,0,1), 3, 3, byrow = TRUE,
                  dimnames = list(c("x","y","z"), c("a","b","c")))
  out <- to_concepts(pairs)
  expect_equal(dim(out), c(8, 8))
  expect_equal(sum(out), 12)
  # three objects with one attribute each give a diamond: 5 concepts, 6 ties
  diamond <- diag(3)
  dimnames(diamond) <- list(c("x","y","z"), c("a","b","c"))
  out <- to_concepts(diamond)
  expect_equal(dim(out), c(5, 5))
  expect_equal(sum(out), 6)
  # the ties agree with comparing every pair of extents
  set.seed(2)
  for(r in 1:10){
    X <- matrix(stats::runif(12 * 7) < 0.4, 12, 7)
    I <- .concept_intents(X)
    E <- (X * 1) %*% t(I * 1) ==
      matrix(rowSums(I), nrow(X), nrow(I), byrow = TRUE)
    below <- crossprod(E * 1) ==
      matrix(colSums(E), ncol(E), ncol(E), byrow = TRUE)
    diag(below) <- FALSE
    expected <- below & !((below * 1) %*% (below * 1) > 0)
    found <- matrix(FALSE, ncol(E), ncol(E))
    found[as.matrix(.concept_covers(X, E, I))] <- TRUE
    expect_identical(found, expected)
  }
})

test_that("to_concepts() labels each node and affiliation once", {
  out <- to_concepts(as_tidygraph(ison_southern_women))
  expect_true(is_directed(out))
  expect_true(is_acyclic(out))
  nodes <- node_attribute(out, "name")
  expect_false(any(duplicated(nodes)))
  labels <- unlist(strsplit(gsub("[{}]", "", nodes), ", | "))
  labels <- labels[!grepl("^C[0-9]+$", labels)]
  expect_setequal(labels, c(node_names(ison_southern_women)))
  expect_false(any(duplicated(labels)))
  # the top concept holds every woman
  expect_length(node_attribute(out, "extent")[[1]], 18)
  expect_equal(unname(node_attribute(out, "extent_size")[1]), 18)
  expect_equal(net_name(out), "Concept lattice of Southern Women Data")
})

test_that("to_concepts() returns the class it was given", {
  expect_s3_class(to_concepts(ison_southern_women), "stocnet")
  expect_s3_class(to_concepts(as_igraph(ison_southern_women)), "igraph")
  expect_s3_class(to_concepts(as_network(ison_southern_women)), "network")
  expect_true(is.matrix(to_concepts(as_matrix(ison_southern_women))))
})

test_that("to_concepts() dichotomises valued data with a warning", {
  valued <- matrix(c(2,1,0, 0,3,1), 2, 3, byrow = TRUE,
                   dimnames = list(c("x","y"), c("a","b","c")))
  expect_warning(out <- to_concepts(valued), "dichotomised")
  expect_equal(out, to_concepts((valued > 0) * 1))
})

test_that("to_concepts() treats a one-mode network by its out-neighbours", {
  out <- to_concepts(ison_adolescents)
  expect_true(is_directed(out))
  expect_true(is_acyclic(out))
  expect_false(any(duplicated(node_names(out))))
})
