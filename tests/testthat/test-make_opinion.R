# That play_opinion() runs and returns what it should
# is tested for the family in test-functional_makes.R.

# play_learning() as it was before 2.5.0, to check that old calls give the
# same values. It sets a node with no one inside `closeness` to 0, which
# play_opinion() does not, so the networks below give no node that case.
old_learning <- function(.data, beliefs, closeness = Inf, steps,
                         epsilon = 0.0005){
  n <- net_nodes(.data)
  if(missing(steps)) steps <- n
  t <- 0
  out <- matrix(NA, steps + 1, length(beliefs))
  out[1, ] <- beliefs
  trust_mat <- as_matrix(.data)/rowSums(as_matrix(.data))
  repeat{
    old_beliefs <- beliefs
    listen_mat <- trust_mat * (as.matrix(stats::dist(beliefs)) <= closeness)
    listen_mat <- listen_mat/rowSums(listen_mat)
    listen_mat[is.nan(listen_mat)] <- 0
    beliefs <- listen_mat %*% beliefs
    if(all(abs(old_beliefs - beliefs) < epsilon)) break
    t <- t + 1
    out[t + 1, ] <- beliefs
    if(t == steps) break
  }
  stats::na.omit(out)
}

final <- function(net, opinions = "opinions"){
  out <- as_opinion(net, opinions)
  unlist(out[nrow(out), ], use.names = FALSE)
}

path <- create_explicit(A-B-C)
first <- c(0, 0.5, 1)

# The deprecated wrapper ####

test_that("play_learning() warns and gives the same values as before", {
  set.seed(1)
  beliefs <- stats::runif(net_nodes(ison_networkers))
  expect_warning(out <- play_learning(ison_networkers, beliefs), "deprecated")
  expect_s3_class(out, "opinionModel")
  expect_s3_class(out, "learn_model")
  expect_equal(unname(as.matrix(out)),
               old_learning(ison_networkers, beliefs), ignore_attr = TRUE)
  expect_named(out, node_labels(ison_networkers))
})

test_that("play_learning() gives the same values for steps and epsilon", {
  beliefs <- (1:8)/8
  expect_warning(out <- play_learning(create_ring(8), beliefs,
                                      epsilon = 0.01, steps = 50))
  expect_equal(unname(as.matrix(out)),
               old_learning(create_ring(8), beliefs, epsilon = 0.01,
                            steps = 50), ignore_attr = TRUE)
})

test_that("play_learning() gives the same values for a finite closeness", {
  set.seed(3)
  beliefs <- stats::runif(10)
  net <- create_filled(10)
  expect_warning(out <- play_learning(net, beliefs, closeness = 0.5))
  expect_equal(unname(as.matrix(out)),
               old_learning(net, beliefs, closeness = 0.5), ignore_attr = TRUE)
})

# Stubbornness and self-weight ####

test_that("stubbornness 0 is DeGroot's model", {
  set.seed(1)
  x <- stats::runif(net_nodes(ison_networkers))
  out <- as_opinion(play_opinion(ison_networkers, x, tolerance = 0.0005))
  old <- old_learning(ison_networkers, x)
  expect_equal(dim(out), dim(old))
  # The record leaves out the changes smaller than the tolerance
  expect_true(all(abs(as.matrix(out) - old) <= 0.0005))
})

test_that("the path example on the help page gives its values", {
  run <- function(net, ...) final(play_opinion(net, first, steps = 2000,
                                               tolerance = 1e-10, ...))
  expect_equal(run(path), c(0.5, 0.5, 0.5), tolerance = 1e-6)
  expect_equal(run(path, stubbornness = 0.5), c(0.25, 0.5, 0.75),
               tolerance = 1e-6)
  expect_equal(run(path, stubbornness = c(1, 0, 0)), c(0, 0, 0),
               tolerance = 1e-6)
  expect_equal(run(path, stubbornness = c(2/3, 0, 0)), c(0, 0, 0),
               tolerance = 1e-6)
  W <- rbind(c(1, 0, 0), c(0.5, 0, 0.5), c(0, 1, 0))
  expect_equal(run(path, weights = W), c(0, 0, 0), tolerance = 1e-6)
  loopy <- igraph::make_graph(~A-B-C)
  igraph::E(loopy)$weight <- 1
  loopy <- igraph::add_edges(loopy, c(1, 1), weight = 2)
  expect_equal(run(loopy), rep(1/3, 3), tolerance = 1e-6)
})

test_that("a fully stubborn node keeps its first opinion and pulls the rest", {
  set.seed(2)
  x <- stats::runif(net_nodes(ison_adolescents))
  s <- c(1, rep(0, 7))
  out <- as_opinion(play_opinion(ison_adolescents, x, stubbornness = s,
                                 steps = 2000, tolerance = 1e-10))
  expect_true(all(out[[1]] == x[1]))
  expect_equal(unlist(out[nrow(out), ], use.names = FALSE), rep(x[1], 8),
               tolerance = 1e-6)
})

test_that("partial stubbornness keeps disagreement, but self-loops do not", {
  stubborn <- play_opinion(path, first, stubbornness = c(0.5, 0, 0.5),
                           steps = 2000, tolerance = 1e-10)
  expect_gt(diff(range(final(stubborn))), 0.1)
  loopy <- igraph::make_graph(~A-B-C)
  igraph::E(loopy)$weight <- 1
  loopy <- igraph::add_edges(loopy, c(1, 1, 3, 3), weight = c(1, 1))
  selfish <- play_opinion(loopy, first, steps = 2000, tolerance = 1e-10)
  expect_lt(diff(range(final(selfish))), 1e-6)
})

test_that("stubbornness can be the name of a node attribute", {
  net <- add_node_attribute(path, "stub", c(1, 0, 0))
  expect_equal(final(play_opinion(net, first, stubbornness = "stub",
                                  steps = 2000, tolerance = 1e-10)),
               c(0, 0, 0), tolerance = 1e-6)
})

# Rules ####

test_that("hegselmann_krause is deterministic, and deffuant repeats by seed", {
  set.seed(1)
  x <- stats::runif(net_nodes(ison_networkers))
  h1 <- play_opinion(ison_networkers, x, "hegselmann_krause",
                     confidence = 0.2, steps = 100)
  h2 <- play_opinion(ison_networkers, x, "hegselmann_krause",
                     confidence = 0.2, steps = 100)
  expect_identical(as_opinion(h1), as_opinion(h2))
  set.seed(5)
  d1 <- play_opinion(ison_networkers, x, "deffuant", confidence = 0.3,
                     steps = 500)
  set.seed(5)
  d2 <- play_opinion(ison_networkers, x, "deffuant", confidence = 0.3,
                     steps = 500)
  expect_identical(as_opinion(d1), as_opinion(d2))
  # Only one pair moves at each step
  expect_true(all(table(as_changelist(d1)$time) <= 2))
})

test_that("confidence warns where the rule does not use it", {
  expect_warning(play_opinion(path, first, confidence = 0.1),
                 "bounded-confidence")
})

# Opinions ####

test_that("opinions can be a node attribute, and its record continues", {
  net <- add_node_attribute(create_ring(5), "view", 1:5/5)
  out <- play_opinion(net, "view", steps = 3)
  expect_equal(unique(as_changelist(out)$var), "view")
  expect_equal(as.numeric(node_attribute(out, "view")), 1:5/5)
  more <- play_opinion(out, "view", steps = 3)
  expect_equal(max(as_changelist(more)$time), 6)
})

test_that("opinions can be a matrix of nodes by issues", {
  m <- cbind(left = first, right = rev(first))
  out <- play_opinion(path, m)
  expect_setequal(unique(as_changelist(out)$var), c("left", "right"))
  expect_equal(final(out, "left"), c(0.5, 0.5, 0.5), tolerance = 0.001)
})

test_that("the record has rows only for changes larger than the tolerance", {
  out <- play_opinion(path, first, stubbornness = c(0, 1, 0), steps = 100,
                      tolerance = 0.01)
  changes <- as_changelist(out)
  expect_false(any(changes$node == 2))
  expect_true(all(abs(diff(as.matrix(as_opinion(out)))) <= 0.5))
})

test_that("play_opinion() keeps the class it was given", {
  x <- stats::runif(8)
  expect_s3_class(play_opinion(as_igraph(ison_adolescents), x), "igraph")
  expect_s3_class(play_opinion(as_tidygraph(ison_adolescents), x), "tbl_graph")
  expect_s3_class(play_opinion(ison_adolescents, x), "stocnet")
  expect_s3_class(play_opinion(as_matrix(ison_adolescents), x), "stocnet")
})

test_that("play_opinion() explains what it cannot use", {
  expect_error(play_opinion(path, 1:2), "one value for each")
  expect_error(play_opinion(path, "nothing"), "no node attribute")
  expect_error(play_opinion(path, c(0, NA, 1)), "missing")
  expect_error(play_opinion(path, first, stubbornness = 2), "from 0 to 1")
  expect_error(play_opinion(path, first, weights = diag(2)), "one row")
  expect_error(play_opinion(path, first, weights = matrix(1, 3, 3)),
               "sum to 1")
  expect_error(play_opinion(path), "Please give the opinions")
})

test_that("as_opinion() of a network with no record has one row", {
  net <- add_node_attribute(path, "opinions", first)
  out <- as_opinion(net)
  expect_s3_class(out, "opinionModel")
  expect_equal(nrow(out), 1)
})
