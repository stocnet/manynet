# Tests for the generate family of functions.
# Family-wide conventions (node counts, one-/two-mode `n`, directedness,
# output class) are tested generically for every generate_*() function in
# test-functional_makes.R; here we test each function's specific semantics.

test_that("random creation works", {
  expect_false(isTRUE(all.equal(generate_random(4,0.3), generate_random(4,0.3))))
  expect_false(isTRUE(all.equal(generate_random(c(2,4),0.3), generate_random(c(2,4),0.3))))
  expect_error(generate_random(c(1,2,3)), "must be of length")
  # Bipartite graph
  expect_s3_class(generate_random(ison_southern_women, 0.4), "igraph")
  expect_true(is_twomode(generate_random(ison_southern_women, 0.4)))
})

test_that("generate_smallworld() works", {
  expect_s3_class(generate_smallworld(12, 0.025), "igraph")
  expect_equal(igraph::vcount(generate_smallworld(12, 0.025)), 12)
  expect_s3_class(generate_smallworld(c(6,6), 0.025), "igraph")
})

test_that("generate_scalefree() works", {
  expect_s3_class(generate_scalefree(12, 0.025), "igraph")
  expect_s3_class(generate_scalefree(c(6,6), 0.025), "igraph")
})

test_that("generate_configuration works", {
  expect_s3_class(generate_configuration(ison_adolescents), "igraph")
  expect_s3_class(generate_configuration(ison_southern_women), "igraph")
})

test_that("generate_man works", {
  expect_s3_class(generate_man(ison_adolescents), "igraph")
  expect_s3_class(generate_man(ison_southern_women), "igraph")
})

test_that("generate_man works without a dyad census", {
  onemode <- generate_man(6)
  expect_equal(as.numeric(net_nodes(onemode)), 6)
  expect_false(is_twomode(onemode))
  twomode <- generate_man(c(4, 6))
  expect_equal(as.numeric(net_nodes(twomode)), 10)
  expect_true(is_twomode(twomode))
  expect_error(generate_man(6, man = c(1, 2)), "length 3")
})

test_that("generate_fire works", {
  expect_s3_class(generate_fire(ison_adolescents), "igraph")
  fire <- generate_fire(c(20, 10))
  expect_true(is_twomode(fire))
  expect_equal(as.numeric(net_nodes(fire)), 30)
  expect_true(is_twomode(generate_fire(ison_southern_women)))
  # `their_out` is the burn probability, so raising it spreads the fire
  expect_gt(mean(replicate(10, net_ties(generate_fire(c(20, 10),
                                                      their_out = 0.5)))),
            mean(replicate(10, net_ties(generate_fire(c(20, 10))))))
})

test_that("generate_islands works", {
  expect_s3_class(generate_islands(ison_adolescents), "igraph")
  isles <- generate_islands(c(40, 20), islands = 4)
  expect_true(is_twomode(isles))
  expect_equal(as.numeric(net_nodes(isles)), 60)
  # the diagonal blocks must be much denser than the off-diagonal blocks
  mat <- as_matrix(isles)
  same <- outer(cut(seq_len(40), 4, labels = FALSE),
                cut(seq_len(20), 4, labels = FALSE), "==")
  expect_gt(mean(mat[same]), mean(mat[!same]) + 0.2)
  expect_true(is_twomode(generate_islands(ison_southern_women)))
})

test_that("generate_citations works", {
  expect_s3_class(generate_citations(ison_adolescents), "igraph")
  cites <- generate_citations(c(20, 10))
  expect_true(is_twomode(cites))
  expect_equal(as.numeric(net_nodes(cites)), 30)
  expect_true(is_twomode(generate_citations(ison_southern_women)))
  # recency concentrates ties on some second-mode nodes more than chance does
  gini <- function(x) {
    x <- sort(x)
    sum((2 * seq_along(x) - length(x) - 1) * x) / (length(x) * sum(x))
  }
  expect_gt(mean(replicate(10, gini(colSums(
    as_matrix(generate_citations(c(200, 40), ties = 2)))))),
    mean(replicate(10, gini(colSums(
      matrix(stats::rbinom(200 * 40, 1, 2/40), 200, 40))))))
})

test_that("generate_configuration reads the modes of a stocnet", {
  # a stocnet marks its modes in 'mode', where an igraph uses 'type'
  sw <- as_stocnet(ison_southern_women)
  out <- generate_configuration(sw)
  expect_true(is_twomode(out))
  expect_equal(as.numeric(net_nodes(out)), as.numeric(net_nodes(sw)))
})
