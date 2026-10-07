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

test_that("generate_utilities keeps utilities between -1 and 1", {
  set.seed(1234)
  for (form in c("normal", "uniform", "relative")) {
    out <- generate_utilities(10, form = form)
    expect_s3_class(out, "stocnet")
    expect_true(is_directed(out))
    expect_true(is_signed(out))
    expect_true(is_weighted(out))
    # a node has no utility for itself, and one for every other node
    expect_true(all(diag(as_matrix(out)) == 0))
    expect_equal(as.numeric(net_ties(out)), 90)
    weights <- unlist(lapply(1:20, function(i)
      tie_weights(generate_utilities(10, form = form))))
    expect_true(all(abs(weights) <= 1))
    expect_true(any(weights < 0) && any(weights > 0))
  }
  # each node's strongest utility is the measure of its others
  relative <- abs(as_matrix(generate_utilities(10, form = "relative")))
  expect_equal(apply(relative, 1, max), rep(1, 10), ignore_attr = TRUE)
})

test_that("generate_utilities leaves out utilities within the threshold", {
  set.seed(1234)
  all <- generate_utilities(20)
  some <- generate_utilities(20, threshold = 0.5)
  expect_lt(as.numeric(net_ties(some)), as.numeric(net_ties(all)))
  expect_true(all(abs(tie_weights(some)) > 0.5))
  expect_true(any(tie_weights(some) < 0) && any(tie_weights(some) > 0))
  expect_equal(as.numeric(net_ties(generate_utilities(6, threshold = 1))), 0)
  expect_error(generate_utilities(6, threshold = 2), "threshold")
  expect_error(generate_utilities(6, steps = 0), "steps")
  expect_error(generate_utilities(6, volatility = -1), "volatility")
})

test_that("generate_utilities takes two-mode sizes and networks", {
  set.seed(1234)
  twomode <- generate_utilities(c(4, 6))
  expect_true(is_twomode(twomode))
  expect_equal(as.numeric(net_dims(twomode)), c(4, 6))
  expect_equal(as.numeric(net_ties(twomode)), 24)
  expect_equal(node_names(generate_utilities(ison_adolescents)),
               node_names(ison_adolescents))
  women <- generate_utilities(ison_southern_women)
  expect_true(is_twomode(women))
  expect_equal(node_names(women), node_names(ison_southern_women))
})

test_that("generate_utilities returns a wave for each step", {
  set.seed(1234)
  out <- generate_utilities(6, steps = 5, volatility = 0.2)
  expect_s3_class(out, "stocnet")
  expect_true(is_longitudinal(out))
  expect_equal(as.numeric(net_nodes(out)), 6)
  waves <- lapply(to_waves(out), as_matrix)
  expect_length(waves, 5)
  expect_false(isTRUE(all.equal(waves[[1]], waves[[5]])))
  expect_true(all(abs(unlist(waves)) <= 1))
  # without volatility the utilities do not change
  still <- lapply(to_waves(generate_utilities(6, steps = 3, volatility = 0)),
                  as_matrix)
  expect_equal(still[[1]], still[[3]], ignore_attr = TRUE)
  expect_true(is_twomode(generate_utilities(c(4, 6), steps = 3)))
  # a step adds a new draw, scaled by the volatility, to the utilities
  moved <- lapply(to_waves(generate_utilities(10, form = "uniform", steps = 2,
                                              volatility = 0.5)), as_matrix)
  expect_true(all(abs(moved[[2]] - moved[[1]]) <= 0.5 + 1e-8))
  expect_gt(max(abs(moved[[2]] - moved[[1]])), 0.3)
})

test_that("generate_utilities holds utilities back with inertia", {
  set.seed(1234)
  changed <- function(inertia) {
    waves <- lapply(to_waves(generate_utilities(10, steps = 2,
                                                inertia = inertia)), as_matrix)
    sum(waves[[1]] != waves[[2]])
  }
  expect_equal(changed(0), 90)
  expect_equal(changed(0.8), 18)
  expect_equal(changed(1), 0)
  # the utilities held back are those that would have changed the least
  set.seed(1234)
  free <- lapply(to_waves(generate_utilities(10, steps = 2)), as_matrix)
  set.seed(1234)
  held <- lapply(to_waves(generate_utilities(10, steps = 2, inertia = 0.8)),
                 as_matrix)
  moves <- abs(free[[2]] - free[[1]])
  expect_true(min(moves[held[[2]] != held[[1]]]) >=
                max(moves[held[[2]] == held[[1]]]))
  expect_error(generate_utilities(6, inertia = 2), "inertia")
  expect_equal(c(table(tie_attribute(generate_utilities(c(4, 6), steps = 3,
                                                        inertia = 0.5),
                                     "time"))), rep(24, 3), ignore_attr = TRUE)
})

test_that("generate_utilities can be read as the ties nodes want", {
  set.seed(1234)
  out <- generate_utilities(8)
  wanted <- to_unsigned(out, keep = "positive")
  expect_false(is_signed(wanted))
  expect_equal(as.numeric(net_ties(wanted)), sum(tie_weights(out) > 0))
  agreed <- to_unsigned(to_undirected(out, rule = "min"), keep = "positive")
  expect_false(is_directed(agreed))
  mat <- as_matrix(out)
  expect_equal(as.numeric(net_ties(agreed)),
               sum((mat > 0 & t(mat) > 0)[upper.tri(mat)]))
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

test_that("generate_islands adds a bridge for each pair of islands", {
  # both branches tie each pair of islands, so the count of bridge ties grows
  # as `choose(islands, 2)`, not as `islands`. The `p` inference subtracts it.
  for(k in c(2, 3, 4, 6)){
    onemode <- igraph::sample_islands(islands.n = k, islands.size = 10,
                                      islands.pin = 0, n.inter = 1)
    expect_equal(igraph::ecount(onemode), choose(k, 2))
    twomode <- generate_islands(c(10 * k, 10 * k), islands = k, p = 0,
                                bridges = 1)
    expect_equal(as.numeric(net_ties(twomode)), choose(k, 2))
  }
})

test_that("generate_islands records the island of each node", {
  set.seed(1234)
  onemode <- generate_islands(12, islands = 3)
  expect_equal(c(table(node_attribute(onemode, "community"))),
               c(4, 4, 4), ignore_attr = TRUE)
  # surplus nodes are deleted, so the attribute must shrink with the network
  expect_length(node_attribute(generate_islands(10, islands = 3), "community"),
                10)
  twomode <- generate_islands(c(40, 20), islands = 4)
  expect_equal(c(table(node_attribute(twomode, "community"))),
               rep(15, 4), ignore_attr = TRUE)
})

test_that("generate_communities plants communities of unequal size", {
  set.seed(1234)
  out <- generate_communities(1000, degree = 15, max_degree = 50,
                              community = c(20, 50), mixing = 0.1)
  memb <- node_attribute(out, "community")
  degs <- igraph::degree(as_igraph(out))
  ties <- igraph::as_edgelist(as_igraph(out), names = FALSE)
  expect_true(igraph::is_simple(as_igraph(out)))
  expect_false(is_directed(out))
  expect_equal(mean(degs), 15, tolerance = 0.1)
  expect_lte(max(degs), 50)
  expect_true(all(table(memb) >= 20 & table(memb) <= 50))
  expect_gt(length(unique(table(memb))), 1)
  # the share of ties between communities is the mixing asked for
  expect_equal(mean(memb[ties[, 1]] != memb[ties[, 2]]), 0.1,
               tolerance = 0.1)
})

test_that("generate_communities are harder to tell apart with more mixing", {
  set.seed(1234)
  between <- vapply(c(0, 0.3, 0.6), function(mu) {
    out <- generate_communities(500, mixing = mu)
    memb <- node_attribute(out, "community")
    ties <- igraph::as_edgelist(as_igraph(out), names = FALSE)
    mean(memb[ties[, 1]] != memb[ties[, 2]])
  }, numeric(1))
  expect_equal(between[1], 0)
  expect_true(all(diff(between) > 0.2))
})

test_that("generate_communities works on small networks and checks arguments", {
  set.seed(1234)
  # degree sequences that no simple network has must not stall the function
  for (n in c(3, 4, 5, 6, 7, 10)) for (i in 1:20)
    expect_equal(as.numeric(net_nodes(generate_communities(n))), n)
  expect_error(generate_communities(2), "At least 3")
  expect_error(generate_communities(50, mixing = 2), "mixing")
  expect_error(generate_communities(50, degree = 10, max_degree = 5),
               "max_degree")
  expect_error(generate_communities(50, community = c(30, 10)), "community")
})

test_that("generate_communities plants communities in two-mode networks", {
  set.seed(1234)
  out <- generate_communities(c(600, 400), degree = 10, mixing = 0.1)
  expect_true(is_twomode(out))
  expect_equal(as.numeric(net_dims(out)), c(600, 400))
  ig <- as_igraph(out)
  memb <- node_attribute(out, "community")
  modes <- igraph::V(ig)$type
  degs <- igraph::degree(ig)
  ties <- igraph::as_edgelist(ig, names = FALSE)
  expect_true(igraph::is_simple(ig))
  expect_true(all(modes[ties[, 1]] != modes[ties[, 2]]))
  # both modes have the same ties, so the second mode's degree follows
  expect_equal(mean(degs[!modes]), 10, tolerance = 0.1)
  expect_equal(mean(degs[modes]), 15, tolerance = 0.1)
  # every community has nodes of both modes
  expect_true(all(table(memb, modes) > 0))
  expect_gt(length(unique(memb)), 5)
  expect_equal(mean(memb[ties[, 1]] != memb[ties[, 2]]), 0.1,
               tolerance = 0.1)
  more <- generate_communities(c(600, 400), degree = 10, mixing = 0.6)
  memb <- node_attribute(more, "community")
  ties <- igraph::as_edgelist(as_igraph(more), names = FALSE)
  expect_equal(mean(memb[ties[, 1]] != memb[ties[, 2]]), 0.6,
               tolerance = 0.1)
})

test_that("generate_communities works on small two-mode networks", {
  set.seed(1234)
  # dense ties must not stall the function, whatever the shape of the network
  for (n in list(c(1, 1), c(2, 2), c(4, 6), c(3, 10), c(10, 3), c(20, 20)))
    for (mu in c(0, 0.5, 1)) for (i in 1:5) {
      out <- suppressWarnings(generate_communities(n, mixing = mu))
      expect_equal(as.numeric(net_dims(out)), n)
      expect_true(all(table(node_attribute(out, "community"),
                            igraph::V(as_igraph(out))$type) > 0))
    }
  expect_equal(as.numeric(net_dims(generate_communities(ison_southern_women))),
               c(18, 14))
  expect_error(generate_communities(c(10, 10), degree = 20), "second mode")
  expect_error(generate_communities(c(10, 10), max_degree = 50), "max_degree")
  expect_error(generate_communities(c(3, 100), community = c(5, 20)),
               "both modes")
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
