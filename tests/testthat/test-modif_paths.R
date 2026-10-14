# That to_geodesic() returns the class it was given, and agrees across the
# classes, is tested for the family in test-functional_to.R. What it cannot
# test is the length that each kind of network gives a path.

test_that("to_geodesic() weights a tie by the length of the shortest path", {
  out <- to_geodesic(ison_adolescents)
  expect_true(is_weighted(out))
  expect_false(is_directed(out))
  lengths <- igraph::distances(as_igraph(ison_adolescents))
  expect_equal(unname(as_matrix(out)), unname(lengths))
  expect_equal(as.numeric(net_ties(out)), sum(lengths[upper.tri(lengths)] > 0))
  # a pair that no path joins has no tie
  expect_equal(as.numeric(net_ties(to_geodesic(create_empty(3)))), 0)
  apart <- to_geodesic(as_igraph(data.frame(from = c("A", "C"),
                                            to = c("B", "D"))))
  expect_equal(as.numeric(net_ties(apart)), 2)
})

test_that("to_geodesic() follows the direction of the ties", {
  out <- as_matrix(to_geodesic(create_ring(4, directed = TRUE)))
  expect_equal(out[1, 4], 3)
  expect_equal(out[4, 1], 1)
  expect_true(is_directed(to_geodesic(create_ring(4, directed = TRUE))))
})

test_that("to_geodesic() takes a stronger tie as a shorter step", {
  # A and C are tied directly by a weak tie, and through B by strong ties
  net <- as_igraph(data.frame(from = c("A", "B", "A"), to = c("B", "C", "C"),
                              weight = c(4, 4, 1)))
  out <- as_matrix(to_geodesic(net))
  # each tie costs the mean weight of 3 over its own
  expect_equal(out["A", "C"], 1.5)
  expect_equal(out["A", "B"], 0.75)
})

test_that("to_geodesic() takes only the positive ties as paths", {
  net <- as_igraph(data.frame(from = c("A", "B"), to = c("B", "C"),
                              sign = c(1, -1)))
  out <- as_matrix(to_geodesic(net))
  expect_equal(out["A", "B"], 1)
  expect_equal(out["A", "C"], 0)
})

test_that("to_geodesic() measures paths between the nodes of both modes", {
  out <- to_geodesic(ison_southern_women)
  expect_false(is_twomode(out))
  expect_equal(as.numeric(net_nodes(out)), 32)
  mat <- as_matrix(out)
  expect_equal(mat["Evelyn", "E1"], 1)
  expect_equal(mat["Evelyn", "Laura"], 2)
})

test_that("to_geodesic() measures paths through the hyperedges of a hypergraph", {
  h <- to_hypergraph(as_stocnet(ison_southern_women))
  out <- to_geodesic(h)
  expect_false(is_hypergraph(out))
  expect_equal(as.numeric(net_nodes(out)), 18)
  # the length of a path through hyperedges is the number of them it passes
  expect_equal(as_matrix(out),
               as_matrix(to_geodesic(to_unweighted(to_mode(h, 1)))))
  expect_equal(max(as_matrix(out)), 2)
})
