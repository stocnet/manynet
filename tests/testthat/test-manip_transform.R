# Test transform functions

test_that("to_giant works",{
  expect_equal(network_nodes(ison_marvel_relationships), 53)
  expect_equal(network_nodes(to_giant(ison_marvel_relationships)), 50)
  expect_equal(network_nodes(to_giant(as_igraph(ison_marvel_relationships))), 50)
  expect_equal(network_nodes(to_giant(as_matrix(ison_marvel_relationships))), 50)
  expect_equal(network_nodes(to_giant(as_network(ison_marvel_relationships))), 50)
  expect_equal(network_nodes(to_giant(as_edgelist(ison_marvel_relationships))), 50)
})

test_that("matrix projected correctly by rows",{
  expect_false(is_weighted(ison_southern_women))
  expect_true(is_weighted(to_mode1(ison_southern_women)))
  expect_true(is_weighted(to_mode1(as_igraph(ison_southern_women))))
  expect_true(is_weighted(to_mode1(as_matrix(ison_southern_women))))
  expect_true(is_weighted(to_mode1(as_network(ison_southern_women))))
  expect_true(is_weighted(to_mode1(as_edgelist(ison_southern_women))))
  expect_true(all(node_names(to_mode1(ison_southern_women)) %in% node_names(ison_southern_women)))
  expect_true(length(node_names(to_mode1(ison_southern_women))) != length(node_names(ison_southern_women)))
  expect_equal(length(node_names(to_mode1(ison_southern_women))), length(rownames(as_matrix(ison_southern_women))))
  expect_equal(network_nodes(to_mode1(ison_southern_women, "count")), network_nodes(to_mode1(ison_southern_women, "jaccard")))
  expect_true(is_weighted(to_mode1(ison_southern_women, "pearson")))
  expect_false(tie_weights(to_mode1(ison_southern_women, "rand"))[3] == tie_weights(to_mode1(ison_southern_women, "count"))[3])
})

test_that("matrix projected correctly by columns",{
  expect_false(is_weighted(ison_southern_women))
  expect_true(is_weighted(to_mode2(ison_southern_women)))
  expect_true(is_weighted(to_mode2(as_igraph(ison_southern_women))))
  expect_true(is_weighted(to_mode2(as_matrix(ison_southern_women))))
  expect_true(is_weighted(to_mode2(as_network(ison_southern_women))))
  expect_true(is_weighted(to_mode2(as_edgelist(ison_southern_women))))
  expect_true(all(node_names(to_mode2(ison_southern_women)) %in% node_names(ison_southern_women)))
  expect_true(length(node_names(to_mode2(ison_southern_women))) != length(node_names(ison_southern_women)))
  expect_equal(length(node_names(to_mode2(ison_southern_women))), length(colnames(as_matrix(ison_southern_women))))
  expect_equal(network_nodes(to_mode2(ison_southern_women, "count")), network_nodes(to_mode2(ison_southern_women, "jaccard")))
  expect_true(is_weighted(to_mode2(ison_southern_women, "pearson")))
  expect_false(tie_weights(to_mode2(ison_southern_women, "rand"))[1] == tie_weights(to_mode2(ison_southern_women, "count"))[1])
})

test_that("to matching works", {
  sw <- as_edgelist(to_matching(ison_southern_women))
  expect_equal(network_nodes(to_matching(ison_southern_women)),
               network_nodes(ison_southern_women))
  expect_true(nrow(sw) == nrow(dplyr::distinct(sw)))
})

test_that("to_subgraph works", {
  expect_length(igraph::edge_attr(
    to_subgraph(activate(ison_algebra, "edges"), friends == 1), "friends"), 54)
})

test_that("to ties works", {
  expect_length(to_ties(ison_adolescents), 10)
  expect_length(to_ties(as_igraph(ison_adolescents)), 10)
  expect_length(rownames(to_ties(as_matrix(ison_adolescents))), 10)
  expect_equal(nrow(to_ties(as_edgelist(ison_adolescents))), 10)
  expect_length(network::network.vertex.names(to_ties(as_network(ison_adolescents))), 10)
})

test_that("to anti works", {
  expect_length(to_anti(ison_southern_women), 32)
  expect_length(to_anti(as_igraph(ison_southern_women)), 32)
})

test_that("to, and from, waves work", {
  orig <- ison_adolescents %>%
    activate(edges) %>%
    mutate(wave = sample(1995:1998, 10, replace = TRUE))
  waves <- to_waves(orig, attribute = "wave")
  from_wave <- from_waves(waves)
  expect_length(waves, length(unique(tie_attribute(orig, "wave"))))
  expect_equal(length(from_wave), length(as_igraph(orig)))
})

test_that("to and from slices work", {
  orig <- ison_adolescents %>%
    mutate_ties(time = 1:10, increment = 1) %>%
    add_ties(c(1,2), list(time = 3, increment = -1))
  slice <- to_slices(orig, slice = 7)
  expect_length(slice, length(orig))
  #expect_false(is.null(tie_attribute(slice, "time")))
  ##should attribute names change?
  slices <- ison_adolescents %>%
    mutate_ties(time = 1:10, increment = 1) %>%
    to_slices(slice = c(5,8))
  expect_length(slices, 2)
})
