test_that("tie_lengths() returns lengths in tie order, and 1 where absent", {
  g <- igraph::sample_gnm(20, 40)
  igraph::E(g)$length <- seq_len(40)
  expect_equal(as.numeric(tie_lengths(g)), as.numeric(seq_len(40)))
  expect_equal(as.numeric(tie_lengths(as_tidygraph(g))), as.numeric(seq_len(40)))
  expect_equal(as.numeric(tie_lengths(as_stocnet(g))), as.numeric(seq_len(40)))
  expect_equal(as.numeric(tie_lengths(create_ring(5))), rep(1, 5))
})

test_that("a length is neither a weight nor read as one", {
  net <- create_ring(4) |> mutate_ties(length = c(1, 2.5, 1, 2.5))
  expect_false(is_weighted(net))
  expect_equal(as.numeric(tie_weights(net)), rep(1, 4))
  expect_equal(as.numeric(tie_weights(to_weighted(net, "length"))),
               c(1, 2.5, 1, 2.5))
  expect_error(to_weighted(net, "nonesuch"), "numeric tie attribute")
})

test_that("a stocnet with lengths and weights keeps both through coercion", {
  net <- make_stocnet(nodes = dplyr::tibble(label = c("a", "b", "c")),
                      ties = dplyr::tibble(from = 1:3, to = c(2L, 3L, 1L),
                                           length = c(2.5, 1.2, 3),
                                           weight = c(5, 1, 2)),
                      info = list(directed = FALSE))
  for(f in list(as_igraph, as_tidygraph, as_network, as_edgelist)){
    back <- as_stocnet(f(net))
    expect_equal(back$ties$length, net$ties$length)
    expect_equal(back$ties$weight, net$ties$weight)
  }
})

test_that("a data frame keeps a third 'length' column as a length", {
  el <- data.frame(from = c("a", "b", "c"), to = c("b", "c", "a"),
                   length = c(2.5, 1.2, 3))
  expect_false(is_weighted(el))
  expect_equal(igraph::edge_attr_names(as_igraph(el)), "length")
  expect_named(as_edgelist(el), c("from", "to", "length"))
  expect_true(all(as_matrix(el) %in% c(0, 1)))
})

test_that("a 'distance' tie column is flagged as a length", {
  expect_warning(make_stocnet(nodes = dplyr::tibble(label = c("a", "b")),
                              ties = dplyr::tibble(from = 1L, to = 2L,
                                                   distance = 3)),
                 "ties\\$length")
})

test_that("to_backbone() reads a short tie as a strong one", {
  set.seed(1)
  net <- as_stocnet(igraph::sample_gnm(30, 120))
  net$ties$length <- stats::runif(nrow(net$ties), 1, 100)
  inverse <- net
  inverse$ties$weight <- 1 / inverse$ties$length
  inverse$ties$length <- NULL
  expect_equal(suppressMessages(to_backbone(net))$ties$length,
               net$ties$length[suppressMessages(
                 tie_is_backbone(inverse))])
})

test_that("tie_nodes() counts the nodes that each tie joins", {
  # A tie between two nodes joins 2, whether one-mode or two-mode.
  expect_true(all(tie_nodes(ison_adolescents) == 2))
  expect_true(all(tie_nodes(ison_southern_women) == 2))
  expect_length(tie_nodes(ison_southern_women), net_ties(ison_southern_women))
  # A loop joins only the one node, in any class.
  loops <- igraph::add_edges(create_ring(4), c(1, 1))
  expect_equal(as.numeric(tie_nodes(loops)), c(2, 2, 2, 2, 1))
  expect_equal(as.numeric(tie_nodes(as_stocnet(loops))), c(2, 2, 2, 2, 1))
  # A hyperedge joins its members, and is named by its label.
  hyper <- to_hypergraph(ison_southern_women)
  res <- tie_nodes(hyper)
  expect_length(res, net_ties(hyper))
  expect_equal(names(res)[1:2], c("E1", "E2"))
  expect_equal(unname(as.numeric(res)),
               unname(colSums(as_matrix(ison_southern_women))))
  # A hyperedge may have one member or none.
  odd <- make_stocnet(nodes = data.frame(label = letters[1:4]),
                      ties = dplyr::tibble(from = NA_integer_, to = NA_integer_,
                                           with = list(1:3, integer(0), 4L)))
  expect_equal(as.numeric(tie_nodes(odd)), c(3, 0, 1))
})
