# Hypergraphs: the hyperedges a 'stocnet' holds in 'with', the classes it is
# coerced to and from, and the formats it is read from and written to.

hyper_fixtures <- function() {
  nodes <- data.frame(label = LETTERS[1:5], age = c(31, 42, 53, 64, 75))
  list(
    undirected = make_stocnet(
      info = list(directed = FALSE), nodes = nodes,
      ties = dplyr::tibble(with = list(c("A", "B", "C"), c("C", "D")),
                           label = c("e1", "e2"), weight = c(2, 3))),
    directed = make_stocnet(
      info = list(directed = TRUE), nodes = nodes,
      ties = dplyr::tibble(with = list(c("A", "B", "C"), c("C", "D")),
                           label = c("e1", "e2"),
                           with_direction = list(c("from", "from", "to"),
                                                 c("from", "to")))),
    weighted = make_stocnet(
      info = list(directed = FALSE), nodes = nodes,
      ties = dplyr::tibble(with = list(c("A", "B", "C"), c("C", "D")),
                           label = c("e1", "e2"),
                           with_weight = list(c(0.1, 0.2, 0.3), c(0.5, 0.6)),
                           with_role = list(c("x", "y", "z"), c("x", "x")))),
    mixed = make_stocnet(
      info = list(directed = c(friend = TRUE, meet = FALSE),
                  layers = c("friend", "meet")), nodes = nodes,
      ties = dplyr::tibble(from = c("A", "B", NA, NA),
                           to = c("B", "C", NA, NA),
                           with = list(NULL, NULL, c("A", "B", "C"),
                                       c("C", "D", "E")),
                           layer = c("friend", "friend", "meet", "meet"),
                           label = c(NA, NA, "m1", "m2")))
  )
}

# The parts of a network that a round trip is expected to return. Classes
# differ in the order they list the information about a network in, and in
# whether an empty component is listed at all.
parts <- function(x) {
  list(info = x$info[sort(names(x$info))], nodes = x$nodes, ties = x$ties)
}

test_that("a stocnet holds hyperedges as ties among any number of nodes", {
  h <- hyper_fixtures()$undirected
  expect_true(is_hypergraph(h))
  expect_false(is_twomode(h))
  expect_false(is_directed(h))
  expect_equal(as.numeric(net_nodes(h)), 5)
  expect_equal(as.numeric(net_ties(h)), 2)
  expect_equal(h$ties$with, list(1:3, 3:4))
  expect_true(all(is.na(h$ties$from)) && all(is.na(h$ties$to)))
  expect_false(is_hypergraph(create_ring(4)))
  expect_false(is_hypergraph(as_stocnet(create_ring(4))))
  expect_true(is_directed(hyper_fixtures()$directed))
})

test_that("hyperedges are validated", {
  nodes <- data.frame(label = LETTERS[1:3])
  hyperedges <- function(...) make_stocnet(nodes = nodes,
                                           ties = dplyr::tibble(...))
  expect_error(hyperedges(with = list(c("A", "Z"))), "do not match")
  expect_error(hyperedges(with = list(c(1L, 9L))), "indices of the nodes")
  expect_error(hyperedges(with = list(c(1L, 1L))), "more than once")
  expect_error(hyperedges(with = list(1:2), with_weight = list(1)),
               "one value for each")
  expect_error(hyperedges(with = list(1:2),
                          with_direction = list(c("up", "down"))),
               "'from' or its 'to' side")
  expect_error(hyperedges(from = 1L, to = 2L, with = list(1:2)),
               "holds both|hold both")
  expect_error(hyperedges(from = c(1L, NA), to = c(2L, NA),
                          with = list(NULL, 1:3)), "layer")
  expect_error(hyperedges(from = c(1L, NA), to = c(2L, NA),
                          with = list(NULL, 1:3), layer = c("a", "a")),
               "one or the other")
  expect_error(make_stocnet(nodes = nodes,
                            ties = dplyr::tibble(from = 1L, to = 2L,
                                                 with_weight = list(1))),
               "'with' column")
  # an empty hyperedge and a node on each side of a directed one are valid
  expect_s3_class(hyperedges(with = list(integer(0))), "stocnet")
  expect_s3_class(hyperedges(with = list(c(1L, 1L)),
                             with_direction = list(c("from", "to"))),
                  "stocnet")
})

test_that("deleting nodes keeps the hyperedges of those that remain", {
  h <- hyper_fixtures()$weighted
  out <- delete_nodes(h, 3)
  expect_equal(out$ties$with, list(1:2, 3L))
  expect_equal(out$ties$with_weight, list(c(0.1, 0.2), 0.6))
  # a hyperedge is dropped only where none of its members remain
  expect_equal(as.numeric(net_ties(delete_nodes(h, 3:4))), 1)
  expect_equal(as.numeric(net_ties(delete_ties(h, 1))), 1)
  expect_equal(as.numeric(net_nodes(delete_isolates(h))), 4)
  expect_equal(names(add_tie_attribute(h, "size", 3:2)$ties)[7], "size")
})

test_that("hypergraphs round trip through igraph, tidygraph, and network", {
  for(fx in names(hyper_fixtures())){
    h <- hyper_fixtures()[[fx]]
    expect_equal(parts(as_stocnet(as_igraph(h))), parts(h), label = fx)
    expect_equal(parts(as_stocnet(as_tidygraph(h))), parts(h), label = fx)
    expect_equal(parts(as_stocnet(as_network(h))), parts(h), label = fx)
  }
})

test_that("other classes hold a hypergraph meaningfully", {
  h <- hyper_fixtures()$undirected
  g <- as_igraph(h)
  expect_true(is_hypergraph(g))
  expect_true(is_twomode(g))
  expect_true(igraph::is_bipartite(g))
  expect_equal(igraph::vcount(g), 5 + 2)
  # the weight of a hyperedge is held by its node, and its members are tied to it
  expect_equal(igraph::V(g)$weight, c(rep(NA, 5), 2, 3))
  expect_equal(igraph::V(g)$age[1:5], h$nodes$age)
  n <- as_network(h)
  expect_true(network::is.hyper(n))
  expect_true(is_hypergraph(n))
  expect_equal(network::network.size(n), 5)
  expect_equal(network::network.edgecount(n), 2)
  expect_equal(as_matrix(n), as_matrix(h))
  # a directed hyperedge runs to its node from its 'from' side and on from it
  d <- as_igraph(hyper_fixtures()$directed)
  expect_true(igraph::is_directed(d))
  expect_equal(as.numeric(igraph::distances(d, "A", "D", mode = "out")), 4)
  expect_equal(as.numeric(igraph::distances(d, "D", "A", mode = "out")), Inf)
  # a hypergraph that is marked no longer is the two-mode network it is held as
  expect_false(is_hypergraph(to_twomode(g)))
})

test_that("a hyperedge that a network object cannot hold is carried", {
  h <- make_stocnet(info = list(directed = FALSE),
                    nodes = data.frame(label = LETTERS[1:3]),
                    ties = dplyr::tibble(with = list(c("A", "B"), character(0)),
                                         label = c("e1", "e2")))
  expect_warning(n <- as_network(h), "cannot hold as an edge")
  expect_equal(network::network.edgecount(n), 1)
  expect_equal(parts(as_stocnet(n)), parts(h))
  expect_equal(parts(as_stocnet(as_igraph(h))), parts(h))
})

test_that("the matrix of a hypergraph is its incidence matrix", {
  fx <- hyper_fixtures()
  B <- as_matrix(fx$undirected)
  expect_equal(dim(B), c(5, 2))
  expect_equal(dimnames(B), list(LETTERS[1:5], c("e1", "e2")))
  expect_equal(unname(colSums(B)), c(3, 2))
  expect_equal(unname(as_matrix(fx$weighted)[, 1]), c(0.1, 0.2, 0.3, 0, 0))
  expect_equal(unname(as_matrix(fx$directed)[, 1]), c(-1, -1, 1, 0, 0))
  expect_equal(dim(as_matrix(fx$undirected, twomode = FALSE)), c(7, 7))
  el <- as_edgelist(fx$weighted)
  expect_equal(el$from, c("A", "B", "C", "C", "D"))
  expect_equal(el$to, c("e1", "e1", "e1", "e2", "e2"))
  expect_equal(el$role, c("x", "y", "z", "x", "x"))
})

test_that("to_hypergraph() takes the second mode as hyperedges and back", {
  sw <- as_stocnet(ison_southern_women)
  h <- to_hypergraph(sw)
  expect_true(is_hypergraph(h))
  expect_equal(as.numeric(net_nodes(h)), 18)
  expect_equal(as.numeric(net_ties(h)), 14)
  expect_equal(h$info$modes, "women")
  expect_equal(h$info$hyperedges, "social events")
  expect_equal(as_matrix(h), as_matrix(sw))
  # the attributes of the events are those of the hyperedges
  expect_true(all(c("label", "time") %in% names(h$ties)))
  expect_s3_class(h$ties$time, "Date")
  expect_identical(to_hypergraph(h), h)
  back <- to_twomode(h)
  expect_true(is_twomode(back))
  expect_false(is_hypergraph(back))
  expect_equal(back$nodes[c("label", "mode")], sw$nodes[c("label", "mode")])
  expect_equal(dplyr::arrange(back$ties, from, to),
               dplyr::arrange(sw$ties, from, to))
  expect_equal(back$info[c("modes", "sender", "receiver")],
               sw$info[c("modes", "sender", "receiver")])
  # every class is projected, and says that it holds a hypergraph
  for(net in list(as_igraph(sw), as_tidygraph(sw), as_network(sw)))
    expect_true(is_hypergraph(to_hypergraph(net)))
  # the weight of a tie is the weight of the membership it becomes
  w <- to_hypergraph(add_tie_attribute(sw, "weight", rep(1:2, length.out = 89)))
  expect_true("with_weight" %in% names(w$ties))
  expect_true(is_weighted(w))
})

test_that("to_hypergraph() takes each tie of a one-mode network as a hyperedge", {
  h <- to_hypergraph(as_stocnet(create_ring(4)))
  expect_s3_class(h, "stocnet")
  expect_true(is_hypergraph(h))
  expect_equal(lengths(h$ties$with), rep(2L, 4))
  expect_false("with_direction" %in% names(h$ties))
  d <- to_hypergraph(as_stocnet(create_ring(4, directed = TRUE)))
  expect_equal(d$ties$with_direction[[1]], c("from", "to"))
  expect_equal(unname(as_matrix(d)[, 1]), c(-1, 1, 0, 0))
  # nothing is lost: the ties keep their attributes, and are returned
  net <- as_stocnet(ison_adolescents)
  w <- to_hypergraph(add_tie_attribute(net, "weight", 1:10))
  expect_equal(w$ties$weight, 1:10)
  expect_equal(as_matrix(to_unweighted(to_mode(to_hypergraph(net), 1))),
               as_matrix(net))
  # every class is reformatted, and says that it holds a hypergraph
  for(ring in list(create_ring(4), as_igraph(create_ring(4)),
                   as_network(create_ring(4))))
    expect_true(is_hypergraph(to_hypergraph(ring)))
  expect_identical(to_hypergraph(h), h)
})

test_that("to_simplicial() takes the maximal cliques of a one-mode network", {
  net <- as_stocnet(ison_adolescents)
  h <- to_simplicial(net)
  expect_true(is_hypergraph(h))
  expect_true(h$info$simplicial)
  expect_match(describe_network(h), "simplicial complex")
  expect_true(all(lengths(h$ties$with) >= 2))
  # the nodes a clique ties are tied in the network the cliques came from
  expect_equal(as_matrix(to_unweighted(to_mode(h, 1))), as_matrix(net))
  expect_true(is_hypergraph(to_simplicial(as_igraph(ison_adolescents))))
})

test_that("to_simplicial() keeps the largest hyperedges of a hypergraph", {
  h <- make_stocnet(info = list(directed = FALSE),
                    nodes = data.frame(label = LETTERS[1:5]),
                    ties = dplyr::tibble(with = list(c("A", "B", "C"),
                                                     c("A", "B"), c("C", "D"),
                                                     c("D", "C"), "E"),
                                         label = paste0("e", 1:5)))
  out <- to_simplicial(h)
  expect_true(out$info$simplicial)
  expect_equal(out$ties$label, c("e1", "e3", "e5"))
  # the events of a two-mode network that are not within another event
  sw <- to_simplicial(as_stocnet(ison_southern_women))
  expect_true(is_hypergraph(sw))
  expect_lt(as.numeric(net_ties(sw)), 14)
})

test_that("a hypergraph is projected through its hyperedges", {
  sw <- as_stocnet(ison_southern_women)
  h <- to_hypergraph(sw)
  expect_equal(as_matrix(to_mode1(h)), as_matrix(to_mode1(sw)))
  expect_equal(as_matrix(to_mode2(h)), as_matrix(to_mode2(sw)))
  expect_equal(as_matrix(to_mode(h, "events")), as_matrix(to_mode2(sw)))
  expect_false(is_hypergraph(to_mode1(h)))
})

test_that("the counts of a hypergraph agree however they are taken", {
  h <- to_hypergraph(as_stocnet(ison_southern_women))
  B <- as_matrix(h)
  degree <- rowSums(B)
  sizes <- colSums(B)
  expect_equal(sum(degree), sum(sizes))
  expect_equal(sum(degree), sum(lengths(h$ties$with)))
  expect_equal(unname(degree), tabulate(unlist(h$ties$with), 18))
  expect_equal(unname(sizes), lengths(h$ties$with))
  # a path through a hyperedge is two steps where the hyperedge is a node,
  # and one step between the nodes that share it
  g <- as_igraph(h)
  women <- which(!igraph::V(g)$type)
  berge <- igraph::distances(g, women, women) / 2
  shared <- igraph::distances(as_igraph(to_mode1(h)), weights = NA)
  expect_equal(unname(berge), unname(shared))
})

test_that("to_path() returns the ties on the shortest paths", {
  path <- to_path(ison_adolescents, "Betty", "Alice")
  expect_equal(as.numeric(net_ties(path)), 2)
  expect_equal(as.numeric(net_nodes(path)),
               as.numeric(net_nodes(ison_adolescents)))
  ring <- to_path(create_ring(6), 1, 4)
  expect_equal(as.numeric(net_ties(ring)), 6)
  expect_equal(as.numeric(net_ties(to_path(create_ring(6, directed = TRUE),
                                            1, 4))), 3)
  expect_equal(as.numeric(net_ties(to_path(create_empty(3), 1, 2))), 0)
  expect_error(to_path(create_ring(6), 1, 9), "not a node")
  expect_error(to_path(create_ring(6), 1), "from.*to")
  # in a hypergraph, the hyperedges on the shortest paths
  h <- hyper_fixtures()$undirected
  expect_equal(to_path(h, "A", "D")$ties$label, c("e1", "e2"))
  expect_equal(to_path(h, "A", "B")$ties$label, "e1")
  expect_equal(nrow(to_path(h, "A", "E")$ties), 0)
})

test_that("functions that need dyads say so of a hypergraph", {
  h <- hyper_fixtures()$weighted
  expect_error(to_backbone(h), "to_mode")
  expect_error(to_normalised(h), "to_mode")
})

test_that("a hypergraph is described as one", {
  fx <- hyper_fixtures()
  expect_match(describe_network(fx$undirected), "hypergraph")
  expect_match(describe_ties(fx$undirected), "2 hyperedges of 2 to 3 nodes")
  expect_match(describe_ties(fx$mixed),
               "2 meet hyperedges of 3 nodes and 2 friend arcs")
})

test_that("read_hif() reads nodes, hyperedges, and memberships", {
  skip_if_not_installed("jsonlite")
  h <- read_hif(testthat::test_path("sheets", "hif_publications.json"))
  expect_true(is_hypergraph(h))
  expect_equal(h$info$name, "HIF publications dataset")
  expect_equal(h$info$doi, "10.5281/zenodo.15802760")
  expect_equal(h$info$date, "2025-07-04")
  expect_false(h$info$directed)
  expect_equal(h$nodes$label, "20")
  expect_equal(h$ties$label, c("10", "30"))
  expect_equal(h$ties$source, c("Arxiv", "DBLP"))
  expect_equal(h$ties$with_weight, list(2, 3))
  # an attribute of more than one value is kept as the JSON that records it
  expect_equal(h$nodes$institutions, "[\"PNNL\",\"UVA\"]")
})

test_that("read_hif() reads directed hyperedges and what each member holds", {
  skip_if_not_installed("jsonlite")
  h <- read_hif(testthat::test_path("sheets", "hif_directed.json"))
  expect_true(is_directed(h))
  expect_equal(h$nodes$label, LETTERS[1:4])
  expect_equal(h$nodes$weight, c(1.5, NA, NA, NA))
  expect_equal(h$nodes$kind, c("acid", NA, NA, "salt"))
  expect_equal(h$ties$weight, c(0.5, NA))
  expect_equal(h$ties$with, list(1:3, 3:4))
  expect_equal(h$ties$with_direction,
               list(c("from", "from", "to"), c("from", "to")))
  expect_equal(h$ties$with_weight, list(c(2, 1, 1), c(1, 3)))
  expect_equal(h$ties$with_role, list(c(NA, NA, "product"), c(NA_character_, NA)))
  expect_equal(unname(as_matrix(h)[, "r1"]), c(-2, -1, 1, 0))
})

test_that("read_hif() reads repeats once and what no incidence names", {
  skip_if_not_installed("jsonlite")
  h <- read_hif(testthat::test_path("sheets", "hif_empty.json"))
  expect_true(h$info$simplicial)
  expect_equal(h$nodes$label, c("n1", "n3", "n2"))
  expect_equal(h$ties$label, c("e1", "e2"))
  expect_equal(h$ties$with, list(c(1L, 3L), integer(0)))
  expect_error(read_hif(testthat::test_path("sheets", "test_matrix.csv")))
})

test_that("write_hif() writes what read_hif() reads", {
  skip_if_not_installed("jsonlite")
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  for(fx in c("undirected", "directed", "weighted")){
    h <- hyper_fixtures()[[fx]]
    write_hif(h, file)
    expect_equal(parts(read_hif(file)), parts(h), label = fx)
  }
  for(sheet in c("hif_publications", "hif_directed", "hif_empty")){
    h <- read_hif(testthat::test_path("sheets", paste0(sheet, ".json")))
    write_hif(h, file)
    expect_equal(read_hif(file), h, label = sheet)
  }
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_equal(written[["network-type"]], "asc")
  expect_setequal(names(written),
                  c("network-type", "incidences", "nodes", "edges"))
  # a network that is not a hypergraph is written as one
  write_hif(create_ring(5), file)
  expect_equal(parts(read_hif(file)),
               parts(to_hypergraph(as_stocnet(create_ring(5)))))
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_equal(written$incidences[[1]], list(edge = 1L, node = 1L))
  write_hif(create_ring(3, directed = TRUE), file)
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_equal(written[["network-type"]], "directed")
  expect_equal(written$incidences[[1]]$direction, "tail")
})

test_that("a hypergraph is coerced to and from a HyperG object", {
  h <- hyper_fixtures()$undirected
  expect_warning(x <- as_hypergraph(h), "weights and attributes")
  # a 'hypergraph' object is a list of one sparse matrix of hyperedges by nodes
  expect_s3_class(x, "hypergraph")
  expect_named(x, "M")
  expect_s4_class(x$M, "sparseMatrix")
  expect_equal(dim(x$M), c(2, 5))
  expect_equal(dimnames(x$M), list(c("e1", "e2"), LETTERS[1:5]))
  expect_equal(unname(as.matrix(x$M)), unname(t(as_matrix(h))))
  expect_identical(as_hypergraph(x), x)
  back <- as_stocnet(x)
  expect_true(is_hypergraph(back))
  expect_equal(back$ties$with, h$ties$with)
  expect_equal(back$ties$label, h$ties$label)
  expect_equal(back$nodes$label, h$nodes$label)
  # nodes without names are named by their positions, and read back unnamed
  ring <- as_hypergraph(create_ring(4))
  expect_equal(colnames(ring$M), as.character(1:4))
  expect_null(as_stocnet(ring)$nodes[["label"]])
  expect_equal(lengths(as_stocnet(ring)$ties$with), rep(2L, 4))
  # a node in no hyperedge and a hyperedge of no nodes keep their places
  sparse <- make_stocnet(nodes = data.frame(label = LETTERS[1:3]),
                         ties = dplyr::tibble(with = list(c("A", "B"),
                                                          character(0))))
  expect_equal(dim(as_hypergraph(sparse)$M), c(2, 3))
  expect_equal(as_stocnet(as_hypergraph(sparse))$ties$with,
               list(1:2, integer(0)))
})
