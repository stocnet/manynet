# object without nodal attributes
net_node1 <- as_tidygraph(data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("B", "C", "D", "E", "A")))

# object with nodal attributes
net_node2 <- net_node1 |> 
  dplyr::mutate(attribute = c("friend", "family", "friend", "friend", "family"))

# object without edge attributes
net_edge1 <- data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("C", "D", "A", "A", "B"))

test_that("add_node_attribute works", {
  # Test on one mode network
  expect_equal(as_tidygraph(add_node_attribute(net_node1, "attribute", 
                                                c("friend", "family", "friend", "friend", "family"))), 
               net_node2)
  # On two mode network
  # First nodeset
  south1 <- add_node_attribute(ison_southern_women, "Age", rep(25, 18))
  expect_equal(igraph::vertex_attr(south1, "Age"),
               c(rep(25, 18), rep(NA, 14)))
  # Second nodeset
  south2 <- add_node_attribute(ison_southern_women, "Budget", rep(100, 14))
  expect_equal(igraph::vertex_attr(as_igraph(south2),  "Budget"),
               c(rep(NA, 18), rep(100, 14)))
  # Test error when wrong number of attributes
  expect_error(add_node_attribute(ison_southern_women, "Budget", rep(100, 15)))
})

test_that("bind_node_attributes works", {
  expect_equal(as_tidygraph(bind_node_attributes(net_node1, net_node2)), 
               net_node2)
  # Test error when different number of dimensions
  net_node3 <- as_tidygraph(data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "D")))
  expect_error(bind_node_attributes(net_node1, net_node3))
})

test_that("add_tie_attribute works", {
  expect_equal(unname(unlist(add_tie_attribute(net_edge1, "weight", c(1,2,1,2,1))[,"weight"])),
               c(1,2,1,2,1))
  expect_s3_class(add_tie_attribute(net_edge1, "weight", c(1,2,1,2,1)), "data.frame")
})

test_that("delete_node_attribute works", {
  net <- ison_adolescents |> mutate_nodes(age = 11:18, sex = rep("F", 8))
  expect_true(all(c("age", "sex") %in% net_node_attributes(net)))
  # Removes a single attribute
  expect_false("age" %in% net_node_attributes(delete_node_attribute(net, "age")))
  expect_true("sex" %in% net_node_attributes(delete_node_attribute(net, "age")))
  # Removes several at once
  expect_equal(net_node_attributes(delete_node_attribute(net, c("age", "sex"))), "name")
  # Returns the same class it was given
  expect_s3_class(delete_node_attribute(net, "age"), "tbl_graph")
  expect_s3_class(delete_node_attribute(as_igraph(net), "age"), "igraph")
  # Same result as the tidyverse-style NULL assignment
  expect_equal(net_node_attributes(delete_node_attribute(net, "age")),
               net_node_attributes(mutate_nodes(net, age = NULL)))
})

test_that("delete_tie_attribute works", {
  net <- ison_adolescents |> mutate_ties(weight = 1:10, kind = rep("x", 10))
  expect_true(all(c("weight", "kind") %in% net_tie_attributes(net)))
  expect_false("weight" %in% net_tie_attributes(delete_tie_attribute(net, "weight")))
  expect_equal(net_tie_attributes(delete_tie_attribute(net, c("weight", "kind"))),
               character(0))
  expect_s3_class(delete_tie_attribute(net, "weight"), "tbl_graph")
})

test_that("join_ties works", {
  testmutateedges <- join_ties(ison_southern_women, create_filled(c(3,4)))
  expect_s3_class(testmutateedges, c("tbl_graph", "igraph"))
})

test_that("mutate_ties and filter_ties works", {
  orig <- ison_southern_women |> mutate_ties(year = 1:89)
  filt <- orig |> filter_ties(year > 5)
  filt1 <- filter_ties(orig, year > 5)
  expect_equal(1:89, igraph::edge_attr(as_igraph(orig), "year"))
  expect_equal(igraph::edge_attr(as_igraph(filt), "year"),
               igraph::edge_attr(as_igraph(filt1), "year"))
})

test_that("summarise_ties works", {
  set.seed(1234)
  orig <- from_ties(bloop = as_tidygraph(ison_southern_women),
                    bleep = as_tidygraph(ison_southern_women)) |>
    mutate_ties(year = sample(1:3, 178, replace = TRUE))
  sum <- summarise_ties(orig, mean = mean(year))
  expect_length(igraph::edge_attr(sum, "weight"), 89)
})

test_that("delete_nodes works", {
  expect_length(ison_adolescents, 8)
  expect_length(delete_nodes(ison_adolescents, "Betty"), 7)
})

test_that("add_ties accepts an even vector of nodes", {
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents, c("Betty", "Tina")))), 11)
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents, c(1, 5)))), 11)
  expect_equal(as_edgelist(add_ties(ison_adolescents, c("Betty", "Tina")))[11,],
               dplyr::tibble(from = "Betty", to = "Tina"))
  expect_error(add_ties(ison_adolescents, c(1, 2, 3)), "even vector")
  expect_error(add_ties(ison_adolescents, c("Betty", "Nobody")), "not found")
})

test_that("add_ties accepts explicit tie syntax", {
  expect_equal(as_edgelist(add_ties(ison_adolescents, Betty -+ Tina))[11,],
               dplyr::tibble(from = "Betty", to = "Tina"))
  # numbers are interpreted as node indices
  expect_equal(as_edgelist(add_ties(ison_adolescents, 1 -+ 3))[11,],
               dplyr::tibble(from = "Betty", to = "Alice"))
  # several ties can be added at once, and node sets linked
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents,
                                 c(Betty -+ Tina, Sue -+ Pam)))), 12)
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents, Betty:Sue -+ Tina))), 12)
  # one-sided formulae are equivalent
  expect_equal(as_edgelist(add_ties(ison_adolescents, ~ Betty -- Tina))[11,],
               dplyr::tibble(from = "Betty", to = "Tina"))
  # in a directed network, mutual syntax adds both arcs
  dir <- to_directed(create_ring(4))
  expect_equal(as.numeric(net_ties(add_ties(dir, 1 -+ 3))), as.numeric(net_ties(dir)) + 1)
  expect_equal(as.numeric(net_ties(add_ties(dir, 1 +-+ 3))), as.numeric(net_ties(dir)) + 2)
  # subtraction is still arithmetic, not tie syntax
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents, c(4 - 3, 5)))), 11)
})

test_that("add_ties adds a number of ties at random", {
  set.seed(123)
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents, 3))), 13)
  expect_equal(as.numeric(net_ties(add_ties(ison_adolescents, 0))), 10)
  expect_true(is_twomode(add_ties(ison_southern_women, 5)))
  expect_equal(as.numeric(net_ties(add_ties(ison_southern_women, 5))), 94)
  # no more ties can be added than there are dyads left untied
  expect_error(add_ties(create_filled(4), 1), "not already tied")
  expect_error(add_ties(ison_adolescents, 2.5), "non-negative integer")
})

test_that("add_ties accepts a two-column matrix or edgelist", {
  expect_equal(as_edgelist(add_ties(ison_adolescents,
                                    matrix(c(1, 3, 2, 4), 2)))[11:12,],
               dplyr::tibble(from = c("Betty", "Alice"),
                             to = c("Sue", "Jane")))
  expect_error(add_ties(ison_adolescents, matrix(1:9, 3)), "two columns")
})

test_that("add_ties works across classes and keeps new ties weighted", {
  wtd <- add_tie_attribute(ison_adolescents, "weight", 1:10)
  for (cl in c("tidygraph", "igraph", "matrix", "network", "edgelist")) {
    x <- get(paste0("as_", cl))(wtd)
    expect_equal(as.numeric(net_ties(add_ties(x, c(1, 5)))), 11,
                 label = paste0("add_ties() on ", cl))
  }
  # new ties are given a weight of 1 rather than a missing weight
  expect_false(anyNA(as_matrix(add_ties(as_matrix(wtd), c(1, 5)))))
  expect_equal(igraph::edge_attr(add_ties(wtd, c(1, 5)), "weight")[11], 1)
  expect_equal(igraph::edge_attr(add_ties(wtd, c(1, 5),
                                          list(weight = 7)), "weight")[11], 7)
})

test_that("as_network() keeps a single node attribute per node", {
  out <- as_network(add_node_attribute(ison_adolescents, "group",
                                       rep(c("A", "B"), 4)))
  expect_equal(network::get.vertex.attribute(out, "group"),
               rep(c("A", "B"), 4))
})
