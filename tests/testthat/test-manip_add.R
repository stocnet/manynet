southern_women <- readRDS(testthat::test_path("sheets", "southern_women.Rds"))

# object without nodal attributes
net_node1 <- as_tidygraph(data.frame(
  from = c("A", "B", "C", "D","E"),
  to = c("B", "C", "D", "E", "A")))

# object with nodal attributes
net_node2 <- net_node1 %>%
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
  south1 <- add_node_attribute(southern_women, "Age", rep(25, 18))
  expect_equal(igraph::vertex_attr(south1, "Age"),
               c(rep(25, 18), rep(NA, 14)))
  # Second nodeset
  south2 <- add_node_attribute(southern_women, "Budget", rep(100, 14))
  expect_equal(igraph::get.vertex.attribute(as_igraph(south2),  "Budget"),
               c(rep(NA, 18), rep(100, 14)))
  # Test error when wrong number of attributes
  expect_error(add_node_attribute(southern_women, "Budget", rep(100, 15)))
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
  expect_equal(igraph::edge_attr(add_tie_attribute(net_edge1, "weight", 
                                                  c(1,2,1,2,1)), 
                              "weight"), 
               c(1,2,1,2,1))
  expect_equal(class(add_tie_attribute(net_edge1, "weight", c(1,2,1,2,1))), 
               "igraph")
})

test_that("join_ties works", {
  testmutateedges <- join_ties(southern_women, create_filled(c(3,4)))
  expect_equal(class(testmutateedges), c("tbl_graph", "igraph"))
})

test_that("mutate_ties and filter_ties works", {
  orig <- southern_women %>% mutate_ties(year = 1:93)
  filt <- orig %>% activate(edges) %>% dplyr::filter(year > 5) %>% activate(nodes)
  filt1 <- filter_ties(orig, year > 5)
  expect_equal(1:93, igraph::edge_attr(as_igraph(orig), "year"))
  expect_equal(igraph::edge_attr(as_igraph(filt), "year"),
               igraph::edge_attr(as_igraph(filt1), "year"))
})

test_that("summarise_ties works", {
  set.seed(1234)
  orig <- as_tidygraph(southern_women) %>%
    tidygraph::graph_join(as_tidygraph(southern_women)) %>%
    mutate_ties(year = sample(1:3, 186, replace = TRUE))
  sum <- summarise_ties(orig, mean = mean(year))
  expect_length(igraph::edge_attr(sum, "weight"), 93)
})
