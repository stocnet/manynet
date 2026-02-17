# Tests for the as_ conversion methods
mat1 <- matrix(c(0,1,0,0,1,0,1,0,0,1,0,1,0,0,0,0),4,4, byrow = TRUE)
rownames(mat1) <- LETTERS[1:4]
colnames(mat1) <- LETTERS[1:4]
mat2 <- matrix(c(0,1,0,0,2,0,3,0,0,4,0,5,0,0,0,0),4,4, byrow = TRUE)
rownames(mat2) <- LETTERS[1:4]
colnames(mat2) <- LETTERS[1:4]
# Unweighted test
data1 <- dplyr::arrange(data.frame(from = c("A","B","B","C","C"),
                    to = c("B","C","A","D","B")),
                    from, to)
# Weighted test
data2 <- data1
data2$weight <- 1:5

# Data 3: misnamed weight col
data3 <- data1
data3$hello <- 1:5

test_that("as_edgelist converts correctly", {
  expect_s3_class(as_edgelist(as_igraph(data2)), "tbl_df")
  expect_equal(as_edgelist(as_igraph(data2)), dplyr::as_tibble(data2))
  expect_equal(as_edgelist(as_igraph(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_tidygraph(data2)), dplyr::as_tibble(data2))
  expect_equal(as_edgelist(as_tidygraph(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_network(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_network(data2)), dplyr::as_tibble(data2))
})

test_that("data frame converted to matrix correctly",{
  expect_equal(as_matrix(data1), mat1)
  expect_equal(as_matrix(data2), mat2)
})

test_that("as_matrix converts correctly",{
  expect_vector(as_matrix(mat1))
  expect_vector(as_matrix(ison_southern_women))
  expect_vector(ison_southern_women %>% as_matrix())
  expect_vector(ison_southern_women %>% as_matrix())
  expect_equal(as_matrix(as_network(ison_southern_women)),
               as_matrix(ison_southern_women))
})

test_that("as_igraph converts correctly",{
  expect_s3_class(as_igraph(mat1), "igraph")
  expect_s3_class(as_igraph(ison_southern_women), "igraph")
  expect_s3_class(as_igraph(as_network(ison_southern_women)), "igraph")
  expect_error(as_igraph(data3, weight = T))
  expect_equal(igraph::vcount(as_igraph(as_network(data2))),
               igraph::vcount(as_igraph(data2)))
  # NB: ordering of edges is a little different when converting from network
  # to igraph. Should not matter though.
})

test_that("as_graphAM converts correctly",{
  expect_s4_class(as_graphAM(mat1), "graphAM")
  expect_s4_class(as_graphAM(ison_southern_women), "graphAM")
  expect_s4_class(as_graphAM(as_network(ison_southern_women)), "graphAM")
  expect_equal(as_graphAM(as_network(data2))@edgemode, "directed")
})

test_that("as_tidygraph converts correctly",{
  expect_s3_class(as_tidygraph(mat1), "tbl_graph")
  expect_s3_class(as_tidygraph(ison_southern_women), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(mat1)), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(ison_southern_women)),
                  "tbl_graph")
})

test_that("as_network converts correctly",{
  expect_s3_class(as_network(mat1), "network")
  expect_s3_class(as_network(ison_southern_women), "network")
  expect_equal(as_network(as_network(data2)), as_network(data2))
  expect_equal(as_network(as_igraph(ison_southern_women)),
               as_network(ison_southern_women))
  expect_equal(igraph::vcount(as_igraph(as_network(dplyr::as_tibble(data2)))),
               igraph::vcount(as_igraph(as_network(data2))))
  expect_equal(is_directed(ison_southern_women),
               is_directed(as_network(ison_southern_women)))
  expect_equal(is_directed(ison_southern_women),
               is_directed(as_network(ison_southern_women)))
  # NB: ordering of edges is a little different when converting from network
  # to igraph. Should not matter though.
})

# test conversion of siena objects
# test_that("as_tidygraph.siena converts correctly", {
#   expect_equal(net_nodes(as_igraph(sienadata)), net_nodes(as_matrix(sienadata)))
#   expect_equal(net_nodes(as_igraph(sienadata)), length(sienadata[["nodeSets"]][["Actors"]]))
# })

test_that("conversion of diff_model object works correctly", {
  skip_on_cran()
  skip_on_ci()
  expect_warning(diff <- play_diffusion(ison_brandes, old_version = TRUE))
  tidy_diff <- as_tidygraph(diff)
  expect_values(net_nodes(tidy_diff), net_nodes(ison_brandes))
  expect_values(net_ties(tidy_diff), net_ties(ison_brandes))
  expect_values(net_nodes(tidy_diff), max(diff$I))
})

test_that("mnet objects printed correctly", {
  expect_output(print(ison_adolescents), "A tibble")
  expect_match(describe_network(ison_adolescents), "labelled, undirected")
  expect_match(describe_nodes(ison_adolescents), "8 adolescents")
  expect_match(describe_ties(ison_adolescents), "10 friendship")
  expect_null(describe_changes(ison_adolescents))
})

test_that("network dynamic converts correctly",{
  skip_if_not(requireNamespace("networkDynamic", quietly = TRUE)) 
  networkDynamic <- get("networkDynamic", asNamespace("networkDynamic"))
  onset <- 7168
  terminus <- 19843
  nodeID <- c( 1,2,13,29,31,34,44,59,67,82,89,115,121,122,128,146,156,181,190,191,
               197,211,223,274,288,289,301,302,334,351,393,394,396,418)
  ndf <- data.frame(onset, terminus, nodeID)
  start_time <- c(19517, 19794, 19138, 19425, 19837, 19805, 19438, 19462, 19507,
                  19796, 19832, 19514, 19808, 19252, 19266, 19711, 19783, 19178,
                  19348, 19508)
  end_time <- c(19517, 19795, 19139, 19426, 19838, 19805, 19439, 19462, 19508,
                19796, 19836, 19515, 19809, 19254, 19266, 19712, 19783, 19178,
                19348, 19509)
  from <- c(1, 418, 34, 396, 34, 223, 13, 334, 34, 191, 181, 2, 2, 211, 31, 156, 288, 289, 122, 156)
  to <- c(156, 393, 351, 394, 146, 115, 274, 121, 29, 190, 89, 128, 44, 67, 302, 59, 34,301, 82, 197)
  edf <- data.frame(start_time, end_time, from, to)
  sample_net <- networkDynamic(vertex.spells = ndf[ ,c(1,2,3)],
                               edge.spells = edf[ ,c(1,2,3,4)])
  network::set.network.attribute(sample_net, 'net.obs.period', list(observations = list(c(7168,19843)), mode = "discrete", time.increment = 1, time.unit = 'day'))
  class(sample_net)
  expect_no_failure(as_igraph(sample_net))
})
