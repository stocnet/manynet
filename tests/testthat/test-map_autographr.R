test_that("unweighted, unsigned, undirected networks graph correctly", {
  skip_on_cran()
  # Unweighted, unsigned, undirected network
  test_brandes <- autographr(ison_brandes)
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  # Node parameters
  expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  expect_equal(as.character(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

test_that("unweighted, signed, undirected networks graph correctly", {
  skip_on_cran()
  # Unweighted, signed, undirected network
  test_marvel <- autographr(to_giant(ison_marvel_relationships))
  # Node position
  expect_equal(round(test_marvel[["data"]][["x"]][[1]]), -1)
  expect_equal(round(test_marvel[["data"]][["y"]][[1]]), 1)
  # Edge parameters
  expect_equal(test_marvel[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  # Node parameters
  expect_equal(test_marvel[["layers"]][[3]][["aes_params"]][["size"]], 1)
  expect_equal(test_marvel[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

test_that("unweighted, unsigned, directed networks graph correctly", {
  skip_on_cran()
  # Unweighted, unsigned, directed network
  test_algebra <- autographr(ison_algebra)
  # Node position
  expect_equal(round(test_algebra[["data"]][["x"]][[1]]), 0)
  expect_equal(round(test_algebra[["data"]][["y"]][[1]]), 0)
  # Edge parameters
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_algebra[["layers"]][[1]][["aes_params"]][["end_cap"]]), "circle")
  #expect_s3_class(test_algebra[["layers"]][[2]][["aes_params"]][["end_cap"]], "ggraph_geometry")
  # Node parameters
  expect_equal(round(test_algebra[["layers"]][[2]][["aes_params"]][["size"]]), 3)
  expect_equal(test_algebra[["layers"]][[2]][["aes_params"]][["shape"]], "circle")
})

test_that("weighted, unsigned, directed networks graph correctly", {
  skip_on_cran()
  # Weighted, unsigned, directed network
  test_networkers <- autographr(ison_networkers)
  # Node position
  expect_equal(round(test_networkers[["data"]][["x"]][[1]]), 0)
  expect_equal(round(test_networkers[["data"]][["y"]][[1]]), 0)
  # Edge parameters
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]]), "circle")
  #expect_s3_class(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]], "ggraph_geometry")
  # Node parameters
  expect_equal(round(test_networkers[["layers"]][[3]][["aes_params"]][["size"]]), 2)
  expect_equal(test_networkers[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

# Testing the node_color, node_size, and node_shape args by specifying a node attribute
test_that("fancy node mods graph correctly", {
  skip_on_cran()
  # one-mode network
  ison_marvel_relationships <- dplyr::mutate(ison_marvel_relationships,
                                             nodesize = Appearances/1000)
  testcolnodes <- autographr(ison_marvel_relationships,
                             node_color = "Gender",
                             node_size = "nodesize",
                             node_shape = "Attractive")
  expect_s3_class(testcolnodes, c("ggraph","gg","ggplot"))
  expect_equal(round(testcolnodes$data$x[1]), 4)
  expect_equal(round(testcolnodes$data$y[1]), 3)
  expect_equal(nrow(testcolnodes[["plot_env"]][["lo"]]),
               network_nodes(ison_marvel_relationships))
  # two-mode network
  ison_southern_women <- add_node_attribute(ison_southern_women, "group",
                                            c(sample(c("a", "b"),
                                                     length(ison_southern_women),
                                                     replace = TRUE)))
  test2 <- autographr(ison_southern_women,
                      node_color = "type")
  expect_s3_class(test2, c("ggraph","gg","ggplot"))
  expect_equal(round(test2$data$x[1]), 0)
  expect_equal(round(test2$data$y[1]), 0)
  expect_equal(nrow(test2[["plot_env"]][["lo"]]),
               network_nodes(ison_southern_women))
})

test_that("edge colours graph correctly", {
  skip_on_cran()
  ison_brandes2 <- ison_brandes %>%
    add_tie_attribute("tiecolour",
                      c("A", "B", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))
  test_brandes2 <- autographr(ison_brandes2, edge_color = "tiecolour")
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_colour))
})

# Named networks
test_that("named networks plot correctly", {
  skip_on_cran()
  onemode <- autographr(ison_adolescents)
  twomode <- autographr(ison_southern_women)
  expect_equal(onemode[["data"]][["name"]], node_names(ison_adolescents))
  expect_equal(twomode[["data"]][["name"]], node_names(ison_southern_women))
})

# Test that autographr() works with arguments without quotes
test_that("node_group works correctly", {
  skip_on_cran()
  expect_equal(autographr(ison_lawfirm, node_group = Gender),
               autographr(ison_lawfirm, node_group = "Gender"))
})

test_that("unquoted arguments plot correctly", {
  skip_on_cran()
  expect_equal(autographr(ison_lawfirm, node_color = "Gender"),
               autographr(ison_lawfirm, node_color = Gender))
})
