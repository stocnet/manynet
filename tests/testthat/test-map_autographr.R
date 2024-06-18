test_that("unweighted, unsigned, undirected networks graph correctly", {
  skip_on_cran()
  # Unweighted, unsigned, undirected network
  test_brandes <- graphr(ison_brandes)
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  # Node parameters
  #expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 5)
  #expect_equal(as.character(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]]), "circle")
})

test_that("unweighted, signed, undirected networks graph correctly", {
  skip_on_cran()
  # Unweighted, signed, undirected network
  test_marvel <- graphr(to_giant(ison_marvel_relationships))
  # Node position
  expect_equal(round(test_marvel[["data"]][["x"]][[1]]), -1)
  expect_equal(round(test_marvel[["data"]][["y"]][[1]]), 1)
  # Edge parameters
  expect_equal(test_marvel[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  # Node parameters
  #expect_equal(test_marvel[["layers"]][[4]][["aes_params"]][["size"]], 1)
  #expect_equal(test_marvel[["layers"]][[4]][["aes_params"]][["shape"]], "circle")
})

test_that("unweighted, unsigned, directed networks graph correctly", {
  skip_on_cran()
  # Unweighted, unsigned, directed network
  test_algebra <- graphr(ison_algebra)
  # Node position
  expect_equal(round(test_algebra[["data"]][["x"]][[1]]), 0)
  expect_equal(round(test_algebra[["data"]][["y"]][[1]]), 0)
  # Edge parameters
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_algebra[["layers"]][[1]][["aes_params"]][["end_cap"]]), "circle")
  expect_s3_class(test_algebra[["layers"]][[1]][["aes_params"]][["end_cap"]], "ggraph_geometry")
  # Node parameters
  #expect_equal(round(test_algebra[["layers"]][[2]][["aes_params"]][["size"]]), 3)
  #expect_equal(test_algebra[["layers"]][[2]][["aes_params"]][["shape"]], "circle")
})

test_that("weighted, unsigned, directed networks graph correctly", {
  skip_on_cran()
  skip_on_ci()
  # Weighted, unsigned, directed network
  test_networkers <- graphr(ison_networkers)
  # Node position
  expect_equal(round(test_networkers[["data"]][["x"]][[1]]), 9)
  expect_equal(round(test_networkers[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_linetype"]], "solid")
  expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_colour"]], "black")
  expect_equal(as.character(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]]), "circle")
  # Node parameters
  #expect_equal(round(test_networkers[["layers"]][[3]][["aes_params"]][["size"]]), 2)
  #expect_equal(test_networkers[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

# Testing the node_color, node_size, and node_shape args by specifying a node attribute
test_that("fancy node mods graph correctly", {
  skip_on_cran()
  # one-mode network
  ison_marvel_relationships <- dplyr::mutate(ison_marvel_relationships,
                                             nodesize = Appearances/1000)
  testcolnodes <- graphr(ison_marvel_relationships,
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
  test2 <- graphr(ison_southern_women,
                      node_color = "type")
  expect_s3_class(test2, c("ggraph","gg","ggplot"))
  expect_equal(round(test2$data$x[1]), 0)
  expect_equal(round(test2$data$y[1]), 0)
  expect_equal(nrow(test2[["plot_env"]][["lo"]]),
               network_nodes(ison_southern_women))
})

test_that("edge colours and edge size graph correctly", {
  skip_on_cran()
  ison_brandes2 <- ison_brandes %>%
    add_tie_attribute("tiecolour",
                      c("A", "B", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B")) %>%
    add_tie_attribute("weight", c(rep(1:6, 2)))
  test_brandes2 <- graphr(ison_brandes2, edge_color = "tiecolour", edge_size = "weight")
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_colour))
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_width))
})

# Named networks
test_that("named networks plot correctly", {
  skip_on_cran()
  onemode <- graphr(ison_adolescents)
  twomode <- graphr(ison_southern_women)
  expect_equal(onemode[["data"]][["name"]], node_names(ison_adolescents))
  expect_equal(twomode[["data"]][["name"]], node_names(ison_southern_women))
})

# Test that autographr() works with arguments without quotes
test_that("node_group works correctly", {
  skip_on_cran()
  expect_equal(graphr(ison_lawfirm, node_group = gender),
               graphr(ison_lawfirm, node_group = "gender"))
})

test_that("unquoted arguments plot correctly", {
  skip_on_cran()
  expect_equal(graphr(ison_lawfirm, node_color = "Gender"),
               graphr(ison_lawfirm, node_color = Gender))
})

# Layouts
test_that("concentric and circular layouts graph correctly", {
  skip_on_cran()
  test_circle <- graphr(to_giant(ison_marvel_relationships),
                            layout = "circle")
  test_conc <- graphr(to_giant(ison_marvel_relationships),
                          layout = "concentric", membership = "Gender")
  expect_equal(test_circle$plot_env$layout, "circle")
  expect_equal(test_conc$plot_env$layout, "concentric")
  expect_equal(eval(quote(pairlist(...)),
                    envir = test_conc$plot_env)$membership,
               "Gender")
})

test_that("hierarchy and lineage layouts graph correctly", {
  skip_on_cran()
  test_lin <- ison_adolescents %>% 
    mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) %>%
    graphr(layout = "lineage", rank = "year")
  test_hie <- graphr(ison_southern_women,
                         layout = "hierarchy", center = "events")
  expect_equal(test_lin$plot_env$layout, "lineage")
  expect_equal((eval(quote(pairlist(...)),
                     envir = test_lin[["plot_env"]])[["rank"]]),
               "year")
  expect_equal(test_hie$plot_env$layout, "hierarchy")
  expect_equal((eval(quote(pairlist(...)),
                     envir = test_hie[["plot_env"]])[["center"]]),
               "events")
})

test_that("autographr works for diff_model objects", {
  skip_on_cran()
  skip_on_ci()
  test_diff <- graphr(play_diffusion(ison_brandes))
  if (inherits(test_diff$guides, "Guides")) {
    expect_s3_class(test_diff[["guides"]][["guides"]][["shape"]], "GuideLegend")
    expect_s3_class(test_diff[["guides"]][["guides"]][["colour"]], "GuideColourbar")
  } else {
    expect_equal(test_diff[["guides"]][["shape"]][["name"]], "legend")
    expect_equal(test_diff[["guides"]][["colour"]][["name"]], "colorbar")
  }
})

test_that("autographr checks variable names for mapping", {
  skip_on_cran()
  skip_on_ci()
  expect_message(graphr(ison_lawfirm, node_color = "School"),
                 "Please make sure you spelled node color variable correctly.")
})

test_that("concentric layout works when node names are missing", {
  skip_on_cran()
  skip_on_ci()
  llabel <- ison_southern_women |>
    mutate(name = ifelse(type == TRUE, "", name)) |>
    graphr(layout = "concentric")
  expect_true(any(llabel$data$name == ""))
})

test_that("hierarchy layout works for two mode networks", {
  skip_on_cran()
  skip_on_ci()
  tm <- ison_brandes |>
    mutate(type = twomode_type, name = LETTERS[1:11]) |>
    graphr()
  expect_length(unique(tm$data[tm$data$type == TRUE, "y"]), 1)
  expect_length(unique(tm$data[tm$data$type == FALSE, "y"]), 1)
})
