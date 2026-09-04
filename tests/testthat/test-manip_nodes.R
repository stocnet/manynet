# delete_isolates() ####

# create_ring(4) plus two nodes tied to nothing
stranded <- add_nodes(create_ring(4), 2)

test_that("delete_isolates removes nodes without ties", {
  expect_equal(as.numeric(net_nodes(stranded)), 6)
  expect_equal(as.numeric(net_nodes(delete_isolates(stranded))), 4)
  expect_equal(as.numeric(net_ties(delete_isolates(stranded))),
               as.numeric(net_ties(stranded)))
})

isolate <- ison_adolescents |>
  activate(edges) |>
  to_subgraph(from == 1:5)

test_that("delete_isolates returns the class it was given", {
  expect_length(delete_isolates(isolate), 5)
  expect_length(delete_isolates(as_igraph(isolate)), 5)
  expect_true(all(rowSums(delete_isolates(as_matrix(isolate))) >= 1))
  expect_length(delete_isolates(as_network(isolate)), 5)
  expect_equal(nrow(delete_isolates(as_edgelist(isolate))), 5)
})

test_that("delete_isolates works over a list of waves", {
  waves <- ison_adolescents |>
    mutate_ties(wave = rep(1995:1998, length.out = 10)) |>
    to_waves(attribute = "wave")
  out <- delete_isolates(waves)
  expect_length(out, length(waves))
  expect_true(all(vapply(out, function(x) as.numeric(net_nodes(x)),
                         numeric(1)) <=
                    vapply(waves, function(x) as.numeric(net_nodes(x)),
                           numeric(1))))
})

test_that("delete_isolates.stocnet reindexes ties and changes", {
  # Node 1 is untied, so every from/to index should shift down by one
  sn <- make_stocnet(
    nodes = tibble::tibble(label = c("A", "B", "C", "D")),
    ties = tibble::tibble(from = c(2L, 3L), to = c(3L, 4L)),
    changes = tibble::tibble(node = c(2L, 4L), time = c(1L, 2L),
                             var = "label", value = c("B", "D"))
  )
  out <- delete_isolates(sn)
  expect_equal(out$nodes$label, c("B", "C", "D"))
  expect_equal(out$ties$from, c(1, 2))
  expect_equal(out$ties$to, c(2, 3))
  expect_equal(out$changes$node, c(1, 3))
})

test_that("delete_isolates.stocnet treats nodes changing but untied as isolates", {
  expect_s3_class(delete_isolates(test_stocnet_obj), "stocnet")
  # every node in the ring fixture is tied, so this is a no-op
  expect_equal(nrow(delete_isolates(test_stocnet_obj)$nodes),
               nrow(test_stocnet_obj$nodes))
})

# delete_incomplete() ####

test_that("delete_incomplete.tbl_graph removes nodes with missing values", {
  graph <- tbl_graph(
    nodes = tibble::tibble(name = c("A", "B", NA, "D")),
    edges = tibble::tibble(from = c(1, 2, 3), to = c(2, 3, 4))
  )
  cleaned_graph <- delete_incomplete(graph)
  expect_equal(nrow(as_tibble(cleaned_graph, active = "nodes")), 3)
  expect_true(all(complete.cases(as_tibble(cleaned_graph, active = "nodes"))))
})

test_that("delete_incomplete.stocnet reindexes ties and changes", {
  sn <- make_stocnet(
    nodes = tibble::tibble(label = c("A", "B", "C", "D"),
                           group = c(NA, "x", "y", "z")),
    ties = tibble::tibble(from = c(1L, 2L, 3L), to = c(2L, 3L, 4L)),
    changes = tibble::tibble(node = c(1L, 4L), time = c(1L, 2L),
                             var = "group", value = c("q", "z"))
  )
  out <- delete_incomplete(sn)
  expect_equal(out$nodes$label, c("B", "C", "D"))
  # the tie from the dropped node 1 goes with it, the rest shift down
  expect_equal(out$ties$from, c(1, 2))
  expect_equal(out$ties$to, c(2, 3))
  expect_equal(out$changes$node, 3)
})

test_that("delete_incomplete leaves complete networks untouched", {
  expect_equal(as_matrix(delete_incomplete(ison_adolescents)),
               as_matrix(ison_adolescents))
})

test_that("delete_incomplete ignores a column that is missing for every node", {
  # 'alcohol' is a placeholder for a variable that ison_classmates records as
  # changes, so it is NA for all 26 nodes. Reading it as missing data would
  # delete every node.
  out <- delete_incomplete(ison_classmates)
  expect_equal(net_nodes(out), 21)
  expect_true("alcohol" %in% net_node_attributes(out))
  expect_equal(net_layers(out), 2)
  expect_no_error(validate_stocnet(out))
})

test_that("a node drop that empties a layer drops that layer's information", {
  sn <- make_stocnet(
    nodes = tibble::tibble(label = c("A", "B", "C", "D"),
                           group = c(NA, "x", "y", "z")),
    ties = tibble::tibble(from = c(2L, 1L), to = c(3L, 4L),
                          layer = c("friends", "kin")),
    info = list(layers = c("friends", "kin"),
                directed = c(friends = TRUE, kin = FALSE),
                observation = c(friends = "panel", kin = "cross-sectional"),
                update = c(friends = "replace", kin = "replace"),
                focal = c("kin", "group"))
  )
  # dropping node A takes the only 'kin' tie with it
  out <- delete_incomplete(sn)
  expect_equal(net_layers(out), 1)
  expect_equal(out$info$layers, "friends")
  expect_equal(out$info$directed, c(friends = TRUE))
  expect_equal(out$info$observation, c(friends = "panel"))
  expect_equal(out$info$update, c(friends = "replace"))
  # 'group' is a node attribute rather than a layer, so it stays focal
  expect_equal(out$info$focal, "group")
  expect_no_error(validate_stocnet(out))
})

test_that("a node drop that empties the network gives an empty network", {
  sn <- make_stocnet(
    nodes = tibble::tibble(label = c("A", "B"), group = c(NA, "x"),
                           school = c("y", NA)),
    ties = tibble::tibble(from = 1L, to = 2L, layer = "friends"),
    info = list(layers = "friends", directed = c(friends = TRUE))
  )
  # each node misses a value in a column that other nodes hold a value in
  out <- delete_incomplete(sn)
  expect_equal(net_nodes(out), 0)
  expect_equal(net_ties(out), 0)
  expect_null(out$info$layers)
  expect_no_error(validate_stocnet(out))
  # snet_info() is silent under the default quiet verbosity
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old))
  expect_message(delete_incomplete(sn), "network is now empty")
})

test_that("filter_nodes empties a layer without error", {
  sn <- make_stocnet(
    nodes = tibble::tibble(label = c("A", "B", "C", "D")),
    ties = tibble::tibble(from = c(2L, 1L), to = c(3L, 4L),
                          layer = c("friends", "kin")),
    info = list(layers = c("friends", "kin"))
  )
  out <- filter_nodes(sn, label != "A")
  expect_equal(out$info$layers, "friends")
  expect_no_error(validate_stocnet(out))
})

test_that("dropping nodes drops and renumbers the missings (#173)", {
  sn <- as_stocnet(ison_adolescents)
  # two dyads the network could have observed and did not
  sn$missings <- tibble::tibble(from = c(6L, 7L), to = c(8L, 1L))
  sn <- validate_stocnet(sn)
  # keeping every node leaves the missings as they were
  expect_equal(to_subgraph(sn, seq_len(8) <= 8)$missings, sn$missings)
  # dropping either end of a dyad drops the dyad with it
  out <- to_subgraph(sn, seq_len(8) <= 4)
  expect_no_error(validate_stocnet(out))
  expect_null(out$missings)
  # a dyad both of whose ends remain is renumbered onto the nodes that are left
  sn$missings <- tibble::tibble(from = c(3L, 7L), to = c(4L, 1L))
  sn <- validate_stocnet(sn)
  kept <- to_subgraph(sn, seq_len(8) >= 3)
  expect_equal(kept$missings$from, 1L)
  expect_equal(kept$missings$to, 2L)
  expect_true(all(unlist(kept$missings) <= nrow(kept$nodes)))
})

test_that("validate_stocnet names every out-of-range id (#173)", {
  sn <- as_stocnet(ison_adolescents)
  sn$missings <- tibble::tibble(from = c(20L, 30L), to = c(1L, 2L))
  # two unmatched ids used to make the message builder itself error
  expect_error(validate_stocnet(sn), "20 and 30")
})

test_that("a one-mode attribute lands on its own nodes when the modes interleave", {
  nodes <- tibble::tibble(label = c("a", "X", "b", "Y", "c"),
                          mode = c("person", "event", "person", "event", "person"))
  ties <- tibble::tibble(from = c(1L, 3L), to = c(2L, 4L))
  sn <- make_stocnet(nodes = nodes, ties = ties)
  expect_equal(mode_nodes(sn), c(3L, 2L))
  # the values of the second mode go to the rows of the second mode
  out <- add_node_attribute(sn, "size", c(10, 20))
  expect_equal(out$nodes$size, c(NA, 10, NA, 20, NA))
  # and the values of the first mode to the rows of the first
  out <- add_node_attribute(sn, "age", c(1, 2, 3))
  expect_equal(out$nodes$age, c(1, NA, 2, NA, 3))
  expect_error(add_node_attribute(sn, "bad", c(1, 2, 3, 4)), "5, not 4")
})
