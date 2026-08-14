# delete_isolates() ####

# create_ring(4) plus two nodes tied to nothing
stranded <- add_nodes(create_ring(4), 2)

test_that("delete_isolates removes nodes without ties", {
  expect_equal(as.numeric(net_nodes(stranded)), 6)
  expect_equal(as.numeric(net_nodes(delete_isolates(stranded))), 4)
  expect_equal(as.numeric(net_ties(delete_isolates(stranded))),
               as.numeric(net_ties(stranded)))
})

isolate <- ison_adolescents %>%
  activate(edges) %>%
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
    nodes = tibble::tibble(name = c("A", "B", "C", "D")),
    ties = tibble::tibble(from = c(2L, 3L), to = c(3L, 4L)),
    changes = tibble::tibble(node = c(2L, 4L), time = c(1L, 2L),
                             var = "name", value = c("B", "D"))
  )
  out <- delete_isolates(sn)
  expect_equal(out$nodes$name, c("B", "C", "D"))
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
    nodes = tibble::tibble(name = c("A", "B", "C", "D"),
                           group = c(NA, "x", "y", "z")),
    ties = tibble::tibble(from = c(1L, 2L, 3L), to = c(2L, 3L, 4L)),
    changes = tibble::tibble(node = c(1L, 4L), time = c(1L, 2L),
                             var = "group", value = c("q", "z"))
  )
  out <- delete_incomplete(sn)
  expect_equal(out$nodes$name, c("B", "C", "D"))
  # the tie from the dropped node 1 goes with it, the rest shift down
  expect_equal(out$ties$from, c(1, 2))
  expect_equal(out$ties$to, c(2, 3))
  expect_equal(out$changes$node, 3)
})

test_that("delete_incomplete leaves complete networks untouched", {
  expect_equal(as_matrix(delete_incomplete(ison_adolescents)),
               as_matrix(ison_adolescents))
})
