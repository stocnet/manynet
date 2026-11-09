test_that("to_influence() returns rows that sum to 1, and keeps the class", {
  for(net in list(ison_adolescents, as_igraph(ison_adolescents),
                  as_tidygraph(ison_adolescents), as_network(ison_adolescents),
                  as_matrix(ison_adolescents), ison_networkers)){
    out <- to_influence(net)
    expect_identical(class(out)[1], class(net)[1])
    expect_equal(unname(rowSums(as_matrix(out))),
                 rep(1, net_nodes(net)))
  }
})

test_that("to_influence() keeps self-loops as self-weights", {
  net <- igraph::make_graph(~A-B-C)
  igraph::E(net)$weight <- 1
  net <- igraph::add_edges(net, c(1, 1), weight = 2)
  W <- as_matrix(to_influence(net))
  expect_equal(W["A", "A"], 2/3)
  expect_equal(W["B", ], c(A = 0.5, B = 0, C = 0.5))
})

test_that("to_influence() reads a tie attribute or a matrix", {
  net <- add_tie_attribute(create_ring(4), "trust", c(1, 3, 1, 3))
  W <- to_influence(as_matrix(net |> add_tie_attribute("weight", c(1, 3, 1, 3))))
  expect_equal(as_matrix(to_influence(net, "trust")), W,
               ignore_attr = TRUE)
  expect_error(to_influence(net, "nothing"), "no tie attribute")
  given <- matrix(1/4, 4, 4)
  expect_equal(to_influence(as_matrix(net), given), given)
})

test_that("a node that listens to no one listens only to itself", {
  W <- to_influence(as_matrix(create_star(4, directed = TRUE)))
  expect_equal(diag(W)[-1], rep(1, 3))
})

test_that("to_influence() explains what it cannot use", {
  expect_error(to_influence(ison_southern_women), "one-mode")
  signed <- add_tie_attribute(create_ring(4), "weight", c(1, -1, 1, 1))
  expect_error(to_influence(signed), "negative")
})
