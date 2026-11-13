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
})

test_that("a negative tie repels, and the absolute weights sum to 1", {
  signed <- add_tie_attribute(create_ring(4), "weight", c(1, -1, 1, 1))
  W <- to_influence(as_matrix(signed))
  expect_equal(rowSums(abs(W)), rep(1, 4))
  expect_equal(W[2, 3], -0.5)
  expect_equal(W[3, 2], -0.5)
  expect_error(to_influence(as_matrix(signed), W * 2), "absolute")
  expect_equal(to_influence(as_matrix(signed), W), W)
})

test_that("play_opinion() splits a balanced signed network into two camps", {
  # positive ties inside two camps, and negative ties between them
  camps <- rep(1:2, each = 3)
  A <- ifelse(outer(camps, camps, "=="), 1, -1)
  diag(A) <- 0
  net <- play_opinion(A, c(1, 0.5, 0, 0.2, -0.4, 0.1), steps = 200)
  end <- unlist(tail(as_opinion(net), 1))
  expect_equal(end[1:3], rep(end[1], 3), tolerance = 0.01, ignore_attr = TRUE)
  expect_equal(end[4:6], rep(-end[1], 3), tolerance = 0.01, ignore_attr = TRUE)
  expect_gt(abs(end[1]), 0.05)
  # one tie that breaks the balance pulls everyone to 0
  A[1, 2] <- A[2, 1] <- -1
  net <- play_opinion(A, c(1, 0.5, 0, 0.2, -0.4, 0.1), steps = 500)
  expect_equal(unlist(tail(as_opinion(net), 1)), rep(0, 6),
               tolerance = 0.01, ignore_attr = TRUE)
  expect_error(play_opinion(A, rep(0, 6), rule = "deffuant"), "negative")
})
