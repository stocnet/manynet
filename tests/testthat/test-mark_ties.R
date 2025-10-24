graph1 <- igraph::make_directed_graph(c(1,2,1,5,2,3,2,4,3,5,4,5,5,1))
graph2 <- igraph::make_undirected_graph(c(1,1,1,2,2,4,3,4,3,4))

test_that("tie_is_reciprocated works", {
  expect_length(tie_is_reciprocated(graph1), 7)
  expect_true(tie_is_reciprocated(graph1)[2], tie_is_reciprocated(graph1)[7])
})

test_that("tie_is_multiple works", {
  expect_true(tie_is_multiple(graph2)[5])
})

test_that("tie_is_loop works", {
  expect_true(tie_is_loop(graph2)[1])
  expect_false(tie_is_loop(graph2)[2], tie_is_loop(graph2)[3], tie_is_loop(graph2)[4])
})

test_that("tie_is_bridge works", {
  expect_equal(length(tie_is_bridge(graph1)), c(net_ties(graph1)))
})

test_that("tie_is_path works", {
  expect_equal(sum(tie_is_path(ison_adolescents, "Betty", "Alice")), 2)
})

test_that("tie_is_triangular works", {
  expect_equal(sum(tie_is_triangular(ison_adolescents)), 7)
})

test_that("directed triangle tie marks work", {
  acy_adol <- to_acyclic(ison_adolescents)
  expect_true(sum(tie_is_transitive(acy_adol)) >= 3)
  expect_true(sum(tie_is_triplet(acy_adol)) >= 3)
  expect_false(any(tie_is_cyclical(acy_adol)))
  expect_false(any(tie_is_simmelian(acy_adol)))
})

test_that("tie_is_max works", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(length(tie_is_max(tie_betweenness(graph1))),
               c(net_ties(graph1)))
  expect_equal(sum(tie_is_max(tie_betweenness(graph1)) == TRUE), 1)
  expect_s3_class(tie_is_max(tie_betweenness(graph1)), "logical")
})

test_that("tie_is_min works", {
  skip_on_ci()
  skip_on_cran()
  expect_equal(length(tie_is_min(tie_betweenness(ison_brandes))),
               c(net_ties(ison_brandes)))
  expect_equal(sum(tie_is_min(tie_betweenness(ison_brandes)) == TRUE), 1)
  expect_s3_class(tie_is_min(tie_betweenness(ison_brandes)), "logical")
})

test_that("tie_is_feedback() mark functions work", {
  expect_equal(as.character(tie_is_feedback(ison_adolescents)),
               c("FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "FALSE",
                 "TRUE", "FALSE", "FALSE"))
})
