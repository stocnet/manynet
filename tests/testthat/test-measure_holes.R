test_that("redundancy is reported correctly", {
  expect_s3_class(node_redundancy(ison_brandes), "node_measure")
  expect_s3_class(node_redundancy(ison_southern_women), "node_measure")
  expect_equal(length(node_redundancy(ison_brandes)), network_nodes(ison_brandes))
  expect_equal(length(node_redundancy(ison_southern_women)),
               network_nodes(ison_southern_women))
  expect_named(node_redundancy(ison_southern_women))
})

test_that("effective size is calculated and reported correctly", {
  expect_s3_class(node_effsize(ison_brandes), "node_measure")
  expect_s3_class(node_effsize(ison_southern_women), "node_measure")
  expect_equal(length(node_effsize(ison_brandes)), network_nodes(ison_brandes))
  expect_equal(length(node_effsize(ison_southern_women)),
               network_nodes(ison_southern_women))
  expect_named(node_effsize(ison_southern_women))
  expect_equal(unname(node_effsize(ison_southern_women)[1:3]), c(2.5,1.38,2.46), tolerance = 0.01)
})

test_that("efficiency is reported correctly", {
  expect_s3_class(node_efficiency(ison_brandes), "node_measure")
  expect_s3_class(node_efficiency(ison_southern_women), "node_measure")
  expect_equal(length(node_efficiency(ison_brandes)), network_nodes(ison_brandes))
  expect_equal(length(node_efficiency(ison_southern_women)),
               network_nodes(ison_southern_women))
})

test_that("constraint scores are reported correctly for two-mode notworks",{
  expect_equal(round(unname(node_constraint(ison_southern_women)[1:3]),2), c(0.28, 0.31, 0.29))
  # expect_named(node_constraint(ison_southern_women)[1:3], c("Evelyn", "Laura", "Theresa"))
})

om <- igraph::graph(edges = c(1,2, 2,3), n = 4, directed = FALSE) 

test_that("constraint scores are reported correctly for one-mode notworks",{
  expect_equal(round(unname(node_constraint(ison_adolescents)[1:3]),2), c(1, .43, .57))
})

test_that("hierarchy is reported correctly", {
  expect_s3_class(node_hierarchy(ison_brandes), "node_measure")
  expect_s3_class(node_hierarchy(ison_southern_women), "node_measure")
  expect_equal(length(node_hierarchy(ison_brandes)), network_nodes(ison_brandes))
  expect_equal(length(node_hierarchy(ison_southern_women)),
               network_nodes(ison_southern_women))
  expect_named(node_hierarchy(ison_southern_women))
})