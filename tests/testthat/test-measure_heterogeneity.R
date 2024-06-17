#*************** Test the heterogeneity family of functions ******************#

test_that("diversity function works", {
  expect_equal(as.numeric(network_diversity(ison_marvel_relationships, "Gender")), 0.306, tolerance = 0.001)
  expect_equal(as.numeric(network_diversity(ison_marvel_relationships, "Gender", "Rich")),
               c(0.3367,0.1653), tolerance = 0.001)
})

test_that("heterophily function works", {
  expect_equal(as.numeric(network_heterophily(ison_networkers, "Discipline")), .1704, tolerance = 0.001)
  expect_length(node_heterophily(ison_networkers, "Discipline"),
                network_nodes(ison_networkers))
  expect_s3_class(node_heterophily(ison_networkers, "Discipline"), "node_measure")
})

test_that("assortativity function works", {
  expect_length(network_assortativity(ison_networkers), 1)
  expect_s3_class(network_assortativity(ison_networkers), "network_measure")
})

test_that("richness function works", {
  expect_length(network_richness(ison_networkers), 1)
  expect_equal(as.numeric(network_richness(ison_networkers)), 3)
  expect_s3_class(network_richness(ison_networkers), "network_measure")
  expect_length(node_richness(ison_networkers, "type"), 32)
  expect_s3_class(node_richness(ison_networkers, "type"), "node_measure")
})
