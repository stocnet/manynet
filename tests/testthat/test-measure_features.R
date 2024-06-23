set.seed(123)

test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_s3_class(net_smallworld(ison_southern_women), "network_measure")
  expect_equal(as.numeric(net_smallworld(ison_southern_women)), -1.04, tolerance = 0.02)
})

test_that("net_balance works", {
  expect_s3_class(net_balance(ison_marvel_relationships), "network_measure")
  expect_equal(as.numeric(net_balance(ison_marvel_relationships)), 0.668, tolerance = 0.01)
  expect_length(net_balance(ison_marvel_relationships), 1)
  expect_error(net_balance(ison_adolescents))
})

test_that("net_modularity works for two mode networks", {
  expect_s3_class(net_modularity(ison_southern_women,
                                     node_in_partition(ison_southern_women)), "network_measure")
  expect_length(net_modularity(ison_southern_women,
                                   node_in_partition(ison_southern_women)), 1)
})

test_that("net_core works", {
  expect_s3_class(net_core(ison_adolescents), "network_measure")
  expect_equal(length(net_core(ison_adolescents)),
               length(net_core(ison_southern_women)))
})

test_that("net_factions works", {
  expect_s3_class(net_factions(ison_adolescents), "network_measure")
  expect_equal(length(net_factions(ison_adolescents)),
               length(net_factions(ison_southern_women)))
})
