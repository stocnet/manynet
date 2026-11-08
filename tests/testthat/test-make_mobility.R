# That play_segregation() runs and returns what it should
# is tested for the family in test-functional_makes.R. What it cannot test is
# a network in which some nodes start with no value.
test_that("play_segregation runs where some nodes have no starting value", {
  set.seed(1234)
  start <- stats::rbinom(100, 1, prob = 0.5)
  start[sample(seq_len(100), 20)] <- NA
  lattice <- add_node_attribute(create_lattice(100), "start", start)
  out <- play_segregation(lattice, "start", 0.5)
  expect_true(is_manynet(out))
  expect_equal(as.numeric(net_nodes(out)), 100)
})
