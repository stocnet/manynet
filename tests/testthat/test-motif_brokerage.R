test_that("node_brokering_activity works", {
  test <- node_brokering_activity(ison_networkers, "Discipline")
  expect_s3_class(test, "node_measure")
  expect_equal(c(net_nodes(ison_networkers)), length(test))
  expect_equal(top3(test), c(333,207,3))
})

test_that("node_brokering_exclusivity works", {
  test <- node_brokering_exclusivity(ison_networkers, "Discipline")
  expect_s3_class(test, "node_measure")
  expect_equal(c(net_nodes(ison_networkers)), length(test))
  expect_equal(top3(test), c(1,0,0))
})

test_that("node_in_brokering works", {
  test <- node_in_brokering(ison_networkers, "Discipline")
  expect_s3_class(test, "node_member")
  expect_equal(c(net_nodes(ison_networkers)), length(test))
  expect_equal(top3(test), c("Powerhouse","Connectors","Sideliners"))
  expect_output(print(node_in_brokering(ison_marvel_teams)), "4 groups")
  expect_output(print(summary(node_in_brokering(ison_marvel_teams))), "Connectors")
})

test_that("node_by_brokerage works", {
  test <- node_by_brokerage(ison_networkers, "Discipline")
  expect_s3_class(test, "node_motif")
  expect_equal(dim(test), c(32,6))
})

test_that("net_by_brokerage works", {
  test <- net_by_brokerage(ison_networkers, "Discipline")
  expect_s3_class(test, "network_motif")
  expect_equal(top3(names(test)), c("Coordinator","Itinerant","Gatekeeper"))
})
