# # Census function family tests
set.seed(123)
task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))

test <- node_by_tie(task_eg)
test_that("node tie census works", {
  expect_equal(test[1:4], rep(0, 4))
  expect_s3_class(test, "node_motif")
})

test <- node_by_triad(task_eg)
test_that("node triad census works", {
  expect_equal(top3(test[,16]), c(7,8,6))
  expect_s3_class(test, "node_motif")
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test <- net_by_dyad(ison_adolescents)
test_that("net_dyad census works", {
  expect_equal(test[[1]], 10)
  expect_equal(test[[2]], 18)
  expect_equal(names(test), c("Mutual", "Null"))
  expect_s3_class(test, "network_motif")
  # Error
  expect_error(net_by_dyad(ison_southern_women))
})

test <- net_by_triad(ison_adolescents)
test_that("net_triad census works", {
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_s3_class(test, "network_motif")
  # Error
  expect_error(net_by_triad(ison_southern_women))
})

test <- node_by_quad(ison_southern_women)
test_that("node quad census works", {
  expect_s3_class(test, "node_motif")
  expect_equal(test[1,1], 1241)
})

test_that("net_mixed census works", {
  marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
  test <- net_by_mixed(marvel_friends, ison_marvel_teams)
  expect_s3_class(test, "network_motif")
  expect_equal(unname(test[1]), 1137)
  expect_equal(names(test[1]), "22")
  # Errors
  expect_error(net_by_mixed(ison_southern_women,
                                    ison_marvel_teams))
  expect_error(net_by_mixed(ison_marvel_teams,
                                    ison_southern_women))
  expect_error(net_by_mixed(ison_karateka,
                                    ison_marvel_teams))
})

test <- node_by_path(ison_southern_women)
test_that("node path census works", {
  expect_equal(c(net_nodes(ison_adolescents)),
               nrow(node_by_path(ison_adolescents)))
  expect_s3_class(test, "node_motif")
  expect_true(nrow(node_by_path(ison_southern_women)) ==
                ncol(node_by_path(ison_southern_women)))
})

test <- node_brokering_activity(ison_networkers, "Discipline")
test_that("node activity works", {
  expect_s3_class(test, "node_measure")
  expect_equal(c(net_nodes(ison_networkers)), length(test))
  expect_equal(top3(test), c(333,207,3))
})

test <- node_brokering_exclusivity(ison_networkers, "Discipline")
test_that("node exclusivity works", {
  expect_s3_class(test, "node_measure")
  expect_equal(c(net_nodes(ison_networkers)), length(test))
  expect_equal(top3(test), c(1,0,0))
})

test <- node_in_brokering(ison_networkers, "Discipline")
test_that("node brokering works", {
  expect_s3_class(test, "node_member")
  expect_equal(c(net_nodes(ison_networkers)), length(test))
  expect_equal(top3(test), c("Powerhouse","Connectors","Sideliners"))
})
