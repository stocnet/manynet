# # Census function family tests
set.seed(123)
task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))

test_that("node_by_tie census works", {
  test <- node_by_tie(task_eg)
  expect_equal(test[1:4], rep(0, 4))
  expect_s3_class(test, "node_motif")
  expect_output(print(test), "fromA")
  expect_equal(nrow(summary(test, membership = node_in_roulette(task_eg, 3))),
               3)
})

test_that("node_by_dyad census works", {
  test <- node_by_dyad(ison_adolescents)
  expect_s3_class(test, "node_motif")
  expect_equal(colnames(test)[1:2], c("Mutual", "Null"))
})

test_that("node_by_triad census works", {
  test <- node_by_triad(task_eg)
  expect_equal(top3(test[,16]), c(7,8,6))
  expect_s3_class(test, "node_motif")
  expect_equal(colnames(test)[1:3], c("003", "012", "102"))
})

test_that("net_by_dyad census works", {
  test <- net_by_dyad(ison_adolescents)
  expect_equal(test[[1]], 10)
  expect_equal(test[[2]], 18)
  expect_equal(names(test), c("Mutual", "Null"))
  expect_s3_class(test, "network_motif")
  expect_output(print(test), "Mutual")
})

test_that("net_by_triad census works", {
  test <- net_by_triad(ison_adolescents)
  expect_equal(test[[1]], 13)
  expect_equal(test[[3]], 29)
  expect_equal(names(test), c("003", "012", "102", "201", "210", "300"))
  expect_s3_class(test, "network_motif")
  expect_equal(names(summary(test)), c("003", "012", "102", "201", "210", "300"))
  # Error
  expect_error(net_by_triad(ison_southern_women))
})

test_that("net_by_tetrad census works", {
  test <- net_by_tetrad(ison_southern_women)
  expect_s3_class(test, "network_motif")
  expect_values(c(test)[1], 12388)
})

test_that("node_by_tetrad census works", {
  test <- node_by_tetrad(ison_southern_women)
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

