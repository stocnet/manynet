test_that("node cuts works", {
  expect_s3_class(node_is_cutpoint(ison_algebra), "node_mark")
  expect_length(node_is_cutpoint(ison_southern_women),
                network_nodes(ison_southern_women))
})

test_that("node isolate works", {
  expect_s3_class(node_is_isolate(ison_brandes), "logical")
  expect_equal(length(node_is_isolate(ison_brandes)), network_nodes(ison_brandes))
})

test_that("node_is_max works", {
  skip_on_cran()
  skip_on_ci()
  expect_equal(length(node_is_max(migraph::node_betweenness(ison_brandes))),
               network_nodes(ison_brandes))
  expect_equal(sum(node_is_max(migraph::node_betweenness(ison_brandes)) == TRUE), 1)
  expect_s3_class(node_is_max(migraph::node_betweenness(ison_brandes)), "logical")
})

test_that("node_is_min works", {
  skip_on_cran()
  skip_on_ci()
  expect_equal(length(node_is_min(migraph::node_betweenness(ison_brandes))),
               network_nodes(ison_brandes))
  expect_equal(sum(node_is_min(migraph::node_betweenness(ison_brandes)) == TRUE), 4)
  expect_s3_class(node_is_min(migraph::node_betweenness(ison_brandes)), "logical")
})

test_that("additional node mark functions work", {
  expect_equal(as.character(node_is_independent(ison_adolescents)),
               c("TRUE", "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE"))
  expect_equal(as.character(node_is_core(ison_brandes)),
               c("FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE",
                 "FALSE", "TRUE", "FALSE", "FALSE"))
  expect_equal(as.character(node_is_fold(create_explicit(A-B, B-C, A-C, C-D, C-E, D-E))),
               c("FALSE", "FALSE", "TRUE", "FALSE", "FALSE"))
  expect_equal(as.character(node_is_mentor(ison_adolescents)),
               c("FALSE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE"))
  expect_equal(as.character(node_is_latent(play_diffusion(create_tree(6), latency = 1), time = 1)),
               c("FALSE", "TRUE",  "TRUE",  "FALSE", "FALSE", "FALSE"))
  expect_equal(as.character(node_is_infected(play_diffusion(create_tree(6)), time = 1)),
               c("TRUE", "TRUE",  "TRUE",  "FALSE", "FALSE", "FALSE"))
  expect_equal(as.character(node_is_recovered(play_diffusion(create_tree(6), recovery = 0.5), time = 3)),
               c("TRUE", "FALSE",  "FALSE",  "FALSE", "FALSE", "FALSE"))
  expect_equal(as.character(node_is_exposed(manynet::create_tree(6), mark = c(1,3))),
               c("FALSE", "TRUE",  "FALSE",  "FALSE", "FALSE", "TRUE"))
  set.seed(1234)
  expect_equal(as.character(node_is_random(ison_adolescents, 1)),
               c("FALSE", "FALSE",  "FALSE",  "TRUE", "FALSE", "FALSE", "FALSE", "FALSE"))
})
