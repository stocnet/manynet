set.seed(1234)

test_that("node_is_cutpoint", {
  expect_true(exists("node_is_cutpoint"))
  test_that("returns correct type", {
    expect_s3_class(node_is_cutpoint(ison_algebra), "node_mark")
  })
  expect_length(node_is_cutpoint(ison_southern_women),
                c(net_nodes(ison_southern_women)))
})

test_that("node_is_isolate", {
  f <- node_is_isolate
  expect_true(is.function(f))
  test <- f(ison_brandes)
  test_that("returns correct values", {
    expect_equal(length(test), c(net_nodes(ison_brandes)))
  })
  test_that("returns correct type", {
    expect_s3_class(test, "logical")
  })
})

test_that("node_is_fold", {
  expect_true(exists("node_is_fold"))
  test <- node_is_fold(create_explicit(A-B, B-C, A-C, C-D, C-E, D-E))
  test_that("returns correct values", {
    expect_equal(as.logical(test), c(F,F,T,F,F))
  })
  test_that("returns correct type", {
    expect_s3_class(test, "node_mark")
  })
})

test_that("node_is_max works", {
  skip_on_cran()
  skip_on_ci()
  expect_equal(length(node_is_max(node_betweenness(ison_brandes))),
               c(net_nodes(ison_brandes)))
  expect_equal(sum(node_is_max(node_betweenness(ison_brandes)) == TRUE), 1)
  expect_s3_class(node_is_max(node_betweenness(ison_brandes)), "logical")
})

test_that("node_is_min works", {
  skip_on_cran()
  skip_on_ci()
  expect_equal(length(node_is_min(node_betweenness(ison_brandes))),
               c(net_nodes(ison_brandes)))
  expect_equal(sum(node_is_min(node_betweenness(ison_brandes)) == TRUE), 4)
  expect_s3_class(node_is_min(node_betweenness(ison_brandes)), "logical")
})

test_that("additional node mark functions work", {
  set.seed(1234)
  expect_equal(as.character(node_is_independent(ison_adolescents)),
               c("TRUE", "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE"))
  set.seed(1234)
  expect_equal(as.character(node_is_core(ison_brandes)),
               c("FALSE", "FALSE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE",
                 "FALSE", "TRUE", "FALSE", "FALSE"))
  set.seed(1234)
  expect_equal(as.character(node_is_mentor(ison_adolescents)),
               c("FALSE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE"))
  set.seed(1234)
  expect_equal(as.character(node_is_latent(play_diffusion(create_tree(6), latency = 1), time = 1)),
               c("FALSE", "TRUE",  "TRUE",  "FALSE", "FALSE", "FALSE"))
  set.seed(1234)
  expect_equal(as.character(node_is_infected(play_diffusion(create_tree(6)), time = 1)),
               c("TRUE", "TRUE",  "TRUE",  "FALSE", "FALSE", "FALSE"))
  set.seed(1234)
  expect_equal(as.character(node_is_recovered(play_diffusion(create_tree(6), recovery = 0.5), time = 3)),
               c("TRUE", "TRUE",  "TRUE",  "FALSE", "FALSE", "FALSE"))
  set.seed(1234)
  expect_equal(as.character(node_is_exposed(manynet::create_tree(6), mark = c(1,3))),
               c("FALSE", "TRUE",  "FALSE",  "FALSE", "FALSE", "TRUE"))
  set.seed(1234)
  expect_equal(as.character(node_is_random(ison_adolescents, 1)),
               c("FALSE", "FALSE",  "FALSE",  "TRUE", "FALSE", "FALSE", "FALSE", "FALSE"))
})

test_that("node infection, exposure, and recovery works", {
  skip_on_cran()
  skip_on_ci()
  set.seed(1234)
  .data <- play_diffusion(generate_random(to_undirected(to_unnamed(ison_networkers)), with_attr = FALSE),
                          seeds = 10, latency = 0.25, recovery = 0.2, steps = 10)
  expect_true(c(node_is_infected(.data, time = 0))[dplyr::filter(summary(.data),
                                                                 event == "I" & t == 0)$nodes])
  expect_true(c(node_is_recovered(.data, time = 4))[dplyr::filter(summary(.data),
                                                                  event == "R" & t == 4)$nodes[1]])
  expect_true(c(node_is_latent(.data, time = 2))[dplyr::filter(summary(.data),
                                                               event == "E" & t == 2)$nodes[1]])
})


