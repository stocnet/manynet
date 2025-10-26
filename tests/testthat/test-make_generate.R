# Tests for the generate family of functions

test_that("random creation works", {
  expect_false(isTRUE(all.equal(generate_random(4,.3), generate_random(4,.3))))
  expect_false(isTRUE(all.equal(generate_random(c(2,4),.3), generate_random(c(2,4),.3))))
  expect_error(generate_random(c(1,2,3)), "must be of length")
  # Bipartite graph
  expect_s3_class(generate_random(ison_southern_women, 0.4), "igraph")
  expect_true(is_twomode(generate_random(ison_southern_women, 0.4)))
})

test_that("generate_smallworld() works", {
  expect_s3_class(generate_smallworld(12, 0.025), "igraph")
  expect_equal(igraph::vcount(generate_smallworld(12, 0.025)), 12)
  expect_s3_class(generate_smallworld(c(6,6), 0.025), "igraph")
})

test_that("generate_scalefree() works", {
  expect_s3_class(generate_scalefree(12, 0.025), "igraph")
  expect_s3_class(generate_scalefree(c(6,6), 0.025), "igraph")
})

test_that("generate_permutation() works", {
  expect_s3_class(generate_permutation(ison_southern_women), "igraph")
})

test_that("generate_configuration works", {
  expect_s3_class(generate_configuration(ison_adolescents), "igraph")
})

test_that("generate_man works", {
  expect_s3_class(generate_man(ison_adolescents), "igraph")
})

test_that("generate_fire works", {
  expect_s3_class(generate_fire(ison_adolescents), "igraph")
})

test_that("generate_islands works", {
  expect_s3_class(generate_islands(ison_adolescents), "igraph")
})

test_that("generate_citations works", {
  expect_s3_class(generate_citations(ison_southern_women), "igraph")
})
