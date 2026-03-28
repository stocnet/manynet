# Tests for the generate family of functions

gen_funs <- funs_objs[grepl("generate_", names(funs_objs)) &
                           !grepl("permutation|utilities|man|islands|fire|config|citation", names(funs_objs))]

for(fn in names(gen_funs)) {
  test_that(paste(fn, "creates an object of the correct class"), {
    expect_s3_class(gen_funs[[fn]](10), "igraph")
    expect_s3_class(gen_funs[[fn]](c(10,10)), "igraph")
  })
  test_that(paste(fn, "creates an object with the correct number of nodes"), {
    expect_values(net_nodes(gen_funs[[fn]](10)), 10)
    expect_values(net_nodes(gen_funs[[fn]](c(5,5))), 10)
  })
  test_that(paste(fn, "creates an object with the correct directedness"), {
    expect_false(is_directed(gen_funs[[fn]](10)))
    expect_true(is_directed(gen_funs[[fn]](10, directed = TRUE)))
  })
}

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

test_that("generate_configuration works", {
  expect_s3_class(generate_configuration(ison_adolescents), "igraph")
  expect_s3_class(generate_configuration(ison_southern_women), "igraph")
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
  expect_s3_class(generate_citations(ison_adolescents), "igraph")
})
