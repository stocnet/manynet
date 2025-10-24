test_that("create empty graph works", {
  expect_true(is_twomode(create_empty(c(5,5))))
  expect_s3_class(create_empty(4), "igraph")
  expect_error(create_empty(c(5,5,5)), "single integer")
  expect_length(create_empty(4), 4)
})

test_that("create filled graph works", {
  expect_true(is_twomode(create_filled(c(5,5))))
  expect_s3_class(create_filled(4), "igraph")
  expect_error(create_filled(c(5,5,5)), "single integer")
  expect_false(is_directed(create_filled(6)))
  expect_false(is_directed(create_filled(c(5,5))))
})

test_that("ring creation works", {
  expect_true(is_twomode(create_ring(c(5,5))))
  expect_equal(unname(as_matrix(create_ring(3))), 
               matrix(c(0,1,1,1,0,1,1,1,0),3,3))
  expect_equal(unname(as_matrix(create_ring(c(5,5), width = 1))), 
               matrix(c(1,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1),5,5))
  expect_equal(unname(as_matrix(create_ring(c(5,5), width = 2))), 
               matrix(c(1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1),5,5))
  expect_error(create_ring(c(5,5,5)), "single integer")
})

test_that("create_degree works", {
  expect_values(net_ties(create_degree(10, outdegree = rep(1:5, 2))), 15)
  expect_values(net_ties(create_degree(c(5,5), outdegree = 2, indegree = 2)), 10)
})

test_that("create_windmill works", {
  expect_values(net_ties(create_windmill(5)), 6)
  expect_values(net_ties(create_windmill(c(5,8))), 16)
})

test_that("create_cycle works", {
  expect_values(net_ties(create_cycle(5)), 5)
  expect_values(net_ties(create_cycle(c(5,5))), 10)
})

test_that("create_wheel works", {
  expect_values(net_ties(create_wheel(5)), 8)
  expect_error(create_wheel(c(5,5)))
})

test_that("star creation works", {
  expect_true(!is_twomode(create_star(5)))
  expect_true(is_twomode(create_star(c(5,5))))
  expect_equal(unname(as_matrix(create_star(c(2,2), FALSE))),
               matrix(c(1,1,0,0),2,2))
  expect_equal(unname(as_matrix(create_star(c(2,2), directed = TRUE))),
               matrix(c(1,0,1,0),2,2))
  expect_error(create_star(c(5,5,5)), "single integer")
})

test_that("tree creation works", {
  expect_true(!is_twomode(create_tree(5)))
  expect_true(is_twomode(create_tree(c(5,6))))
  expect_equal(unname(as_matrix(create_tree(c(2,2)))), matrix(c(1,1,1,0),2,2))
  expect_error(create_tree(c(5,5,5)), "single integer")
})

test_that("create lattice works", {
  expect_s3_class(create_lattice(4), "igraph")
  expect_equal(igraph::vcount(create_lattice(5)), 5)
  expect_false(is_directed(create_lattice(6)))
  expect_true(is_directed(create_lattice(6, directed = TRUE)))
})

test_that("component creation works", {
  expect_true(is_twomode(create_components(c(5,5))))
  expect_equal(unname(as_matrix(create_components(4))),
               matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0),4,4))
  expect_equal(unname(as_matrix(create_components(c(2,4)))),
               matrix(c(1,0,1,0,0,1,0,1),2,4))
  expect_error(create_components(c(5,5,5)), "single integer")
})

test_that("core-periphery creation works", {
  expect_false(is_twomode(create_core(6)))
  expect_true(is_twomode(create_core(c(6,7))))
  expect_equal(igraph::vcount(create_core(c(10,4))), 14)
})

# test_that("nest creation works", {
#   expect_equal(create_nest(2,4, as = "matrix"), matrix(c(1,1,0,1,0,0,0,0),2,4))
#   expect_s3_class(create_nest(2,4, as = "igraph"), "igraph")
#   expect_s3_class(create_nest(2,4, as = "tidygraph"), "tbl_graph")
# })

test_that("explicit creation works", {
  expect_true(is_directed(create_explicit(A -+ B, B -+ C, A +-+ C, D)))
  expect_false(manynet::is_weighted(create_explicit(A -+ B, B -+ C, A +-+ C, D)))
  expect_length(create_explicit(A -+ B, B -+ C, A +-+ C, D), 4)
  expect_s3_class(create_explicit(A -+ B, B -+ C, A +-+ C, D, as = "igraph"),
                  "igraph")
  expect_s3_class(create_explicit(A -+ B, B -+ C, A +-+ C, D, as = "tidygraph"),
                  "tbl_graph")
})
