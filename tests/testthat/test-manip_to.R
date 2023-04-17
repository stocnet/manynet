southern_women <- as_tidygraph(readRDS(testthat::test_path("sheets",
                                                           "southern_women.Rds")))
test_that("to_unweight works", {
  expect_false(is_weighted(southern_women))
  st <- igraph::set.edge.attribute(southern_women, "weight",
                                   value = sample(1:93, 93))
  expect_true(is_weighted(st))
  expect_false(is_weighted(to_unweighted(st)))
})

test_that("to_unnamed works",{
  expect_true(is_labelled(southern_women))
  expect_false(is_labelled(to_unnamed(southern_women)))
})

test_that("to_undirected works",{
  expect_false(is_directed(southern_women))
  expect_false(is_directed(to_undirected(southern_women)))
  #expect_true(is_directed(to_directed(southern_women)))
})

test_that("to_redirected works",{
  expect_false(is_directed(to_redirected(southern_women)))
  expect_equal(colnames(as_matrix(southern_women)), 
               rownames(to_redirected(as_matrix(southern_women))))
})

test_that("to_onemode works",{
  expect_equal(c(to_onemode(southern_women))[3],
               c(igraph::delete_vertex_attr(southern_women, "type"))[3])
  expect_equal(as_matrix(to_onemode(as_tidygraph(southern_women))),
               as_matrix(as_tidygraph(igraph::delete_vertex_attr(southern_women,
                                                                 "type"))))
})

test_that("to_uniplex works", {
  expect_false(is_multiplex(southern_women))
  expect_false(is_multiplex(to_multilevel(southern_women)))
})

test_that("to_simplex works", {
  expect_false(is_complex(southern_women))
  expect_false(is_complex(to_simplex(southern_women)))
})

test_that("to_unsigned works", {
  expect_false(is_signed(southern_women))
  expect_false(is_signed(to_unsigned(southern_women)))
  expect_false(all(as_matrix(to_unsigned(southern_women, "positive")) != 
                 as_matrix(to_unsigned(southern_women, "negative"))))
})

test_that("to_named works", {
  expect_true(is_labelled(southern_women))
  expect_false(is_labelled(to_unnamed(southern_women)))
  expect_true(is_labelled(to_named(to_unnamed(southern_women))))
  expect_true(is_labelled(to_named(southern_women,
                                   seq_len(igraph::vcount(southern_women)))))
})

test_that("multilevel works", {
  expect_true(is_twomode(southern_women))
  expect_false(is_twomode(to_multilevel(southern_women)))
})
