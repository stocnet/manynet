test_that("to_unweight works", {
  expect_false(is_weighted(ison_southern_women))
  st <- igraph::set.edge.attribute(ison_southern_women, "weight",
                                   value = sample(1:93, 93))
  expect_true(is_weighted(st))
  expect_false(is_weighted(to_unweighted(st)))
})

test_that("to_unnamed works",{
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_labelled(to_unnamed(ison_southern_women)))
})

test_that("to_undirected works",{
  expect_false(is_directed(ison_southern_women))
  expect_false(is_directed(to_undirected(ison_southern_women)))
  #expect_true(is_directed(to_directed(ison_southern_women)))
})

test_that("to_directed works",{
  expect_true(is_directed(to_directed(ison_brandes)))
})

test_that("to_redirected works",{
  expect_false(is_directed(to_redirected(ison_southern_women)))
  expect_equal(colnames(as_matrix(ison_southern_women)), 
               rownames(to_redirected(as_matrix(ison_southern_women))))
})

test_that("to_onemode works",{
  expect_equal(c(to_onemode(ison_southern_women))[3],
               c(igraph::delete_vertex_attr(ison_southern_women, "type"))[3])
  expect_equal(as_matrix(to_onemode(as_tidygraph(ison_southern_women))),
               as_matrix(as_tidygraph(igraph::delete_vertex_attr(ison_southern_women,
                                                                 "type"))))
})

test_that("to_reciprocated works",{
  expect_true(is_directed(to_reciprocated(ison_brandes)))
  expect_true(nrow(as_edgelist(to_reciprocated(ison_brandes))) >
                  length(ison_brandes)*2)
})

test_that("to_uniplex works", {
  expect_false(is_multiplex(ison_southern_women))
  expect_false(is_multiplex(to_multilevel(ison_southern_women)))
})

test_that("to_simplex works", {
  expect_false(is_complex(ison_southern_women))
  expect_false(is_complex(to_simplex(ison_southern_women)))
})

test_that("to_unsigned works", {
  expect_false(is_signed(ison_southern_women))
  expect_false(is_signed(to_unsigned(ison_southern_women)))
  expect_false(all(as_matrix(to_unsigned(ison_southern_women, "positive")) != 
                 as_matrix(to_unsigned(ison_southern_women, "negative"))))
})

test_that("to_named works", {
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_labelled(to_unnamed(ison_southern_women)))
  expect_true(is_labelled(to_named(to_unnamed(ison_southern_women))))
  expect_true(is_labelled(to_named(ison_southern_women,
                                   seq_len(igraph::vcount(ison_southern_women)))))
})

test_that("multilevel works", {
  expect_true(is_twomode(ison_southern_women))
  expect_false(is_twomode(to_multilevel(ison_southern_women)))
})
