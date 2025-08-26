test_that("to_unweight works", {
  expect_false(is_weighted(ison_southern_women))
  st <- igraph::set_edge_attr(ison_southern_women, "weight",
                                   value = sample(1:89, 89))
  expect_true(is_weighted(st))
  expect_false(is_weighted(to_unweighted(st)))
  expect_false(is_weighted(to_unweighted(as_igraph(st))))
  expect_false(is_weighted(to_unweighted(as_matrix(st))))
  expect_false(is_weighted(to_unweighted(as_network(st))))
  expect_false(is_weighted(to_unweighted(as_edgelist(st))))
})

test_that("to_unnamed works",{
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_labelled(to_unnamed(ison_southern_women)))
  expect_false(is_labelled(to_unnamed(as_igraph(ison_southern_women))))
  expect_false(is_labelled(to_unnamed(as_matrix(ison_southern_women))))
  expect_false(is_labelled(to_unnamed(as_network(ison_southern_women))))
  expect_false(is_labelled(to_unnamed(as_edgelist(ison_southern_women))))
})

test_that("to_undirected works",{
  expect_false(is_directed(ison_southern_women))
  expect_false(is_directed(to_undirected(ison_southern_women)))
  expect_false(is_directed(to_undirected(as_igraph(ison_southern_women))))
  expect_false(is_directed(to_undirected(as_matrix(ison_southern_women))))
  expect_false(is_directed(to_undirected(as_network(ison_southern_women))))
  expect_false(is_directed(to_undirected(as_edgelist(ison_southern_women))))
})

test_that("to_directed works",{
  expect_false(is_directed(ison_brandes))
  expect_true(is_directed(to_directed(ison_brandes)))
  expect_true(is_directed(to_directed(as_igraph(ison_brandes))))
  expect_true(is_directed(to_directed(as_matrix(ison_brandes))))
  expect_true(is_directed(to_directed(as_network(ison_brandes))))
  #expect_true(is_directed(to_directed(ison_southern_women))) # twomode?
})

test_that("to_redirected works",{
  expect_true(is_directed(to_redirected(to_directed(ison_brandes))))
  expect_true(is_directed(to_redirected(to_directed(as_igraph(ison_brandes)))))
  expect_true(is_directed(to_redirected(to_directed(as_matrix(ison_brandes)))))
  expect_true(is_directed(to_redirected(to_directed(as_network(ison_brandes)))))
  expect_false(is_directed(to_redirected(ison_southern_women)))
  expect_equal(colnames(as_matrix(ison_southern_women)), 
               rownames(to_redirected(as_matrix(ison_southern_women))))
})

uni <- as_tidygraph(create_filled(5)) %>%
  mutate_ties(type = c(rep("friend",5), rep("enemy",5)),
              weight = rpois(10, lambda = 4))

test_that("to_uniplex works", {
  expect_true(is_uniplex(to_uniplex(uni, "friend")))
  expect_true(is_weighted(to_uniplex(as_igraph(uni), "friend")))
  expect_length(to_uniplex(uni, "friend"), length(uni))
  expect_false(is_multiplex(ison_southern_women))
  expect_false(is_multiplex(to_multilevel(ison_southern_women)))
})

test_that("to_acylic works", {
  expect_false(is_directed(as_igraph(ison_brandes)))
  expect_true(is_directed(to_acyclic(ison_brandes)))
  expect_true(is_directed(to_acyclic(as_igraph(ison_brandes))))
  expect_true(is_directed(to_acyclic(as_matrix(ison_brandes))))
  expect_true(is_directed(to_acyclic(as_network(ison_brandes))))
})

test_that("to_reciprocated works",{
  expect_true(is_directed(to_reciprocated(ison_brandes)))
  expect_true(is_directed(to_reciprocated(as_igraph(ison_brandes))))
  expect_true(isSymmetric(to_reciprocated(as_matrix(ison_brandes))))
  expect_true(is_directed(to_reciprocated(to_directed(as_network(ison_brandes)))))
  expect_true(nrow(as_edgelist(to_reciprocated(ison_brandes))) >
                length(ison_brandes)*2)
})

test_that("to_onemode works",{
  expect_false(is_twomode(to_onemode(ison_southern_women)))
  expect_equal(c(to_onemode(ison_southern_women))[3],
               c(igraph::delete_vertex_attr(ison_southern_women, "type"))[3])
  expect_equal(as_matrix(to_onemode(as_tidygraph(ison_southern_women))),
               as_matrix(as_tidygraph(
                 igraph::delete_vertex_attr(ison_southern_women, "type"))))
})

test_that("to_simplex works", {
  expect_true(is_complex(fict_lotr))
  expect_false(is_complex(to_simplex(fict_lotr)))
  expect_false(is_complex(to_simplex(as_igraph(fict_lotr))))
  expect_false(is_complex(to_simplex(as_matrix(fict_lotr))))
  expect_false(is_complex(to_simplex(as_igraph(fict_lotr))))
})

test_that("to_unsigned works", {
  expect_false(is_signed(ison_southern_women))
  expect_false(is_signed(to_unsigned(ison_southern_women)))
  expect_false(is_signed(to_unsigned(as_igraph(ison_southern_women))))
  expect_false(is_signed(to_unsigned(as_matrix(ison_southern_women))))
  expect_false(is_signed(to_unsigned(as_network(ison_southern_women))))
  expect_false(all(as_matrix(to_unsigned(ison_southern_women, "positive")) != 
                 as_matrix(to_unsigned(ison_southern_women, "negative"))))
})

test_that("to_named works", {
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_labelled(to_unnamed(ison_southern_women)))
  expect_false(is_labelled(to_unnamed(as_igraph(ison_southern_women))))
  expect_false(is_labelled(to_unnamed(as_matrix(ison_southern_women))))
  expect_false(is_labelled(to_unnamed(as_network(ison_southern_women))))
  expect_false(is_labelled(to_unnamed(as_edgelist(ison_southern_women))))
  expect_true(is_labelled(to_named(to_unnamed(ison_southern_women))))
  expect_true(is_labelled(to_named(ison_southern_women,
                                   seq_len(igraph::vcount(ison_southern_women)))))
})

test_that("multilevel works", {
  expect_true(is_twomode(ison_southern_women))
  expect_false(is_twomode(to_multilevel(ison_southern_women)))
  expect_false(is_twomode(to_multilevel(as_igraph(ison_southern_women))))
  expect_false(is_twomode(to_multilevel(as_matrix(ison_southern_women))))
})

test_that("to_twomode works", {
  expect_false(is_twomode(ison_algebra))
  expect_true(is_twomode(to_twomode(ison_algebra, "type")))
  expect_true(is_twomode(to_twomode(as_igraph(ison_algebra), "type")))
  #expect_true(is_twomode(to_twomode(as_network(ison_algebra), "type"))) #twomode
})
