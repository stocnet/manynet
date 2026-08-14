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

test_that("signed-only networks are not marked weighted in any format", {
  # `irps_tribes` holds its signs as weights of -1 and 1, so that no sign is
  # lost when coercing between formats, but records no magnitudes
  expect_true(is_signed(irps_tribes))
  for(net in list(irps_tribes, as_igraph(irps_tribes), as_tidygraph(irps_tribes),
                  as_matrix(irps_tribes), as_network(irps_tribes),
                  as_edgelist(irps_tribes))){
    expect_false(is_weighted(net))
    expect_true(is_signed(net))
  }
  # weights that vary in magnitude are weights again, signed or not
  wtd <- igraph::set_edge_attr(as_igraph(irps_tribes), "weight",
                               value = c(-2, rep(1, net_ties(irps_tribes) - 1)))
  expect_true(is_weighted(wtd))
  expect_true(is_weighted(as_matrix(wtd)))
  # as are weights complementing a separate 'sign' attribute
  both <- igraph::set_edge_attr(wtd, "sign",
                                value = sign(tie_weights(wtd)))
  expect_true(is_weighted(igraph::set_edge_attr(both, "weight",
                                                value = tie_signs(both))))
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

uni <- as_tidygraph(create_filled(5))  |> 
  mutate_ties(type = c(rep("friend",5), rep("enemy",5)),
              weight = rpois(10, lambda = 4))

test_that("to_uniplex works", {
  expect_true(is_uniplex(to_uniplex(uni, "friend")))
  expect_length(to_uniplex(uni, "friend"), length(uni))
  expect_false(is_multiplex(ison_southern_women))
  expect_false(is_multiplex(to_multilevel(ison_southern_women)))
  expect_true(is_twomode(to_uniplex(fict_actually, "appearance")))
  expect_false(is_twomode(to_uniplex(fict_actually, "romance")))
})

test_that("to_uniplex handles networks without tie types", {
  expect_equal(as_matrix(to_uniplex(create_ring(8), "friend")),
               as_matrix(create_ring(8)))
  expect_equal(as_matrix(to_uniplex(ison_southern_women, "participation")),
               as_matrix(ison_southern_women))
})

test_that("to_uniplex reports the available tie types", {
  expect_error(to_uniplex(uni, "nope"), "friend")
  expect_error(to_uniplex(uni), "friend")
})

test_that("to_uniplex works where layers are held in a layer column", {
  expect_equal(as_matrix(to_uniplex(as_stocnet(ison_algebra), "tasks")),
               as_matrix(to_uniplex(ison_algebra, "tasks")))
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
  expect_gt(nrow(as_edgelist(to_reciprocated(ison_brandes))),
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

# Symmetrisation ####

# A weighted digraph with a reciprocated dyad whose directions disagree, which
# is the case that told the three to_undirected() methods apart.
asym <- local({
  m <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  m[1, 2] <- 3; m[2, 1] <- 6; m[3, 4] <- 2; m[4, 5] <- 1
  m
})

test_that("to_undirected agrees across classes for every rule", {
  # regression test: the matrix method binarised tie weights, the igraph
  # method summed them, and the network method returned an asymmetric matrix
  # while reporting the network as undirected
  for (r in c("collapse", "min", "max", "mean", "sum", "product")) {
    from_matrix <- to_undirected(asym, rule = r)
    expect_equal(unname(as_matrix(to_undirected(as_igraph(asym), rule = r))),
                 unname(from_matrix), label = paste0("igraph, rule = ", r))
    expect_equal(unname(as_matrix(to_undirected(as_network(asym), rule = r))),
                 unname(from_matrix), label = paste0("network, rule = ", r))
    expect_true(isSymmetric(unname(from_matrix)),
                label = paste0("symmetry, rule = ", r))
  }
})

test_that("to_undirected reconciles tie values as each rule promises", {
  expect_equal(to_undirected(asym, rule = "collapse")[1, 2], 9)
  expect_equal(to_undirected(asym, rule = "sum")[1, 2], 9)
  expect_equal(to_undirected(asym, rule = "min")[1, 2], 3)
  expect_equal(to_undirected(asym, rule = "max")[1, 2], 6)
  expect_equal(to_undirected(asym, rule = "mean")[1, 2], 4.5)
  expect_equal(to_undirected(asym, rule = "product")[1, 2], 18)
  # a tie in one direction only is kept by every rule but min and product
  expect_equal(to_undirected(asym, rule = "max")[3, 4], 2)
  expect_equal(to_undirected(asym, rule = "min")[3, 4], 0)
  expect_equal(to_undirected(asym, rule = "product")[3, 4], 0)
})

test_that("to_undirected leaves an already undirected network alone", {
  expect_identical(as_matrix(to_undirected(ison_adolescents)),
                   as_matrix(ison_adolescents))
  expect_identical(as_matrix(to_undirected(ison_adolescents, rule = "min")),
                   as_matrix(ison_adolescents))
})

test_that("to_undirected keeps tie attributes other than the weight", {
  # igraph's default combination rule discards everything but the weight
  signed <- to_signed(generate_random(8, directed = TRUE))
  expect_true(is_signed(to_undirected(signed)))
})

test_that("to_undirected does not treat a missing tie as agreement", {
  miss <- asym
  miss[1, 2] <- NA
  expect_true(is.na(to_undirected(miss, rule = "min")[1, 2]))
  expect_true(is.na(to_undirected(miss, rule = "collapse")[1, 2]))
})

test_that("to_undirected records the rule used", {
  expect_match(igraph::graph_attr(to_undirected(as_tidygraph(asym),
                                                rule = "min"), "transform"),
               "symmetrised (min)", fixed = TRUE)
})
