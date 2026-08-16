# Test assembling networks into layers, and flattening those layers again

comb_a <- matrix(c(0, 3, 1, 3, 0, 0, 1, 0, 0), 3, 3,
                 dimnames = list(LETTERS[1:3], LETTERS[1:3]))
comb_b <- matrix(c(0, 2, 0, 2, 0, 4, 0, 4, 0), 3, 3,
                 dimnames = list(LETTERS[1:3], LETTERS[1:3]))
comb <- from_layers(a = comb_a, b = comb_b)

# from_layers() ---------------------------------------------------------------

test_that("from_layers keeps the networks as layers", {
  marriage <- to_uniplex(ison_florentine, "marriage")
  business <- to_uniplex(ison_florentine, "business")
  out <- from_layers(marriage = marriage, business = business)
  expect_true(is_multiplex(out))
  expect_setequal(layer_names(out), c("marriage", "business"))
})

test_that("from_layers accepts a list, so that it reverses to_layers", {
  flor <- as_tidygraph(ison_florentine)
  out <- from_layers(to_layers(flor))
  expect_true(is_multiplex(out))
  expect_setequal(layer_names(out), layer_names(flor))
  # a uniplex network has one layer, and reassembling one network gives it back
  expect_equal(as_matrix(from_layers(to_layers(ison_adolescents))),
               as_matrix(ison_adolescents))
})

test_that("from_layers matches unlabelled networks of equal size by position", {
  out <- from_layers(ring = create_ring(8), star = create_star(8))
  expect_true(is_multiplex(out))
  expect_false(is_labelled(out))
  expect_equal(as.numeric(net_nodes(out)), 8)
  expect_equal(as.numeric(net_ties(out)),
               as.numeric(net_ties(create_ring(8))) +
                 as.numeric(net_ties(create_star(8))))
  # an unlabelled network takes the labelled network's names, node for node
  named <- from_layers(friends = ison_adolescents, ring = create_ring(8))
  expect_true(is_labelled(named))
  expect_equal(as.numeric(net_nodes(named)), 8)
  # but nodes cannot be matched at all where the networks differ in size
  expect_error(from_layers(a = create_ring(8), b = create_star(6)),
               "same size")
})

test_that("from_layers does not make an undirected network directed", {
  # graph_join() returns a directed graph whatever it is given, which would
  # make a tie recorded as A-B differ from the same tie recorded as B-A
  expect_false(is_directed(comb))
  shuffled <- comb_b[c(3, 1, 2), c(3, 1, 2)]
  expect_equal(as_matrix(to_flat(from_layers(a = comb_a, b = shuffled), "sum")),
               as_matrix(to_flat(comb, "sum")))
})

# to_flat() -------------------------------------------------------------------

test_that("to_flat reconciles tie values as each rule promises", {
  expect_equal(as_matrix(to_flat(comb, "max"))[1, 2], 3)
  expect_equal(as_matrix(to_flat(comb, "min"))[1, 2], 2)
  expect_equal(as_matrix(to_flat(comb, "mean"))[1, 2], 2.5)
  expect_equal(as_matrix(to_flat(comb, "sum"))[1, 2], 5)
  expect_equal(as_matrix(to_flat(comb, "product"))[1, 2], 6)
  # a dyad tied in only one layer is untied in the other
  expect_equal(as_matrix(to_flat(comb, "min"))[1, 3], 0)
  expect_equal(as_matrix(to_flat(comb, "product"))[1, 3], 0)
  expect_equal(as_matrix(to_flat(comb, "max"))[1, 3], 1)
})

test_that("to_flat matches nodes by name and over the union of node sets", {
  smaller <- comb_b[1:2, 1:2]
  out <- suppressMessages(as_matrix(to_flat(from_layers(a = comb_a,
                                                        b = smaller), "sum")))
  expect_equal(dim(out), c(3L, 3L))
  # nodes absent from the second network keep the first network's values
  expect_equal(out[1, 3], comb_a[1, 3])
  expect_equal(out[1, 2], comb_a[1, 2] + smaller[1, 2])
})

test_that("to_flat combines more than two layers", {
  # each of the law firm's 71 partners and associates may share up to three
  # of its three relationships with another
  out <- to_flat(ison_lawfirm, rule = "sum")
  expect_false(is_multiplex(out))
  expect_equal(sort(unique(tie_weights(out))), c(1, 2, 3))
  expect_equal(as.numeric(table(tie_weights(out))), c(824, 503, 247))
})

test_that("to_flat flattens what join_ties() produced", {
  # join_ties() marks each network's ties in a column of its own, rather than
  # naming the layers in one 'type' column, but both are multiplex networks
  friends <- to_uniplex(as_tidygraph(ison_lawfirm), "friends")
  advice <- to_uniplex(as_tidygraph(ison_lawfirm), "advice")
  joined <- join_ties(friends, advice)
  expect_true(is_multiplex(joined))
  expect_equal(as.numeric(net_ties(to_flat(joined, "min"))), 358)
  # and the two ways of building a multiplex network flatten to the same thing
  expect_equal(as.numeric(net_ties(to_flat(from_layers(friends = friends,
                                                       advice = advice),
                                           "min"))), 358)
})

test_that("to_flat returns a network holding no layers unaltered", {
  expect_equal(as_matrix(to_flat(ison_adolescents)),
               as_matrix(ison_adolescents))
  expect_equal(as_matrix(to_flat(comb_a)), comb_a)
})

test_that("to_flat returns the class it was given", {
  flor <- ison_florentine
  expect_s3_class(to_flat(flor, "sum"), "stocnet")
  expect_s3_class(to_flat(as_tidygraph(flor), "sum"), "tbl_graph")
  expect_true(inherits(to_flat(as_igraph(flor), "sum"), "igraph"))
  # a 'network' object cannot hold layers: as_network() drops them, so there
  # is nothing left for to_flat() to combine by the time it is called
  expect_s3_class(suppressWarnings(to_flat(as_network(flor), "sum")), "network")
})

test_that("to_flat records the rule used", {
  expect_match(igraph::graph_attr(to_flat(as_tidygraph(ison_florentine), "sum"),
                                  "transform"),
               "combined (sum)", fixed = TRUE)
})

test_that("to_flat does not treat a missing tie as untied", {
  miss <- comb_a
  miss[1, 2] <- NA
  # a missing weight does not survive coercion to a graph, so the layers are
  # combined as the matrices they are
  expect_true(is.na(manynet:::.combine_matrices(miss, comb_b, "sum")[1, 2]))
  expect_true(is.na(manynet:::.combine_matrices(miss, comb_b, "max")[1, 2]))
})
