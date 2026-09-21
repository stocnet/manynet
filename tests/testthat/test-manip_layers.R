# Test assembling networks into layers, and flattening those layers again

comb_a <- matrix(c(0, 3, 1, 3, 0, 0, 1, 0, 0), 3, 3,
                 dimnames = list(LETTERS[1:3], LETTERS[1:3]))
comb_b <- matrix(c(0, 2, 0, 2, 0, 4, 0, 4, 0), 3, 3,
                 dimnames = list(LETTERS[1:3], LETTERS[1:3]))
# the matrices are layered as networks, since a list of matrices is stacked
# into an array instead
comb <- from_layers(a = as_tidygraph(comb_a), b = as_tidygraph(comb_b))

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
  expect_equal(as_matrix(to_flat(from_layers(a = as_tidygraph(comb_a),
                                              b = as_tidygraph(shuffled)), "sum")),
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
  out <- suppressMessages(as_matrix(to_flat(from_layers(
    a = as_tidygraph(comb_a), b = as_tidygraph(smaller)), "sum")))
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
  # GRAND item 4.5, recorded under the "aggregation" name of the transformations
  expect_equal(as_infolist(to_flat(as_tidygraph(ison_florentine),
                                           "sum"))$transformations$aggregation,
               "layers (sum)")
})

test_that("to_uniplex records the ties the other layers held", {
  # GRAND item 4.4, recorded under the "exclusion" name of the transformations
  out <- to_uniplex(ison_bankwiring, "friendship")
  expect_match(as_infolist(out)$transformations$exclusion,
               "^layers other than 'friendship' \\([0-9]+ ties excluded\\)$")
})

test_that("to_flat does not treat a missing tie as untied", {
  miss <- comb_a
  miss[1, 2] <- NA
  # a missing weight does not survive coercion to a graph, so the layers are
  # combined as the matrices they are
  expect_true(is.na(.combine_matrices(miss, comb_b, "sum")[1, 2]))
  expect_true(is.na(.combine_matrices(miss, comb_b, "max")[1, 2]))
})

# Undirected layers of a directed network ------------------------------------

test_that("an undirected layer is reciprocated on coercion and collapsed back", {
  # `ison_bankwiring` is directed as a whole, since `help` and `trades` are
  # asymmetric, but holds its four symmetric layers once per dyad
  expect_false(all(ison_bankwiring$info$directed))
  expect_true(is_directed(ison_bankwiring))
  ties <- nrow(ison_bankwiring$ties)
  expect_gt(igraph::ecount(as_igraph(ison_bankwiring)), ties)
  # the round trip returns the network it started from, in either class
  for(back in list(as_stocnet(as_igraph(ison_bankwiring)),
                   as_stocnet(as_network(ison_bankwiring)))){
    expect_equal(nrow(back$ties), ties)
    expect_equal(back$info$directed, ison_bankwiring$info$directed)
  }
})

test_that("an undirected network keeps every layer undirected on a round trip", {
  # both layers of `ison_florentine` are undirected, and nothing about a round
  # trip should make either of them directed
  for(back in list(as_stocnet(as_igraph(ison_florentine)),
                   as_stocnet(as_network(ison_florentine)))){
    expect_equal(nrow(back$ties), nrow(ison_florentine$ties))
    expect_equal(back$info$directed, ison_florentine$info$directed)
  }
})

test_that("a node keeps its degree in an undirected layer across classes", {
  friends <- to_uniplex(ison_bankwiring, "friendship")
  # nothing directed is left, so the layer is undirected again
  expect_false(is_directed(friends))
  expect_true(isSymmetric(as_matrix(friends)))
  expect_equal(igraph::ecount(as_igraph(friends)), nrow(friends$ties))
  expect_equal(as_matrix(friends), as_matrix(to_uniplex(as_igraph(ison_bankwiring),
                                                        "friendship")))
})

test_that("a layer that is not fully reciprocated is not collapsed", {
  # a directed graph that claims `b` is undirected, but holds a one-way arc in
  # it: collapsing that layer would lose a tie, so `b` is directed after all
  net <- igraph::graph_from_data_frame(
    data.frame(from = c("A", "A", "B", "B"), to = c("B", "B", "A", "C"),
               layer = c("a", "b", "b", "b")),
    directed = TRUE)
  igraph::graph_attr(net, "layers") <- c("a", "b")
  igraph::graph_attr(net, "directed") <- c(a = TRUE, b = FALSE)
  back <- as_stocnet(net)
  expect_equal(nrow(back$ties), 4)
  expect_true(back$info$directed[["b"]])
})

test_that("a duplicated tie in an undirected layer is not doubled", {
  # `A-B` is recorded twice, once in each direction, though one row per dyad
  # is all an undirected layer needs
  net <- make_stocnet(
    info = list(layers = c("a", "b"), directed = c(a = TRUE, b = FALSE)),
    nodes = dplyr::tibble(label = LETTERS[1:3]),
    ties = dplyr::tibble(from = c(1L, 1L, 2L, 2L), to = c(2L, 2L, 1L, 3L),
                         layer = c("a", "b", "b", "b"))
  )
  # 1 arc in `a`, and 2 dyads in `b` reciprocated into 4 arcs
  expect_equal(igraph::ecount(as_igraph(net)), 5)
  expect_equal(nrow(as_stocnet(as_igraph(net))$ties), 3)
})

test_that("to_layer keeps the endpoints of each arc (#170)", {
  net <- make_stocnet(
    info = list(modes = c("states", "IGOs"),
                layers = c("trade", "membership"),
                directed = c(trade = TRUE, membership = FALSE)),
    nodes = tibble::tibble(label = c("a", "b", "x"),
                           mode = c("states", "states", "IGOs")),
    ties = tibble::tibble(from = c(1L, 2L, 1L), to = c(2L, 1L, 3L),
                          weight = c(5, 9, 1),
                          layer = c("trade", "trade", "membership"))
  )
  out <- to_layer(net, "trade")
  # `b -> a` used to be reindexed onto `a -> b`, which `as_matrix()` then
  # summed with the arc already there
  expect_equal(out$ties$from, c(1L, 2L))
  expect_equal(out$ties$to, c(2L, 1L))
  expect_equal(out$ties$weight, c(5, 9))
  mat <- as_matrix(out)
  expect_equal(mat["a", "b"], 5)
  expect_equal(mat["b", "a"], 9)
})

test_that("from_layers renumbers and names the ties each layer misses", {
  one <- make_stocnet(nodes = data.frame(label = c("a", "b", "c")),
                      ties = data.frame(from = "a", to = "b"),
                      info = list(directed = TRUE))
  two <- make_stocnet(nodes = data.frame(label = c("c", "b", "a")),
                      ties = data.frame(from = "c", to = "a", by = "b"),
                      info = list(directed = TRUE))
  # c -> b, as a reported it, is missing from the second network
  two$missings <- dplyr::tibble(from = 1L, to = 2L, by = 3L)
  merged <- from_layers(one = one, two = two)
  missing <- as_missinglist(merged)
  labels <- merged$nodes$label
  expect_equal(labels[missing$from], "c")
  expect_equal(labels[missing$to], "b")
  expect_equal(labels[missing$by], "a")
  expect_equal(missing$layer, "two")
})

test_that("from_layers keeps a node's nonresponse to the layer it missed", {
  answered <- make_stocnet(nodes = data.frame(label = c("a", "b", "c")),
                           ties = data.frame(from = c("a", "c"), to = c("b", "a")),
                           info = list(directed = TRUE))
  silent <- make_stocnet(nodes = data.frame(label = c("a", "b", "c"),
                                            na = c(FALSE, FALSE, TRUE)),
                         ties = data.frame(from = "a", to = "b"),
                         info = list(directed = TRUE))
  merged <- from_layers(first = answered, second = silent)
  missing <- as_missinglist(merged)
  expect_true(all(missing$layer == "second"))
  expect_true(all(merged$nodes$label[missing$from] == "c"))
})

# to_aggregated() -------------------------------------------------------------

test_that("to_flat is to_aggregated over layers", {
  expect_equal(as_matrix(to_flat(comb, "sum")),
               as_matrix(to_aggregated(comb, rule = "sum")))
  expect_equal(as_matrix(to_flat(ison_lawfirm, rule = "mean")),
               as_matrix(to_aggregated(ison_lawfirm, "layer", "mean")))
})

test_that("to_aggregated pools reports as each rule promises", {
  css <- as_stocnet(css_array(), attribute = "by")
  # A->B is reported by all four reporters, B->A and D->A by two, C->D by one
  expect_equal(as_matrix(to_aggregated(css, "by", "mean")),
               matrix(c(0, .5, 0, .5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, .25, 0),
                      4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4])))
  expect_equal(as_matrix(to_aggregated(css, "by", "sum"))["A", "B"], 4)
  expect_equal(as_matrix(to_aggregated(css, "by", "min"))["B", "A"], 0)
  expect_equal(as_matrix(to_aggregated(css, "by", "max"))["C", "D"], 1)
  out <- to_aggregated(css, "by", "mean")
  expect_false(is_cognitive(out))
  expect_null(out$ties$by)
  expect_equal(as_infolist(out)$transformations$aggregation,
               "reporters (mean)")
})

test_that("to_aggregated gives the locally aggregated structures", {
  css <- as_stocnet(css_array(), attribute = "by")
  mat <- function(...) as_matrix(to_aggregated(css, "by", ...))
  # the intersection keeps a tie that both of its ends report
  expect_equal(mat("min", "both")["B", "A"], 1)
  expect_equal(mat("min", "both")["D", "A"], 0)
  # the union keeps a tie that either of its ends reports
  expect_equal(mat("max", "both")["D", "A"], 1)
  expect_equal(mat("max", "both")["C", "D"], 1)
  # the sender's and the receiver's reports
  expect_equal(mat("max", "sender")["C", "D"], 1)
  expect_equal(mat("max", "receiver")["C", "D"], 0)
  expect_equal(mat("max", "receiver")["B", "A"], 1)
  expect_error(to_aggregated(to_undirected(css), "by", "max", "sender"),
               "both")
  expect_error(to_aggregated(css, "time", "max", "both"), "reporters")
})

test_that("to_aggregated leaves a silent reporter out of the pool", {
  reports <- css_array()
  reports[, , "C"] <- NA
  css <- as_stocnet(reports, attribute = "by")
  out <- as_matrix(to_aggregated(css, "by", "mean"))
  expect_equal(out["B", "A"], 2/3)
  expect_equal(out["C", "D"], 0)
  expect_false(anyNA(out[-3, -3]))
})

test_that("to_aggregated combines parallel ties only where over is NULL", {
  merged <- to_aggregated(ison_koenigsberg, over = NULL, rule = "sum")
  expect_equal(as.numeric(net_ties(merged)), 5)
  expect_false(any(tie_is_parallel(merged)))
  expect_equal(sort(merged$ties$weight), c(1, 1, 1, 2, 2))
  expect_equal(as_infolist(merged)$transformations$aggregation,
               "parallel ties (sum)")
  # a network without parallel ties has nothing to combine
  expect_equal(to_aggregated(merged, over = NULL), merged)
})

test_that("to_aggregated combines the moments of a panel", {
  out <- to_aggregated(ison_monks, "time", "max")
  expect_null(out$ties$time)
  expect_lte(as.numeric(net_ties(out)), as.numeric(net_ties(ison_monks)))
  expect_equal(as_infolist(out)$transformations$aggregation, "moments (max)")
})

test_that("to_aggregated returns the class it was given", {
  css <- as_stocnet(css_array(), attribute = "by")
  expect_s3_class(to_aggregated(as_tidygraph(css), "by"), "tbl_graph")
  expect_true(inherits(to_aggregated(as_igraph(css), "by"), "igraph"))
  expect_error(to_aggregated(css, "reporter"), "over")
})

# to_disaggregated() ----------------------------------------------------------

test_that("to_disaggregated reverses a summed aggregation of parallel ties", {
  merged <- to_aggregated(ison_koenigsberg, over = NULL, rule = "sum")
  split <- to_disaggregated(merged)
  expect_equal(as.numeric(net_ties(split)), 7)
  expect_false(is_weighted(split))
  expect_equal(to_aggregated(split, over = NULL, rule = "sum")$ties,
               merged$ties)
  expect_true(inherits(to_disaggregated(as_igraph(merged)), "igraph"))
  expect_error(to_disaggregated(mutate_ties(merged, weight = weight / 2)),
               "whole numbers")
  expect_equal(suppressMessages(to_disaggregated(as_matrix(merged))),
               as_matrix(merged))
})

test_that("to_disaggregated keeps the sign of a negative weight", {
  signed <- make_stocnet(nodes = data.frame(label = c("a", "b", "c")),
                         ties = data.frame(from = c("a", "b"), to = c("b", "c"),
                                           weight = c(-2, 3)))
  split <- to_disaggregated(signed)
  expect_equal(sort(split$ties$weight), c(-1, -1, 1, 1, 1))
  expect_equal(sort(to_aggregated(split, over = NULL, rule = "sum")$ties$weight),
               c(-2, 3))
})

test_that("from_layers keeps the designs that only some layers declare", {
  css <- as_stocnet(css_array(), attribute = "by")
  plain <- as_stocnet(add_node_attribute(create_ring(4), "name", LETTERS[1:4]))
  joined <- from_layers(reports = css, ring = plain)
  expect_equal(joined$info$observation, c(reports = "cognitive"))
  expect_true(is_cognitive(joined))
})

test_that("from_layers keeps the node attributes of unlabelled stocnets", {
  net <- as_stocnet(create_ring(4)) |>
    add_node_attribute("age", c(30, 40, 50, 60))
  joined <- from_layers(a = net, b = net)
  expect_equal(joined$nodes$age, c(30, 40, 50, 60))
})

test_that("to_aggregated keeps ties that no node reported as they are", {
  expect_true(is_cognitive(ison_hightech))
  for (rule in c("mean", "max")) {
    out <- to_aggregated(ison_hightech, over = "by", rule = rule)
    expect_equal(sort(out$info$layers), c("advice", "friends", "reports"))
    expect_equal(as_matrix(to_layer(out, "reports")),
                 as_matrix(to_layer(ison_hightech, "reports")))
  }
  own <- to_aggregated(ison_hightech, over = "by", reporters = "sender")
  expect_false(is_cognitive(own))
  expect_equal(as.numeric(net_ties(to_layer(own, "advice"))), 190)
  expect_equal(as.numeric(net_ties(to_layer(own, "friends"))), 102)
})
