# Behaviour of the to_*() transformation functions that the fixture and
# cross-class sweep in test-functional_to.R cannot assert: which nodes or
# ties each keeps, the arguments each takes, and what each records.


test_that("to_giant works",{
  fm <- to_uniplex(fict_marvel, layer = "relationship")
  expect_equal(c(net_nodes(fm)), 53)
  expect_equal(c(net_nodes(to_giant(fm))), 50)
  expect_equal(c(net_nodes(to_giant(as_igraph(fm)))), 50)
  # expect_equal(c(net_nodes(to_giant(as_matrix(fict_marvel)))), 50)
  # expect_equal(c(net_nodes(to_giant(as_network(fict_marvel)))), 50)
  expect_equal(c(net_nodes(to_giant(as_edgelist(fm)))), 50)
})

test_that("to_giant is a wrapper for the first component",{
  fm <- to_uniplex(fict_marvel, layer = "relationship")
  expect_identical(as_matrix(to_component(fm)), as_matrix(to_giant(fm)))
  expect_identical(as_matrix(to_component(fm, component = 1)),
                   as_matrix(to_giant(fm)))
})

test_that("to_component selects by size rank",{
  fm <- to_uniplex(fict_marvel, layer = "relationship")
  expect_equal(c(net_nodes(to_component(fm, 1))), 50)
  expect_equal(c(net_nodes(to_component(fm, 2))), 1)
  expect_error(to_component(fm, 99), "between 1 and 4")
  expect_error(to_component(fm, c(1, 2)), "between 1 and 4")
})

test_that("to_component selects by node name",{
  fm <- to_uniplex(fict_marvel, layer = "relationship")
  # Cable is an isolate, and so its own component
  expect_equal(c(net_nodes(to_component(fm, "Cable"))), 1)
  expect_true("Cable" %in% node_names(to_component(fm, "Cable")))
  expect_identical(as_matrix(to_component(fm, "Beast")),
                   as_matrix(to_giant(fm)))
  expect_error(to_component(fm, "Nobody"), "not the name of a node")
  expect_error(to_component(create_ring(8), "Nobody"), "labelled network")
})

test_that("to_component and to_giant name the sense of connection",{
  # only directed networks are qualified, since the notions coincide otherwise
  expect_equal(net_name(to_giant(fict_starwars)),
               "Giant weak component of Star Wars network data")
  expect_equal(net_name(to_giant(fict_starwars, connectivity = "strong")),
               "Giant strong component of Star Wars network data")
  expect_equal(net_name(to_component(fict_starwars, 2, "strong")),
               "Strong component 2 of Star Wars network data")
  expect_match(net_name(to_component(fict_starwars, "Anakin", "strong")),
               "^Strong component containing Anakin of ")
  expect_equal(net_name(to_giant(fict_greys)),
               "Giant component of Grey's Anatomy")
  expect_equal(net_name(to_component(fict_greys, 2)),
               "Component 2 of Grey's Anatomy")
})

test_that("to_component and to_giant respect connectivity",{
  # fict_starwars is weakly but not strongly connected
  expect_equal(c(net_nodes(to_giant(fict_starwars))), 110)
  expect_equal(c(net_nodes(to_giant(fict_starwars, connectivity = "strong"))), 46)
  expect_equal(c(net_nodes(to_component(fict_starwars, 1, "strong"))), 46)
})

test_that("to_wave is an alias of to_time",{
  expect_identical(as_matrix(to_wave(fict_potter, 3)),
                   as_matrix(to_time(fict_potter, 3)))
  expect_equal(c(net_nodes(to_wave(fict_potter, 3))),
               c(net_nodes(to_time(fict_potter, 3))))
})

test_that("to_time returns the network as it stood, however it records time", {
  # A panel that numbers its waves: the ties stamped with the wave asked for
  expect_equal(as.numeric(net_ties(to_time(ison_monks, 2))),
               sum(tie_attribute(ison_monks, "time") == 2) +
                 # the wave-1-only layers state something holding throughout,
                 # so they are carried into every wave
                 sum(tie_attribute(ison_monks, "time") == 1 &
                       tie_attribute(ison_monks, "layer") != "like"))
  expect_false("time" %in% net_tie_attributes(to_time(ison_monks, 2)))
  # A panel that dates its waves, in a stocnet: the ties observed then, and
  # not those observed by then
  w2 <- to_time(ison_tailorshop, 2)
  expect_s3_class(w2, "stocnet")
  expect_equal(as.numeric(net_ties(w2)), sum(ison_tailorshop$ties$time == 2))
  expect_false("time" %in% net_tie_attributes(w2))
  # A dynamic network that increments its ties: accumulated up to the moment,
  # which is what to_slices() returns for the same moment
  d <- sort(unique(tie_attribute(irps_nuclear, "time")))[20]
  expect_equal(as.numeric(net_ties(to_time(irps_nuclear, d))),
               as.numeric(net_ties(to_slices(irps_nuclear, slice = d))))
  # An interval network: the ties active then, on the half-open convention
  expect_equal(as.numeric(net_ties(to_time(irps_wwi, 1901))),
               sum(tie_attribute(irps_wwi, "begin") <= 1901 &
                     tie_attribute(irps_wwi, "end") > 1901))
  # A network that records no time is returned unchanged
  expect_equal(as_matrix(to_time(ison_karateka, 1)), as_matrix(ison_karateka))
})

test_that("to_time carries a time-invariant layer into every moment", {
  # 'primary' is declared cross-sectional, and is recorded at wave 1 only,
  # so it states something that holds at every wave
  out <- to_time(ison_classmates, 2)
  expect_equal(as.numeric(net_ties(out)),
               sum(ison_classmates$ties$time == 2) +
                 sum(ison_classmates$ties$layer == "primary"))
  expect_equal(layer_names(out), layer_names(ison_classmates))
  expect_setequal(unique(out$ties$layer), c("friends", "primary"))
})

test_that("to_time reverts a moment past the last, except for intervals", {
  expect_equal(as_matrix(suppressMessages(to_time(ison_monks, 99))),
               as_matrix(to_time(ison_monks, 3)))
  # irps_wwi is defined after its last change point, where no tie is active
  expect_equal(as.numeric(net_ties(to_time(irps_wwi, 3000))), 0)
})

test_that("to_time without a time points at to_times", {
  expect_error(to_time(irps_wwi), "to_times")
  expect_error(to_time(ison_monks), "to_times")
})

test_that("matrix projected correctly by rows",{
  expect_false(is_weighted(ison_southern_women))
  expect_true(is_weighted(to_mode1(ison_southern_women)))
  expect_true(is_weighted(to_mode1(as_igraph(ison_southern_women))))
  expect_true(is_weighted(to_mode1(as_matrix(ison_southern_women))))
  expect_true(is_weighted(to_mode1(as_network(ison_southern_women))))
  expect_true(is_weighted(to_mode1(as_edgelist(ison_southern_women))))
  expect_true(all(node_names(to_mode1(ison_southern_women)) %in% node_names(ison_southern_women)))
  expect_true(length(node_names(to_mode1(ison_southern_women))) != length(node_names(ison_southern_women)))
  expect_values(length(node_names(to_mode1(ison_southern_women))), length(rownames(as_matrix(ison_southern_women))))
  expect_values(net_nodes(to_mode1(ison_southern_women, "count")), net_nodes(to_mode1(ison_southern_women, "jaccard")))
  expect_true(is_weighted(to_mode1(ison_southern_women, "pearson")))
  expect_false(tie_weights(to_mode1(ison_southern_women, "rand"))[3] == tie_weights(to_mode1(ison_southern_women, "count"))[3])
})

test_that("matrix projected correctly by columns",{
  expect_false(is_weighted(ison_southern_women))
  expect_true(is_weighted(to_mode2(ison_southern_women)))
  expect_true(is_weighted(to_mode2(as_igraph(ison_southern_women))))
  expect_true(is_weighted(to_mode2(as_matrix(ison_southern_women))))
  expect_true(is_weighted(to_mode2(as_network(ison_southern_women))))
  expect_true(is_weighted(to_mode2(as_edgelist(ison_southern_women))))
  expect_true(all(node_names(to_mode2(ison_southern_women)) %in% node_names(ison_southern_women)))
  expect_true(length(node_names(to_mode2(ison_southern_women))) != length(node_names(ison_southern_women)))
  expect_values(length(node_names(to_mode2(ison_southern_women))), length(colnames(as_matrix(ison_southern_women))))
  expect_values(net_nodes(to_mode2(ison_southern_women, "count")), net_nodes(to_mode2(ison_southern_women, "jaccard")))
  expect_true(is_weighted(to_mode2(ison_southern_women, "pearson")))
  expect_false(tie_weights(to_mode2(ison_southern_women, "rand"))[1] == tie_weights(to_mode2(ison_southern_women, "count"))[1])
})

test_that("mode selected by index projects as the numbered function does",{
  expect_equal(as_matrix(to_mode(ison_southern_women, 1)),
               as_matrix(to_mode1(ison_southern_women)))
  expect_equal(as_matrix(to_mode(ison_southern_women, 2)),
               as_matrix(to_mode2(ison_southern_women)))
  # mode 1 is the default, as for the rows of a matrix
  expect_equal(as_matrix(to_mode(ison_southern_women)),
               as_matrix(to_mode1(ison_southern_women)))
  # a one-mode network is returned unchanged, as by to_mode1()
  expect_equal(as_matrix(to_mode(create_ring(5))),
               as_matrix(create_ring(5)))
})

test_that("mode selected by name projects as the numbered function does",{
  expect_equal(mode_names(ison_southern_women), c("women", "social events"))
  expect_equal(as_matrix(to_mode(ison_southern_women, "women")),
               as_matrix(to_mode1(ison_southern_women)))
  expect_equal(as_matrix(to_mode(ison_southern_women, "social events")),
               as_matrix(to_mode2(ison_southern_women)))
  # just one word of the name is enough, in any case and either number
  expect_equal(as_matrix(to_mode(ison_southern_women, "events")),
               as_matrix(to_mode2(ison_southern_women)))
  expect_equal(as_matrix(to_mode(ison_southern_women, "event")),
               as_matrix(to_mode2(ison_southern_women)))
  expect_equal(as_matrix(to_mode(ison_southern_women, "Social Events")),
               as_matrix(to_mode2(ison_southern_women)))
})

test_that("to_mode passes the similarity through",{
  expect_equal(as_matrix(to_mode(ison_southern_women, "events", "jaccard")),
               as_matrix(to_mode2(ison_southern_women, "jaccard")))
  expect_equal(as_matrix(to_mode(ison_southern_women, 1, "pearson")),
               as_matrix(to_mode1(ison_southern_women, "pearson")))
})

test_that("to_mode reports an unusable mode",{
  # cli wraps these messages, so match a phrase short enough to survive it
  expect_error(to_mode(ison_southern_women, 3), "must be 1 or 2")
  expect_error(to_mode(ison_southern_women, c(1,2)), "must be 1 or 2")
  expect_error(to_mode(ison_southern_women, "cats"), "must be an index")
  # the modes of this network are not named, so only an index will do
  expect_error(to_mode(create_ring(c(5,3)), "events"), "not named")
})

test_that("to_mode reports a name matching both modes",{
  two <- add_info(ison_southern_women, nodes = c("work events",
                                                 "social events"))
  expect_error(to_mode(two, "events"), "more than one mode")
  # each mode is still reachable by the word that tells them apart
  expect_equal(as_matrix(to_mode(two, "work")),
               as_matrix(to_mode1(ison_southern_women)))
  expect_equal(as_matrix(to_mode(two, "social")),
               as_matrix(to_mode2(ison_southern_women)))
})

test_that("to_mode refuses a network of three or more modes",{
  # the only three-mode object the package can hold, built as in test-mark_is.R
  three <- as_stocnet(fict_marvel)
  three$nodes$mode[1:5] <- "third"
  expect_equal(net_modes(three), 3)
  expect_error(to_mode(three, 1), "3 modes")
  # two-mode and one-mode networks are unaffected by that check
  expect_equal(net_modes(ison_southern_women), 2)
  expect_equal(net_modes(create_ring(5)), 1)
})

test_that("to matching works", {
  sw <- as_edgelist(to_matching(ison_southern_women))
  expect_values(net_nodes(to_matching(ison_southern_women)),
               net_nodes(ison_southern_women))
  expect_equal(nrow(sw), nrow(dplyr::distinct(sw)))
})

test_that("to_subgraph works", {
  expect_equal(c(net_nodes(to_subgraph(ison_lawfirm, office == "Boston"))), 48)
})

test_that("to anti works", {
  expect_length(to_anti(ison_southern_women), 32)
  expect_length(to_anti(as_igraph(ison_southern_women)), 32)
})

test_that("to, and from, waves work", {
  orig <- ison_adolescents |>
    mutate_ties(wave = sample(1995:1998, 10, replace = TRUE))
  waves <- to_waves(orig, attribute = "wave")
  from_wave <- from_waves(waves)
  expect_length(waves, length(unique(tie_attribute(orig, "wave"))))
  expect_length(from_wave, length(as_igraph(orig)))
})

test_that("to and from slices work", {
  orig <- ison_adolescents |>
    mutate_ties(time = 1:10, increment = 1) |>
    add_ties(c(1,2), list(time = 3, increment = -1))
  slice <- to_slices(orig, slice = 7)
  expect_length(slice, length(orig))
  #expect_false(is.null(tie_attribute(slice, "time")))
  ##should attribute names change?
  slices <- ison_adolescents |>
    mutate_ties(time = 1:10, increment = 1) |>
    to_slices(slice = c(5,8))
  expect_length(slices, 2)
})


test_that("to_blockmodel summarises the blocks of a two-mode network", {
  sw <- as_matrix(ison_southern_women)
  out <- to_blockmodel(sw, c(rep(1, 9), rep(2, 9), rep(1, 7), rep(2, 7)))
  # one row per block of the first mode, one column per block of the second
  expect_equal(dim(out), c(2L, 2L))
  expect_equal(unname(out),
               matrix(c(mean(sw[1:9, 1:7]), mean(sw[10:18, 1:7]),
                        mean(sw[1:9, 8:14]), mean(sw[10:18, 8:14])),
                      nrow = 2, ncol = 2))
})

test_that("to_blockmodel handles two-mode memberships that are not 1...k", {
  # regression test: the block matrix used to be dimensioned by the group
  # labels themselves rather than by how many groups there were
  sw <- as_matrix(ison_southern_women)
  contiguous <- to_blockmodel(sw, c(rep(1, 9), rep(2, 9), rep(1, 7), rep(2, 7)))
  expect_equal(to_blockmodel(sw, c(rep(3, 9), rep(7, 9), rep(5, 7), rep(9, 7))),
               contiguous)
  expect_equal(to_blockmodel(sw, c(rep("a", 9), rep("b", 9),
                               rep("x", 7), rep("y", 7))),
               contiguous)
  # the two modes are partitioned separately, so labels may be shared
  expect_equal(dim(to_blockmodel(sw, c(rep(2, 9), rep(4, 9), rep(4, 14)))),
               c(2L, 1L))
})

test_that("to_blockmodel expects one membership vector across both modes", {
  sw <- as_matrix(ison_southern_women)
  expect_error(to_blockmodel(sw, c(rep(1, 9), rep(2, 9))), "length 32")
})

test_that("to_layers splits a multiplex network into its layers", {
  net <- as_tidygraph(create_filled(5)) |>
    mutate_ties(type = rep(c("friend", "enemy"), 5))
  out <- to_layers(net)
  expect_type(out, "list")
  expect_length(out, net_layers(net))
  expect_named(out, c("friend", "enemy"))
  # Each layer is what to_uniplex() would return for that tie type,
  # so the singular and plural forms cannot drift apart.
  expect_equal(as_matrix(out$friend), as_matrix(to_uniplex(net, "friend")))
  expect_false(any(vapply(out, is_multiplex, logical(1))))
})

test_that("to_layers returns one layer for an already uniplex network", {
  out <- to_layers(ison_adolescents)
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(as_matrix(out[[1]]), as_matrix(ison_adolescents))
})

test_that("to_layer is an alias of to_uniplex", {
  net <- as_tidygraph(create_filled(5)) |>
    mutate_ties(type = rep(c("friend", "enemy"), 5))
  expect_equal(as_matrix(to_layer(net, "friend")),
               as_matrix(to_uniplex(net, "friend")))
})

test_that("renamed to_*() functions are deprecated but still forward", {
  memb <- c(1, 1, 2, 2, 2, 3, 3, 3)
  expect_warning(to_ties(ison_adolescents), "deprecated")
  expect_warning(to_no_isolates(ison_adolescents), "deprecated")
  expect_warning(to_no_missing(ison_adolescents), "deprecated")
  expect_warning(to_blocks(as_matrix(ison_adolescents), memb), "deprecated")
  expect_equal(as_matrix(suppressWarnings(to_ties(ison_adolescents))),
               as_matrix(to_linegraph(ison_adolescents)))
  expect_equal(as_matrix(suppressWarnings(to_no_isolates(ison_adolescents))),
               as_matrix(delete_isolates(ison_adolescents)))
  expect_equal(as_matrix(suppressWarnings(to_no_missing(ison_adolescents))),
               as_matrix(delete_incomplete(ison_adolescents)))
  expect_equal(suppressWarnings(to_blocks(as_matrix(ison_adolescents), memb)),
               to_blockmodel(as_matrix(ison_adolescents), memb))
})


# Projection measures ####
# Reference values are xUCINET's, computed with xTwoModeToOneMode() on
# as_matrix(ison_southern_women), and hard-coded here so that the comparison
# does not depend on xUCINET being installed.

sw_mat <- as_matrix(ison_southern_women)

test_that("projection measures reproduce xUCINET", {
  expect_equal(to_mode1(sw_mat, "count")[1, 2], 6)
  expect_equal(to_mode1(sw_mat, "match")[1, 2], 11)
  expect_equal(to_mode1(sw_mat, "crossmin")[1, 2], 6)
  expect_equal(to_mode1(sw_mat, "maxcrossmin")[1, 2], 1)
  expect_equal(to_mode1(sw_mat, "jaccard")[1, 2], 0.6666667, tolerance = 1e-6)
  expect_equal(to_mode1(sw_mat, "rand")[1, 2], 0.7857143, tolerance = 1e-6)
  expect_equal(to_mode1(sw_mat, "overlap")[1, 2], 0.8571429, tolerance = 1e-6)
  expect_equal(to_mode1(sw_mat, "pearson")[1, 2], 0.5773503, tolerance = 1e-6)
  expect_equal(to_mode1(sw_mat, "covariance")[1, 2], 0.1538462, tolerance = 1e-6)
  expect_equal(to_mode1(sw_mat, "ochiai")[1, 2], 0.8017837, tolerance = 1e-6)
  expect_equal(to_mode1(sw_mat, "yule")[1, 2], 0.875)
  expect_equal(to_mode1(sw_mat, "bonacich")[1, 2], 0.7947869, tolerance = 1e-6)
})

test_that("bonacich's closed form matches the pairwise formulation", {
  # xUCINET computes (X - sqrt(XY))/(X - Y) for X = ad and Y = bc with four
  # combn() sweeps; sqrt(ad)/(sqrt(ad) + sqrt(bc)) is algebraically the same,
  # including the X == Y case that xUCINET sets to 0.5 by hand
  a <- sw_mat %*% t(sw_mat)
  b <- sw_mat %*% (1 - t(sw_mat))
  c <- (1 - sw_mat) %*% t(sw_mat)
  d <- ncol(sw_mat) - a - b - c
  X <- a * d; Y <- b * c
  pairwise <- (X - sqrt(X * Y))/(X - Y)
  pairwise[X == Y] <- 0.5
  closed <- to_mode1(sw_mat, "bonacich")
  expect_equal(closed[upper.tri(closed)], pairwise[upper.tri(pairwise)])
})

test_that("sqdiff is an inverted distance", {
  # documented as recoverable via 1/x - 1, so that larger means more alike
  sq <- to_mode1(sw_mat, "sqdiff")
  raw <- as.matrix(stats::dist(sw_mat))^2
  expect_equal((1/sq - 1)[upper.tri(sq)], raw[upper.tri(raw)])
  expect_true(all(sq[upper.tri(sq)] > 0 & sq[upper.tri(sq)] <= 1))
})

test_that("measures within a family rank dyads identically", {
  # these identities are what the documentation groups the measures by, so
  # they are asserted rather than only described
  rk <- function(m) rank(m[upper.tri(m)])
  expect_equal(rk(to_mode1(sw_mat, "jaccard")), rk(to_mode1(sw_mat, "czekanowski")))
  expect_equal(rk(to_mode1(sw_mat, "jaccard")), rk(to_mode1(sw_mat, "sokalsneath")))
  expect_equal(rk(to_mode1(sw_mat, "rand")), rk(to_mode1(sw_mat, "hamann")))
  expect_equal(rk(to_mode1(sw_mat, "rand")), rk(to_mode1(sw_mat, "rogerstanimoto")))
  expect_equal(rk(to_mode1(sw_mat, "yule")), rk(to_mode1(sw_mat, "bonacich")))
})

test_that("hamann is the simple matching coefficient rescaled", {
  # the branch was named "gowerlegendre" while computing Hamann's coefficient;
  # Gower and Legendre's S is (a+d)/(a+0.5(b+c)+d), which this is not
  rand <- to_mode1(sw_mat, "rand")
  hamann <- to_mode1(sw_mat, "hamann")
  expect_equal(hamann[upper.tri(hamann)], 2 * rand[upper.tri(rand)] - 1)
})

test_that("valued measures generalise their binary counterparts", {
  # on binary data the minimum of two ties is their product
  expect_equal(to_mode1(sw_mat, "crossmin"), to_mode1(sw_mat, "count"))
  # and matching is the simple matching coefficient before it is divided
  # through by the size of the other mode
  match <- to_mode1(sw_mat, "match")/ncol(sw_mat)
  rand <- to_mode1(sw_mat, "rand")
  expect_equal(match[upper.tri(match)], rand[upper.tri(rand)])
})

test_that("binary-only measures dichotomise valued networks", {
  set.seed(1234)
  valued <- sw_mat
  valued[valued == 1] <- sample(1:3, sum(valued == 1), replace = TRUE)
  expect_warning(to_mode1(valued, "jaccard"), "binary")
  # dichotomising is what the warning says it does
  expect_equal(suppressWarnings(to_mode1(valued, "jaccard")),
               to_mode1((valued > 0) * 1, "jaccard"))
  # whereas a measure defined for valued data uses the values
  expect_silent(to_mode1(valued, "crossmin"))
  expect_false(isTRUE(all.equal(to_mode1(valued, "crossmin"),
                                to_mode1(sw_mat, "crossmin"))))
})

test_that("every projection measure works across classes", {
  # a `to_*()` function returns the class it was given, so the fixture's own
  # class is what each projection of it should come back as
  for (s in c("match", "overlap", "crossmin", "bonacich", "covariance")) {
    expect_s3_class(to_mode1(ison_southern_women, s), class(ison_southern_women)[1])
    expect_true(is.matrix(to_mode1(sw_mat, s)))
    expect_s3_class(to_mode2(ison_southern_women, s), class(ison_southern_women)[1])
    expect_s3_class(to_mode1(as_tidygraph(ison_southern_women), s), "tbl_graph")
    expect_s3_class(to_mode2(as_tidygraph(ison_southern_women), s), "tbl_graph")
  }
})

test_that("projections record which measure was used", {
  # GRAND item 4.3, recorded under the "projection" name of the transformations
  expect_equal(as_infolist(to_mode1(ison_southern_women,
                                            "jaccard"))$transformations$projection,
               "mode 1 (jaccard)")
  expect_equal(as_infolist(to_mode2(ison_southern_women,
                                            "bonacich"))$transformations$projection,
               "mode 2 (bonacich)")
  # a transformation that was not applied leaves no name behind
  expect_false("symmetrisation" %in%
                 names(as_infolist(to_mode1(ison_southern_women))$transformations))
})

# What exclusion records ####

test_that("the scoping functions record what they left out", {
  # GRAND item 4.4 asks for the criteria and the number of nodes or ties
  # excluded, recorded under the "exclusion" name of the transformations
  expect_equal(as_infolist(to_component(fict_greys, 2))$transformations$exclusion,
               "not in component 2 (47 nodes excluded)")
  expect_equal(as_infolist(to_subgraph(fict_greys,
                                               sex == "F"))$transformations$exclusion,
               "not sex == \"F\" (23 nodes excluded)")
  expect_match(as_infolist(to_ego(fict_greys,
                                          "Alex Karev"))$transformations$exclusion,
               "^outside the ego network of Alex Karev \\([0-9]+ nodes excluded\\)$")
  expect_equal(as_infolist(delete_isolates(delete_ties(fict_greys,
                                                               1:5)))$transformations$exclusion,
               "isolates (4 nodes excluded)")
  incomplete <- ison_adolescents |> mutate_nodes(age = c(NA, NA, 13:18))
  expect_equal(as_infolist(delete_incomplete(incomplete))$transformations$exclusion,
               "incomplete node data (2 nodes excluded)")
})

test_that("to_giant records once, not twice", {
  # it delegates to to_component(), whose entry already names component 1,
  # and the element accumulates rather than replaces
  out <- as_infolist(to_giant(fict_greys))$transformations$exclusion
  expect_length(out, 1)
  expect_equal(out, "not in component 1 (13 nodes excluded)")
})

test_that("the tie-dropping functions record what they left out", {
  expect_equal(as_infolist(to_acyclic(ison_networkers))$transformations$exclusion,
               "feedback arcs (180 ties excluded)")
  expect_match(as_infolist(to_unsigned(fict_marvel))$transformations$exclusion,
               "^negative ties \\([0-9]+ ties excluded\\)$")
  expect_match(as_infolist(to_unsigned(fict_marvel,
                                               "negative"))$transformations$exclusion,
               "^positive ties \\([0-9]+ ties excluded\\)$")
})

test_that("nothing is recorded where nothing was excluded", {
  # a function with nothing to drop did not transform the network
  expect_length(as_infolist(delete_isolates(ison_adolescents))$transformations, 0)
  expect_length(as_infolist(to_simplex(ison_adolescents))$transformations, 0)
  # a matrix or an edgelist has nowhere to hold information about itself
  expect_true(is.matrix(to_component(as_matrix(fict_greys), 2)))
  expect_true(is.data.frame(to_component(as_edgelist(fict_greys), 2)))
})

test_that("the transformations accumulate in the order applied", {
  # the added node is tied to nothing, so there is an isolate to exclude
  out <- as_infolist(delete_isolates(to_unweighted(
    to_undirected(add_nodes(ison_networkers, 1)))))$transformations
  expect_equal(names(out),
               c("symmetrisation", "dichotomisation", "exclusion"))
})

test_that("to_blockmodel records its aggregation and keeps the network's info", {
  # GRAND item 4.5, recorded under the "aggregation" name
  membs <- rep(1:3, length.out = net_nodes(fict_greys))
  out <- to_blockmodel(fict_greys, membs)
  expect_equal(as_infolist(out)$transformations$aggregation,
               "nodes into 3 blocks (mean)")
  expect_equal(as_infolist(to_blockmodel(fict_greys, membs,
                                                 median))$transformations$aggregation,
               "nodes into 3 blocks (median)")
  # the reduced graph is built from a matrix, so the information the network
  # held about itself has to be carried over rather than lost
  expect_equal(net_name(out), net_name(fict_greys))
  # and an earlier transformation is kept beside the aggregation
  both <- as_infolist(to_blockmodel(to_giant(fict_greys),
                                            rep(1:3, length.out = 40)))$transformations
  expect_equal(names(both), c("exclusion", "aggregation"))
})

test_that("the stocnet methods record without a round trip", {
  # stocnet$info is where this metadata belongs, so these functions record on
  # the class itself rather than on a coerced copy
  g <- as_stocnet(fict_greys)
  for (out in list(to_component(g, 2), to_subgraph(g, sex == "F"),
                   to_ego(g, "Alex Karev"), to_giant(g),
                   to_unweighted(as_stocnet(ison_networkers)),
                   to_acyclic(as_stocnet(ison_networkers)),
                   to_unsigned(as_stocnet(fict_marvel)),
                   delete_isolates(as_stocnet(delete_ties(fict_greys, 1:5))),
                   delete_incomplete(as_stocnet(ison_adolescents |>
                                       mutate_nodes(age = c(NA, NA, 13:18)))))) {
    expect_s3_class(out, "stocnet")
    expect_length(as_infolist(out)$transformations, 1)
  }
})

test_that("the stocnet methods return what the other classes return", {
  for (f in list(to_giant, to_simplex, to_acyclic, to_unweighted)) {
    expect_equal(c(net_nodes(f(ison_networkers))),
                 c(net_nodes(f(as_stocnet(ison_networkers)))))
    expect_equal(c(net_ties(f(ison_networkers))),
                 c(net_ties(f(as_stocnet(ison_networkers)))))
  }
  expect_equal(c(net_ties(to_unsigned(fict_marvel))),
               c(net_ties(to_unsigned(as_stocnet(fict_marvel)))))
})

test_that("to_uniplex keeps the information of the layer it retains", {
  # to_uniplex() and a node drop prune the per-layer information by the same
  # rule, so both sides of that rule are tested.
  out <- to_uniplex(ison_classmates, layer = "friends")
  expect_equal(net_layers(out), 1)
  expect_equal(layer_names(out), "friends")
  info <- as_infolist(out)
  expect_equal(info$directed, c(friends = TRUE))
  expect_false("primary" %in% info$focal)
  expect_no_error(validate_stocnet(as_stocnet(out)))
})

# A small matrix whose normalised values can be read off by hand. It is
# symmetric, so the row and column totals agree: 3, 1, 2, and then 0 for the
# fourth node, an isolate, which is what a zero denominator looks like.
norm_mat <- matrix(c(0, 1, 2, 0,
                     1, 0, 0, 0,
                     2, 0, 0, 0,
                     0, 0, 0, 0), 4, 4, byrow = TRUE)

test_that("to_normalised rescales by each rule", {
  expect_equal(to_normalised(norm_mat, rule = "sum", across = "rows")[1, ],
               c(0, 1/3, 2/3, 0))
  expect_equal(to_normalised(norm_mat, rule = "max", across = "rows")[1, ],
               c(0, 0.5, 1, 0))
  # the mean counts every dyad and not just those tied, so the divisor is 3/4
  expect_equal(to_normalised(norm_mat, rule = "mean", across = "rows")[1, ],
               c(0, 4/3, 8/3, 0))
  expect_equal(to_normalized, to_normalised)
})

test_that("to_normalised across both keeps a symmetric network symmetric", {
  expect_true(isSymmetric(to_normalised(norm_mat, rule = "sum")))
  expect_false(isSymmetric(to_normalised(norm_mat, rule = "sum",
                                         across = "rows")))
  # each value is divided by the square root of the two totals multiplied
  expect_equal(to_normalised(norm_mat, rule = "sum")[1, 2], 1/sqrt(3 * 1))
  cols <- to_normalised(norm_mat, rule = "sum", across = "columns")
  expect_equal(colSums(cols)[1:3], c(1, 1, 1), ignore_attr = TRUE)
  expect_equal(cols[1, ], c(0, 1, 1, 0))
})

test_that("to_normalised leaves a node with nothing to scale against", {
  out <- to_normalised(norm_mat, rule = "sum", across = "rows")
  expect_equal(out[4, ], c(0, 0, 0, 0))
  expect_false(anyNA(out))
  expect_true(all(is.finite(out)))
  expect_true(all(is.finite(to_normalised(norm_mat, rule = "max",
                                          across = "rows"))))
  expect_warning(to_normalised(norm_mat, rule = "sum", across = "rows"),
                 "no value to be scaled against")
})

test_that("to_normalised returns a directed network where it must", {
  # Rescaling rows makes what i sends j differ from what j sends i, which an
  # undirected network cannot hold, so each tie is split into two.
  rows <- to_normalised(ison_adolescents, rule = "sum", across = "rows")
  expect_true(is_directed(rows))
  expect_equal(c(net_ties(rows)), c(net_ties(ison_adolescents)) * 2)
  expect_equal(unname(rowSums(as_matrix(rows))), rep(1, 8))
  # "both" is symmetric, so it leaves the network as it found it
  both <- to_normalised(ison_adolescents, rule = "sum")
  expect_false(is_directed(both))
  expect_equal(c(net_ties(both)), c(net_ties(ison_adolescents)))
})

test_that("to_normalised treats the two modes of a two-mode network as the margins", {
  out <- to_normalised(ison_southern_women, rule = "sum", across = "rows")
  expect_true(is_twomode(out))
  expect_false(is_directed(out))
  expect_equal(unname(rowSums(as_matrix(out))), rep(1, 18))
  cols <- to_normalised(ison_southern_women, rule = "sum", across = "columns")
  expect_equal(unname(colSums(as_matrix(cols))), rep(1, 14))
})

test_that("to_normalised records the transformation", {
  out <- to_normalised(ison_networkers, rule = "sum", across = "rows")
  expect_equal(as_infolist(out)$transformations$normalisation,
               "sum across rows")
  expect_match(describe_transformations(out), "normalisation")
  # a matrix has nowhere to record it, and is returned as a matrix
  expect_true(is.matrix(to_normalised(norm_mat)))
})

test_that("to_normalised keeps what the stocnet class holds", {
  net <- as_stocnet(ison_networkers)
  out <- to_normalised(net, rule = "sum", across = "rows")
  expect_s3_class(out, "stocnet")
  expect_equal(nrow(out$nodes), nrow(net$nodes))
  expect_equal(net_name(out), net_name(net))
  expect_equal(unname(rowSums(as_matrix(out))), rep(1, nrow(net$nodes)))
  expect_no_error(validate_stocnet(out))
})

# Backbone ####

# The counts below were cross-validated against the {backbone} package
# (v3.0.4) by comparing `tie_is_backbone()` tie for tie with the p-values that
# `backbone:::.disparity()`, `.lans()` and `.mlf()` return, retained at
# `p < alpha` as `backbone:::.retain()` does. All three agreed exactly on
# `ison_networkers` both as it is and symmetrised. The values are pinned here
# rather than recomputed, so that {backbone} is not needed to run the tests.
test_that("the statistical filters match their published definitions", {
  net <- ison_networkers
  expect_equal(sum(tie_is_backbone(net, filter = "disparity")), 47)
  expect_equal(sum(tie_is_backbone(net, filter = "mlf")), 159)
  und <- to_undirected(net)
  expect_equal(sum(tie_is_backbone(und, filter = "disparity")), 35)
  expect_equal(sum(tie_is_backbone(und, filter = "lans")), 37)
  expect_equal(sum(tie_is_backbone(und, filter = "mlf")), 92)
})

test_that("to_backbone deletes the ties tie_is_backbone does not mark", {
  net <- ison_networkers
  expect_equal(c(net_ties(to_backbone(net))),
               sum(tie_is_backbone(net)))
  # every class reaches the stocnet method and comes back as it went in
  expect_s3_class(to_backbone(as_igraph(net)), "igraph")
  expect_s3_class(to_backbone(as_tidygraph(net)), "tbl_graph")
  expect_s3_class(to_backbone(as_stocnet(net)), "stocnet")
  expect_no_error(validate_stocnet(to_backbone(as_stocnet(net))))
})

test_that("requiring both endpoints is never less severe than either", {
  net <- ison_networkers
  for (f in c("disparity", "lans")) {
    both <- sum(tie_is_backbone(net, filter = f, endpoints = "both"))
    either <- sum(tie_is_backbone(net, filter = f, endpoints = "either"))
    expect_lte(both, either)
  }
})

test_that("the filter and threshold default to what the network allows", {
  # a weighted network is filtered on its weights, an unweighted one cannot be
  expect_equal(sum(tie_is_backbone(ison_networkers)),
               sum(tie_is_backbone(ison_networkers, filter = "lans")))
  expect_equal(sum(tie_is_backbone(ison_adolescents)),
               sum(tie_is_backbone(ison_adolescents, filter = "simmelian")))
  # each filter's own default threshold is the one used where none is given
  expect_equal(sum(tie_is_backbone(ison_networkers)),
               sum(tie_is_backbone(ison_networkers, threshold = 0.05)))
  expect_equal(sum(tie_is_backbone(ison_adolescents)),
               sum(tie_is_backbone(ison_adolescents, threshold = 0.5)))
  # a higher threshold retains at least as many ties as a lower one
  expect_gte(sum(tie_is_backbone(ison_networkers, threshold = 0.2)),
             sum(tie_is_backbone(ison_networkers, threshold = 0.05)))
})

test_that("to_backbone refuses what it cannot filter", {
  expect_error(to_backbone(fict_marvel), "signed network cannot be")
  expect_error(to_backbone(ison_adolescents, filter = "disparity"),
               "needs a weighted network")
  expect_error(to_backbone(ison_networkers, filter = "sparsest"), "arg")
  expect_error(to_backbone(ison_networkers, threshold = c(0.1, 0.2)),
               "single number")
  # the marginal likelihood filter reads weights as counts of events
  frac <- mutate_ties(ison_adolescents, weight = c(1.5, 2:10))
  expect_error(to_backbone(frac, filter = "mlf"), "whole-number weights")
})

test_that("a tie whose weight was never recorded is marked as retained", {
  # there is no value to test against the null model, so the tie is kept
  # rather than deleted on the strength of a weight it does not have
  net <- mutate_ties(ison_adolescents, weight = c(1, 2, 3, NA, 5:10))
  expect_true(tie_is_backbone(net)[4])
})

test_that("to_backbone records the exclusion and names the result", {
  # GRAND item 4.4, recorded under the "exclusion" name with the filter and
  # the threshold that together decided which ties went
  out <- to_backbone(ison_networkers, filter = "lans", threshold = 0.2)
  expect_equal(as_infolist(out)$transformations$exclusion,
               paste0("not in the lans backbone at threshold 0.2 (",
                      c(net_ties(ison_networkers)) - c(net_ties(out)),
                      " ties excluded)"))
  expect_match(net_name(out), "^lans backbone of")
  # a matrix has nowhere to record it, and is returned as a matrix
  expect_true(is.matrix(to_backbone(as_matrix(ison_networkers))))
})

test_that("the simmelian filter reads structure rather than weights", {
  # it is the only filter an unweighted network can use, and it returns the
  # same marks whether or not weights are present to ignore
  bare <- sum(tie_is_backbone(ison_adolescents, filter = "simmelian"))
  weighted <- mutate_ties(ison_adolescents, weight = 1:10)
  expect_equal(sum(tie_is_backbone(weighted, filter = "simmelian")), bare)
  expect_lte(bare, c(net_ties(ison_adolescents)))
})

test_that("the default filter retains something where disparity cannot", {
  # The disparity filter's null model expects heavy-tailed weights. Where
  # weights are more even, every tie takes about 1/k of its node's strength,
  # the p-value approaches 1/e, and nothing is retained at all. This was
  # confirmed against {backbone} (v3.0.4), which returns an empty backbone
  # here too, so it is the filter's behaviour and not this implementation's.
  expect_equal(sum(tie_is_backbone(ison_karateka, filter = "disparity")), 0)
  expect_gt(sum(tie_is_backbone(ison_karateka)), 0)
  # LANS scores each node's heaviest tie at zero, so no node is ever stranded
  expect_equal(c(net_nodes(delete_isolates(to_backbone(ison_karateka)))),
               c(net_nodes(ison_karateka)))
})

test_that("to_backbone reports what would otherwise pass unnoticed", {
  # `snet_warn()` is silent at the default verbosity, so it is raised here
  # every filter builds its null model from the ties as the network holds
  # them, so a tie restated at each wave is tested once per wave
  repeated <- to_uniplex(ison_monks, layer = "like")
  expect_true(any(grepl("more than once",
                        capture_warnings(tie_is_backbone(repeated)))))
  expect_false(any(grepl("more than once",
                         capture_warnings(tie_is_backbone(ison_networkers)))))
  # a threshold that deletes every tie is more likely a mismatch than a finding
  expect_true(any(grepl("retains no tie",
    capture_warnings(to_backbone(ison_karateka, filter = "disparity")))))
})

test_that("to_mode resolves a mode by name on a stocnet", {
  sw <- as_stocnet(ison_southern_women)
  two <- add_info(sw, nodes = c("work events", "social events"))
  # the word that tells the modes apart reaches one of them
  expect_equal(as_matrix(to_mode(two, "work")), as_matrix(to_mode1(sw)))
  expect_equal(as_matrix(to_mode(two, "social")), as_matrix(to_mode2(sw)))
  # and one that does not is an error, as it is for an igraph
  expect_error(to_mode(two, "events"), "more than one mode")
})
