to_funs <- funs_objs[grepl("to_", names(funs_objs))]
# Deprecated shims just warn and forward to their replacement, so they carry no
# methods of their own. Drop them by body, as alive_functions() does for the
# functional sweep, so that deprecating a to_*() function does not break this.
to_funs <- to_funs[!vapply(to_funs, function(f)
  is.function(f) && grepl("Deprecated|Defunct|fn_moved",
                          paste(deparse(body(f)), collapse = " ")),
  logical(1))]
# to_wave() is an alias of to_time(), to_layer() of to_uniplex(), and
# to_giant() is a plain wrapper for to_component(), so none of them has
# methods of its own to sweep here.
to_funs <- to_funs[!grepl("^na_|s$|^to_named$|^to_unnamed$|^to_wave$|^to_giant$|^to_layer$", names(to_funs))]
fun_names <- names(to_funs)
fun_names <- fun_names[!grepl("\\.", fun_names)]

for(fn in fun_names) {
  test_that(paste(fn, "has a default method"), {
    expect_true(any(grepl(paste0("^", fn, "\\.default$"), utils::methods(fn))))
  })
  test_that(paste(fn, "works"), {
    skip_if(grepl("twomode|uniplex|time|ego|blockmodel|combined", fn), message = "Some functions need more input")
    skip_if(grepl("mode1|mode2|matching", fn), message = "Some functions expect a two-mode network")
    skip_if(grepl("eulerian|dominating", fn), message = "Some functions have internal errors")
    expect_no_error(to_funs[[fn]](create_ring(5)))
  })
}
# Test transform functions

test_that("to_giant works",{
  fm <- to_uniplex(fict_marvel, tie = "relationship")
  expect_equal(c(net_nodes(fm)), 53)
  expect_equal(c(net_nodes(to_giant(fm))), 50)
  expect_equal(c(net_nodes(to_giant(as_igraph(fm)))), 50)
  # expect_equal(c(net_nodes(to_giant(as_matrix(fict_marvel)))), 50)
  # expect_equal(c(net_nodes(to_giant(as_network(fict_marvel)))), 50)
  expect_equal(c(net_nodes(to_giant(as_edgelist(fm)))), 50)
})

test_that("to_giant is a wrapper for the first component",{
  fm <- to_uniplex(fict_marvel, tie = "relationship")
  expect_identical(as_matrix(to_component(fm)), as_matrix(to_giant(fm)))
  expect_identical(as_matrix(to_component(fm, component = 1)),
                   as_matrix(to_giant(fm)))
})

test_that("to_component selects by size rank",{
  fm <- to_uniplex(fict_marvel, tie = "relationship")
  expect_equal(c(net_nodes(to_component(fm, 1))), 50)
  expect_equal(c(net_nodes(to_component(fm, 2))), 1)
  expect_error(to_component(fm, 99), "between 1 and 4")
  expect_error(to_component(fm, c(1, 2)), "between 1 and 4")
})

test_that("to_component selects by node name",{
  fm <- to_uniplex(fict_marvel, tie = "relationship")
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
  orig <- ison_adolescents %>%
    mutate_ties(wave = sample(1995:1998, 10, replace = TRUE))
  waves <- to_waves(orig, attribute = "wave")
  from_wave <- from_waves(waves)
  expect_length(waves, length(unique(tie_attribute(orig, "wave"))))
  expect_length(from_wave, length(as_igraph(orig)))
})

test_that("to and from slices work", {
  orig <- ison_adolescents %>%
    mutate_ties(time = 1:10, increment = 1) %>%
    add_ties(c(1,2), list(time = 3, increment = -1))
  slice <- to_slices(orig, slice = 7)
  expect_length(slice, length(orig))
  #expect_false(is.null(tie_attribute(slice, "time")))
  ##should attribute names change?
  slices <- ison_adolescents %>%
    mutate_ties(time = 1:10, increment = 1) %>%
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
  # snet_warn() emits a cli alert, which is a message rather than a warning,
  # and cli alerts are silenced unless manynet is set to be verbose
  op <- options(snet_verbosity = "verbose")
  on.exit(options(op), add = TRUE)
  expect_message(to_mode1(valued, "jaccard"), "binary")
  # dichotomising is what the message says it does
  expect_equal(suppressMessages(to_mode1(valued, "jaccard")),
               to_mode1((valued > 0) * 1, "jaccard"))
  # whereas a measure defined for valued data uses the values
  expect_silent(to_mode1(valued, "crossmin"))
  expect_false(isTRUE(all.equal(to_mode1(valued, "crossmin"),
                                to_mode1(sw_mat, "crossmin"))))
})

test_that("every projection measure works across classes", {
  for (s in c("match", "overlap", "crossmin", "bonacich", "covariance")) {
    expect_s3_class(to_mode1(ison_southern_women, s), "tbl_graph")
    expect_true(is.matrix(to_mode1(sw_mat, s)))
    expect_s3_class(to_mode2(ison_southern_women, s), "tbl_graph")
  }
})

test_that("projections record which measure was used", {
  expect_match(igraph::graph_attr(to_mode1(ison_southern_women, "jaccard"),
                                  "transform"),
               "mode-1 projection (jaccard)", fixed = TRUE)
  expect_match(igraph::graph_attr(to_mode2(ison_southern_women, "bonacich"),
                                  "transform"),
               "mode-2 projection (bonacich)", fixed = TRUE)
})
