lossless_roundtrip <- function(obj, to_class) {
  from_class <- class(obj)[1]
  if(to_class == "tbl_graph") to_class <- "tidygraph"
  if(from_class == "tbl_graph") from_class <- "tidygraph"
  to_fun     <- get(paste0("as_", to_class))
  back_fun   <- get(paste0("as_", from_class))
  
  obj2 <- back_fun(to_fun(obj))
  
  identical(as_matrix(obj), as_matrix(obj2))
}

for(ms in manynet_classes){
  to_classes <- setdiff(manynet_classes, ms)
  
  test_that(paste("coercion from", ms, "to other classes is lossless where expected"), {
    mat <- matrix(c(0,1,1,0,0,1,1,1,0), 3, 3)
    obj <- switch(ms,
                  "matrix" = mat,
                  "igraph" = igraph::graph_from_adjacency_matrix(mat),
                  "graphAM" = methods::new("graphAM", adjMat = mat, edgemode = "directed"),
                  "tbl_graph" = tidygraph::as_tbl_graph(mat),
                  "network" = network::network(mat),
                  "stocnet" = as_stocnet(mat)
    )
    
    for (to in to_classes) {
      expect_true(
        lossless_roundtrip(obj, to),
        info = paste("Lossy coercion:", ms, "(obj) →", to, "(obj2)")
      )
    }
  })
}

test_that("stocnet <-> network round-trip retains multiplex/multi-wave ties", {
  sn <- make_stocnet(nodes = data.frame(label = as.character(1:4)),
    ties = data.frame(from = c(1,2,3,1,2,4), to = c(2,3,1,3,4,1),
                      layer = "net", weight = 1, time = c(1,1,1,2,2,2)))
  nw <- as_network(sn)
  # The network representation should carry the tie attributes, not just 'na'.
  expect_true(all(c("layer", "time") %in% network::list.edge.attributes(nw)))
  # Reading the network back should not error and should recover the ties.
  back <- as_stocnet(nw)
  expect_s3_class(back, "stocnet")
  expect_true(all(c("layer", "time") %in% names(back$ties)))
  expect_equal(nrow(back$ties), nrow(sn$ties))
  expect_equal(sort(back$ties$time), sort(sn$ties$time))
  expect_identical(as_matrix(sn), as_matrix(back))
})

test_that("two-mode stocnet <-> network round-trip retains ties and attributes", {
  # A two-mode network with a tie attribute (date/wave) including a repeated
  # dyad across waves, which requires a multi-edge bipartite network.
  sn <- make_stocnet(
    nodes = data.frame(label = c("a1","a2","a3","e1","e2"),
                       mode  = c("actor","actor","actor","event","event")),
    ties  = data.frame(from = c("a1","a2","a1","a3"),
                       to   = c("e1","e2","e1","e1"),
                       time = c(1,1,2,2)))
  expect_true(is_twomode(sn))
  nw <- as_network(sn)
  expect_true(network::is.bipartite(nw))
  expect_true("time" %in% network::list.edge.attributes(nw))
  back <- as_stocnet(nw)
  expect_s3_class(back, "stocnet")
  expect_true(is_twomode(back))
  expect_equal(nrow(back$ties), nrow(sn$ties))
  expect_true("time" %in% names(back$ties))
  expect_equal(sort(back$ties$time), sort(sn$ties$time))
  expect_identical(as_matrix(sn), as_matrix(back))
})

# Tests for the as_ conversion methods
mat1 <- matrix(c(0,1,0,0,1,0,1,0,0,1,0,1,0,0,0,0),4,4, byrow = TRUE)
rownames(mat1) <- LETTERS[1:4]
colnames(mat1) <- LETTERS[1:4]
mat2 <- matrix(c(0,1,0,0,2,0,3,0,0,4,0,5,0,0,0,0),4,4, byrow = TRUE)
rownames(mat2) <- LETTERS[1:4]
colnames(mat2) <- LETTERS[1:4]
# Unweighted test
data1 <- dplyr::arrange(data.frame(from = c("A","B","B","C","C"),
                    to = c("B","C","A","D","B")),
                    from, to)
# Weighted test
data2 <- data1
data2$weight <- 1:5

# Data 3: misnamed weight col
data3 <- data1
data3$hello <- 1:5

test_that("as_edgelist converts correctly", {
  expect_s3_class(as_edgelist(as_igraph(data2)), "tbl_df")
  expect_equal(as_edgelist(as_igraph(data2)), dplyr::as_tibble(data2))
  expect_equal(as_edgelist(as_igraph(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_tidygraph(data2)), dplyr::as_tibble(data2))
  expect_equal(as_edgelist(as_tidygraph(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_network(data1)), dplyr::as_tibble(data1))
  expect_equal(as_edgelist(as_network(data2)), dplyr::as_tibble(data2))
})

test_that("data frame converted to matrix correctly",{
  expect_equal(as_matrix(data1), mat1)
  expect_equal(as_matrix(data2), mat2)
})

test_that("as_matrix converts correctly",{
  expect_vector(as_matrix(mat1))
  expect_vector(as_matrix(ison_southern_women))
  expect_vector(ison_southern_women |> as_matrix())
  expect_equal(as_matrix(as_network(ison_southern_women)),
               as_matrix(ison_southern_women))
})

test_that("as_igraph converts correctly",{
  expect_s3_class(as_igraph(mat1), "igraph")
  expect_s3_class(as_igraph(ison_southern_women), "igraph")
  expect_s3_class(as_igraph(as_network(ison_southern_women)), "igraph")
  expect_error(as_igraph(data3, weight = T))
  expect_equal(igraph::vcount(as_igraph(as_network(data2))),
               igraph::vcount(as_igraph(data2)))
  # NB: ordering of edges is a little different when converting from network
  # to igraph. Should not matter though.
})

test_that("as_graphAM converts correctly",{
  expect_s4_class(as_graphAM(mat1), "graphAM")
  expect_s4_class(as_graphAM(ison_southern_women), "graphAM")
  expect_s4_class(as_graphAM(as_network(ison_southern_women)), "graphAM")
  expect_equal(as_graphAM(as_network(data2))@edgemode, "directed")
})

test_that("as_tidygraph converts correctly",{
  expect_s3_class(as_tidygraph(mat1), "tbl_graph")
  expect_s3_class(as_tidygraph(ison_southern_women), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(mat1)), "tbl_graph")
  expect_s3_class(as_tidygraph(as_network(ison_southern_women)),
                  "tbl_graph")
})

test_that("as_network converts correctly",{
  expect_s3_class(as_network(mat1), "network")
  expect_s3_class(as_network(ison_southern_women), "network")
  expect_equal(as_network(as_network(data2)), as_network(data2))
  expect_equal(as_network(as_igraph(ison_southern_women)),
               as_network(ison_southern_women))
  expect_equal(igraph::vcount(as_igraph(as_network(dplyr::as_tibble(data2)))),
               igraph::vcount(as_igraph(as_network(data2))))
  expect_equal(is_directed(ison_southern_women),
               is_directed(as_network(ison_southern_women)))
  # NB: ordering of edges is a little different when converting from network
  # to igraph. Should not matter though.
})

# test conversion of siena objects
test_that("stocnet <-> sienadata coercion is lossless", {
  skip_if_not_installed("RSiena")
  set.seed(42)
  n <- 7; w <- 3
  arr <- array(sample(0:1, n * n * w, replace = TRUE, prob = c(.7, .3)),
               dim = c(n, n, w))
  for (i in seq_len(w)) diag(arr[, , i]) <- 0
  dimnames(arr) <- list(paste0("A", seq_len(n)), paste0("A", seq_len(n)), NULL)
  fr <- RSiena::sienaDependent(arr)
  sm <- RSiena::sienaDependent(matrix(sample(1:5, n * w, replace = TRUE), n, w),
                               type = "behavior")
  ag <- RSiena::coCovar(c(1, 2, NA, 4, 5, 6, 7))
  alc <- RSiena::varCovar(matrix(stats::rnorm(n * (w - 1)), n, w - 1))
  prox <- RSiena::coDyadCovar(matrix(stats::rnorm(n * n), n, n))
  vd <- RSiena::varDyadCovar(array(stats::rnorm(n * n * (w - 1)),
                                   dim = c(n, n, w - 1)))
  orig <- RSiena::sienaDataCreate(fr, sm, ag, alc, prox, vd)

  sn <- as_stocnet(orig)
  expect_s3_class(sn, "stocnet")
  # dependent networks and behaviours are recorded as focal
  expect_setequal(sn$info$focal, c("fr", "sm"))
  # covariate centering is carried in a named logical vector
  expect_type(sn$info$centered, "logical")

  back <- as_siena(sn)
  expect_s3_class(back, "sienadata")
  expect_equal(back$observations, orig$observations)
  expect_equal(names(orig$depvars), names(back$depvars))
  expect_equal(names(orig$cCovars), names(back$cCovars))
  expect_equal(names(orig$vCovars), names(back$vCovars))
  expect_equal(names(orig$dycCovars), names(back$dycCovars))
  expect_equal(names(orig$dyvCovars), names(back$dyvCovars))
  # values round-trip exactly (including missing and node labels)
  expect_equal(as.vector(orig$depvars$fr), as.vector(back$depvars$fr))
  expect_equal(as.vector(orig$depvars$sm), as.vector(back$depvars$sm))
  expect_equal(as.vector(orig$cCovars$ag), as.vector(back$cCovars$ag))
  expect_equal(as.vector(orig$vCovars$alc), as.vector(back$vCovars$alc))
  expect_equal(as.vector(orig$dycCovars$prox), as.vector(back$dycCovars$prox))
  expect_equal(as.vector(orig$dyvCovars$vd), as.vector(back$dyvCovars$vd))
  expect_equal(dimnames(orig$depvars$fr)[[1]], dimnames(back$depvars$fr)[[1]])
})

test_that("missing ties survive the stocnet <-> sienadata round trip", {
  skip_if_not_installed("RSiena")
  set.seed(7)
  n <- 6; w <- 2
  arr <- array(sample(0:1, n * n * w, replace = TRUE, prob = c(.7, .3)),
               dim = c(n, n, w))
  for (i in seq_len(w)) diag(arr[, , i]) <- 0
  arr[1, 2, 2] <- NA
  arr[3, 4, 2] <- NA
  fr <- RSiena::sienaDependent(arr)
  orig <- RSiena::sienaDataCreate(fr)

  sn <- as_stocnet(orig)
  # a tie recorded as missing is neither a row of the ties nor the absence of
  # one, which would record an observed non-tie
  expect_equal(nrow(as_missinglist(sn)), 2)
  expect_false(any(is.na(sn$ties[["weight"]])))
  # scattered over the ties as they are, they stay in the registry
  expect_equal(nrow(sn$missings), 2)
  # the network is no more weighted for holding them
  expect_false(is_weighted(sn))
  expect_false(is_weighted(as_igraph(sn)))
  # a matrix holds them as missing cells
  expect_true(anyNA(as_matrix(sn)))
  expect_equal(net_tie_missing(as_matrix(sn)), 2 / (n * n))

  back <- as_siena(sn)
  expect_equal(as.vector(orig$depvars$fr), as.vector(back$depvars$fr))
  expect_equal(sum(is.na(back$depvars$fr)), 2)
  # treating them as absent ties clears the record and adds no tie
  zeroed <- impute_ties(sn, "zero")
  expect_null(as_missinglist(zeroed))
  expect_equal(nrow(zeroed$ties), nrow(sn$ties))
  expect_false(anyNA(as_matrix(zeroed)))
})

test_that("a matrix holding missing ties coerces to every other class", {
  m <- as_matrix(ison_adolescents)
  m[2, 3] <- NA; m[3, 2] <- NA
  # igraph refuses an adjacency matrix holding a missing value, so the tie is
  # made and its weight set missing rather than the coercion erroring
  g <- as_igraph(m)
  expect_equal(sum(is.na(igraph::E(g)$weight)), 1)
  expect_false(is_weighted(g))
  expect_equal(as_matrix(g), m)
  expect_equal(as_matrix(as_network(m)), m)
  expect_equal(as_matrix(as_stocnet(m)), m)
  # a directed matrix keeps the direction its missing tie was recorded in
  d <- matrix(c(0, 1, NA, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  expect_equal(as_matrix(as_igraph(d)), d)
  # as does a two-mode one
  tm <- as_matrix(ison_southern_women)
  tm[1, 2] <- NA
  expect_equal(as_matrix(as_igraph(tm)), tm)
})

test_that("as_stocnet() keeps a network's isolates", {
  # an isolate appears in no tie, so it is lost unless the nodes record it
  g <- igraph::disjoint_union(as_igraph(create_ring(5)),
                              igraph::make_empty_graph(2, directed = FALSE))
  sn <- as_stocnet(g)
  expect_equal(as.numeric(net_nodes(sn)), 7)
  expect_equal(igraph::vcount(as_igraph(sn)), 7)
  expect_false(is_labelled(sn))
  expect_equal(as_matrix(sn), as_matrix(g))
})

test_that("as_stocnet() keeps what it cannot use out of its way", {
  # a 'grand' attribute is one of several, not a replacement for the rest
  g <- igraph::set_graph_attr(as_igraph(create_ring(3)), "grand",
                              list(name = "A network"))
  g <- igraph::set_graph_attr(g, "creator", "Gephi")
  expect_equal(as_infolist(g)$creator, "Gephi")
  expect_equal(as_infolist(g)$name, "A network")
  # a file can record anything at the network level, including a name that is
  # not a character string, which must not break the functions that follow
  h <- igraph::set_graph_attr(as_igraph(create_ring(3)), "name", 2019)
  sn <- as_stocnet(h)
  expect_null(sn$info$name)
  expect_s3_class(make_stocnet(info = sn$info, nodes = sn$nodes,
                               ties = sn$ties), "stocnet")
})

test_that("ison_classmates records the pupils who did not report", {
  x <- ison_classmates
  # three pupils did not answer at a wave, each logged as a change of their
  # response and a change back at the wave they answer again
  na <- x$changes[x$changes$var == "na", ]
  expect_equal(nrow(na), 6)
  expect_equal(na$node, c(2L, 2L, 16L, 19L, 16L, 19L))
  expect_equal(unlist(na$value), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  # the pupil who leaves the class is a change of activity instead
  active <- x$changes[x$changes$var == "active", ]
  expect_equal(nrow(active), 1)
  expect_equal(active$time, 3)
  expect_false(is_weighted(x))
})

test_that("ties recorded as missing are not ties", {
  x <- ison_classmates
  # the ties hold the ties, so nothing counts them that should not
  expect_equal(nrow(x$ties), 546)
  expect_equal(c(net_ties(x)), 546)
  expect_equal(c(net_ties(as_igraph(x))), 546)
  expect_equal(sum(layer_ties(x)), 546)
  expect_match(describe_ties(x), "460 friendship arcs")
  # 73 nominations were not observed: 25 at wave 2 and 48 at wave 3
  miss <- as_missinglist(x)
  expect_equal(nrow(miss), 73)
  expect_equal(sum(miss$time == 2), 25)
  expect_equal(sum(miss$time == 3), 48)
  expect_setequal(miss$from, c(2, 16, 19))
  # the pupil who is not in the class from wave 3 misses nothing then, since
  # there was nothing there to miss, but is a receiver while still present
  expect_equal(sum(miss$time == 3 & miss$to == 21), 0)
  expect_equal(sum(miss$time == 2 & miss$to == 21), 1)
  # a network recording no missing ties is unaffected
  expect_equal(c(net_ties(ison_adolescents)), 10)
  expect_null(as_missinglist(ison_adolescents))
})

test_that("every class carries the missing ties its own way", {
  x <- ison_classmates
  # igraph cannot mark an edge as missing, so they travel beside the edges
  g <- as_igraph(x)
  expect_equal(igraph::ecount(g), 546)
  expect_equal(nrow(as_missinglist(g)), 73)
  # a network object marks them among its edges, as {ergm} expects, and its
  # own count omits them
  nw <- as_network(x)
  expect_equal(network::network.edgecount(nw), 546)
  expect_equal(network::network.edgecount(nw, na.omit = FALSE), 619)
  expect_equal(c(net_ties(nw)), 546)
  expect_equal(nrow(as_missinglist(nw)), 73)
  # a matrix marks them as missing cells
  expect_equal(sum(is.na(as_matrix(x))), 73)
  # and each is read back into the same records
  for (back in list(as_stocnet(g), as_stocnet(nw))) {
    expect_equal(nrow(back$ties), 546)
    expect_equal(nrow(as_missinglist(back)), 73)
    expect_equal(sum(back$changes$var == "na"), 6)
    expect_null(back$missings)
  }
})

test_that("net_tie_missing counts the ties a network could have observed", {
  # 73 missing nominations over 26 pupils and the five layer-waves recorded,
  # so 26*25 ties on each of those five occasions
  expect_equal(net_tie_missing(ison_classmates), 73 / (26 * 25 * 5))
  # a matrix holds one cell per dyad, so it cannot hold the several ties a
  # longitudinal multiplex network holds for each, and reports more missing
  expect_gt(net_tie_missing(as_matrix(ison_classmates)),
            net_tie_missing(ison_classmates))
  expect_equal(net_tie_missing(ison_adolescents), 0)
})

test_that("a stocnet holds six components, of which every table is plural", {
  expect_named(ison_classmates,
               c("info", "nodes", "ties", "changes", "globals", "missings"))
  # the missings component holds the ties no node's non-response implies
  sn <- make_stocnet(nodes = data.frame(label = LETTERS[1:4]),
                     ties = data.frame(from = c("A", "A", "B"),
                                       to = c("B", "C", "C"),
                                       na = c(TRUE, FALSE, FALSE)))
  expect_equal(nrow(sn$missings), 1)
  # a missing tie is not a node's non-response, so the nodes gain no 'na'
  expect_false("na" %in% names(sn$nodes))
  expect_equal(nrow(as_missinglist(sn)), 1)
  # and takes node labels, which are indexed as the ties are
  sn2 <- make_stocnet(nodes = data.frame(label = LETTERS[1:4]),
                      ties = data.frame(from = "A", to = "C"),
                      missings = data.frame(from = "A", to = "B"))
  expect_equal(sn2$missings$from, 1L)
  expect_equal(sn2$missings$to, 2L)
  expect_equal(nrow(as_stocnet(as_igraph(sn2))$missings), 1)
})

test_that("globals survive the round trip through every class", {
  sn <- mutate_globals(as_stocnet(ison_algebra),
                       time = 1, var = "term", value = 1)
  expect_named(sn$globals, c("time", "var", "value"))
  expect_true("globals" %in% igraph::graph_attr_names(as_igraph(sn)))
  expect_equal(nrow(as_stocnet(as_igraph(sn))$globals), 1)
  expect_equal(nrow(as_stocnet(as_network(sn))$globals), 1)
  # the attribute was called 'global' before the component was renamed
  old <- igraph::set_graph_attr(as_igraph(ison_algebra), "global", sn$globals)
  expect_equal(nrow(as_globallist(old)), 1)
})

test_that("make_stocnet compresses a ties table marking its missing ties", {
  # handing over one row per missing tie is a reasonable way to give the data,
  # and is how the other classes hold it, so it is accepted and compressed
  sn <- make_stocnet(
    nodes = data.frame(label = LETTERS[1:4]),
    ties = data.frame(from = c("A", "A", "A", "B"), to = c("B", "C", "D", "C"),
                      na = c(TRUE, TRUE, TRUE, FALSE)))
  expect_equal(nrow(sn$ties), 1)
  expect_true(sn$nodes$na[1])
  expect_false(any(sn$nodes$na[2:4]))
  expect_null(sn$missings)
  expect_equal(nrow(as_missinglist(sn)), 3)
})

test_that("as_siena renders the classmates' missing nominations as missing", {
  skip_if_not_installed("RSiena")
  d <- as_siena(ison_classmates)
  expect_equal(sum(is.na(d$depvars$friends[, , 2])), 25)
  expect_equal(sum(is.na(d$depvars$friends[, , 3])), 48)
  expect_false(is.null(d$compositionChange))
})

test_that("sienadata coerces through the wider coercion family", {
  skip_if_not_installed("RSiena")
  set.seed(1); n <- 6; w <- 2
  a <- array(sample(0:1, n * n * w, replace = TRUE), dim = c(n, n, w))
  for (i in seq_len(w)) diag(a[, , i]) <- 0
  d <- RSiena::sienaDataCreate(RSiena::sienaDependent(a))
  expect_s3_class(as_igraph(d), "igraph")
  expect_s3_class(as_tidygraph(d), "tbl_graph")
  expect_s3_class(as_network(d), "network")
})

test_that("as_siena errors clearly for a single-wave network", {
  skip_if_not_installed("RSiena")
  expect_error(as_siena(as_stocnet(generate_random(6))), "at least two waves")
  expect_error(as_siena(ison_adolescents), "at least two waves")
})

test_that("as_siena routes any coercible object through stocnet", {
  skip_if_not_installed("RSiena")
  # a longitudinal manynet network (waves encoded as a 'wave' tie attribute)
  # reaches SIENA through the stocnet path
  d <- as_siena(fict_potter)
  expect_s3_class(d, "sienadata")
  expect_gt(d$observations, 1)
  expect_false(is.null(d$compositionChange))
  # valued/signed dependent networks are refused with a helpful message
  expect_error(as_siena(ison_monks), "binary")
})

test_that("conversion of diff_model object works correctly", {
  skip_on_cran()
  skip_on_ci()
  expect_warning(diff <- play_diffusion(ison_brandes, old_version = TRUE))
  tidy_diff <- as_tidygraph(diff)
  expect_values(net_nodes(tidy_diff), net_nodes(ison_brandes))
  expect_values(net_ties(tidy_diff), net_ties(ison_brandes))
  expect_values(net_nodes(tidy_diff), max(diff$I))
})

test_that("mnet objects printed correctly", {
  expect_match(describe_network(ison_adolescents), "labelled, undirected")
  expect_match(describe_nodes(ison_adolescents), "8 adolescents")
  expect_match(describe_ties(ison_adolescents), "10 friendship")
  expect_null(describe_changes(ison_adolescents))
  skip_if(format(Sys.time(), "%H") >= "09", message = "Avoid verbose output tests during the day")
  expect_output(print(ison_adolescents), "A tibble")
})

# test_that("network dynamic converts correctly",{
#   skip_if_not(requireNamespace("networkDynamic", quietly = TRUE)) 
#   networkDynamic <- get("networkDynamic", asNamespace("networkDynamic"))
#   onset <- 7168
#   terminus <- 19843
#   nodeID <- c( 1,2,13,29,31,34,44,59,67,82,89,115,121,122,128,146,156,181,190,191,
#                197,211,223,274,288,289,301,302,334,351,393,394,396,418)
#   ndf <- data.frame(onset, terminus, nodeID)
#   start_time <- c(19517, 19794, 19138, 19425, 19837, 19805, 19438, 19462, 19507,
#                   19796, 19832, 19514, 19808, 19252, 19266, 19711, 19783, 19178,
#                   19348, 19508)
#   end_time <- c(19517, 19795, 19139, 19426, 19838, 19805, 19439, 19462, 19508,
#                 19796, 19836, 19515, 19809, 19254, 19266, 19712, 19783, 19178,
#                 19348, 19509)
#   from <- c(1, 418, 34, 396, 34, 223, 13, 334, 34, 191, 181, 2, 2, 211, 31, 156, 288, 289, 122, 156)
#   to <- c(156, 393, 351, 394, 146, 115, 274, 121, 29, 190, 89, 128, 44, 67, 302, 59, 34,301, 82, 197)
#   edf <- data.frame(start_time, end_time, from, to)
#   sample_net <- networkDynamic(vertex.spells = ndf[ ,c(1,2,3)],
#                                edge.spells = edf[ ,c(1,2,3,4)])
#   network::set.network.attribute(sample_net, 'net.obs.period', list(observations = list(c(7168,19843)), mode = "discrete", time.increment = 1, time.unit = 'day'))
#   class(sample_net)
#   expect_no_failure(as_igraph(sample_net))
# })

test_that("a directed multilevel network round trips through igraph", {
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
  ig <- as_igraph(net)
  # the network holds arcs, so it does not reach igraph as an undirected graph
  expect_true(igraph::is_directed(ig))
  # the undirected layer travels as a reciprocated pair, and collapses again
  expect_equal(igraph::ecount(ig), 4)
  back <- as_stocnet(ig)
  expect_equal(back$ties$from, net$ties$from)
  expect_equal(back$ties$to, net$ties$to)
  expect_equal(back$info$directed, net$info$directed)
})

test_that("a network of three modes keeps them through igraph", {
  net <- make_stocnet(
    info = list(modes = c("A", "B", "C")),
    nodes = tibble::tibble(label = c("a", "b", "x", "z"),
                           mode = c("A", "A", "B", "C")),
    ties = tibble::tibble(from = c(1L, 1L, 3L), to = c(2L, 3L, 4L))
  )
  ig <- as_igraph(net)
  # igraph records two modes as 'type' and more as 'lvl', which is what
  # net_modes() and is_multilevel() read; a bare 'mode' attribute is read by
  # neither, so the modes would be lost
  expect_true("lvl" %in% igraph::vertex_attr_names(ig))
  expect_false("mode" %in% igraph::vertex_attr_names(ig))
  expect_equal(net_modes(ig), 3)
  expect_equal(mode_nodes(ig), c(2L, 1L, 1L))
  expect_true(is_multilevel(ig))
  expect_equal(as_stocnet(ig)$nodes$mode, net$nodes$mode)
})

test_that("coercing to a network keeps what the network knows about itself", {
  sw <- as_stocnet(ison_southern_women)
  direct <- as_network(sw)
  # the graph attributes used to be dropped on the way through igraph
  expect_equal(sort(network::list.network.attributes(as_network(as_igraph(sw)))),
               sort(network::list.network.attributes(direct)))
  expect_equal(sort(network::list.network.attributes(as_network(as_tidygraph(sw)))),
               sort(network::list.network.attributes(direct)))
  # and the names 'network' reserves for itself are not overwritten by them
  via <- as_network(as_igraph(sw))
  for (f in c("n", "mnext", "directed", "bipartite", "hyper", "loops", "multiple"))
    expect_equal(network::get.network.attribute(via, f),
                 network::get.network.attribute(direct, f))
})
