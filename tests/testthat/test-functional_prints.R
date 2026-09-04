# Functional tests for print, summary and describe methods across the
# manynet result classes, plus the mnet $-accessors. These are exercised on
# representative objects of each class so that console output code paths stay
# covered without snapshotting exact (cli-styled) output.

expect_prints <- function(x, label) {
  # capture both stdout and the message stream ({cli} output) so test
  # output stays clean
  msgs <- capture.output(out <- capture.output(print(x)), type = "message")
  expect_true(is.character(c(out, msgs)), label = paste(label, "printing"))
  invisible(c(out, msgs))
}

# Returns the (unstyled) metadata header a measure object prints, or NA if it
# prints none. The header is always the first line; the "# ... and n more
# values" footer of print_tblvec() also begins with "#", so is excluded.
measure_header_of <- function(x) {
  out <- cli::ansi_strip(expect_prints(x, "measure header"))
  if (length(out) > 0 && grepl("^# ", out[1]) && !grepl("^# \\.\\.\\.", out[1]))
    trimws(out[1]) else NA_character_
}

test_that("print.mnet() prints all network components", {
  for (d in list(ison_adolescents, ison_southern_women, ison_algebra,
                 fict_starwars)) {
    expect_no_error(expect_prints(d, "mnet"))
  }
  expect_no_error(capture.output(print_all(ison_adolescents)))
})

test_that("mnet $-accessors get and set attributes", {
  net <- ison_adolescents
  expect_type(net$name, "character")
  expect_length(net$name, as.numeric(net_nodes(net)))
  net$tst <- seq_len(as.numeric(net_nodes(net)))
  expect_identical(net$tst, seq_len(as.numeric(net_nodes(net))))
  net$tie_tst <- seq_len(as.numeric(net_ties(net)))
  expect_identical(net$tie_tst, seq_len(as.numeric(net_ties(net))))
  net$glob <- "hello"
  expect_identical(net$glob, "hello")
  expect_error(net$absent, "No attribute")
  expect_error(net$oops <- 1:3, "Length")
  expect_type(net$"node$name", "character")
})

test_that("print.stocnet() prints stocnet objects", {
  expect_no_error(expect_prints(test_stocnet_obj, "stocnet"))
  expect_no_error(expect_prints(as_stocnet(ison_southern_women), "stocnet"))
})

test_that("describe_nodes() names every mode of a three-mode network", {
  # A 'stocnet' holds its modes in 'mode', which can name three or more,
  # and each of them is counted and named, see #174.
  three <- as_stocnet(fict_marvel)
  three$nodes$mode[1:5] <- "third"
  three$info$modes <- NULL
  expect_length(as.numeric(mode_nodes(three)), 3)
  expect_equal(sum(as.numeric(mode_nodes(three))),
               as.numeric(net_nodes(three)))
  desc <- describe_nodes(three)
  for (nm in mode_names(three))
    expect_match(desc, nm)
  # An igraph records more than two modes in 'lvl' rather than in 'type'.
  levelled <- to_multilevel(as_igraph(fict_marvel))
  levelled <- igraph::set_vertex_attr(levelled, "lvl",
                                      index = 1:5, value = 3)
  levelled <- igraph::set_graph_attr(levelled, "modes",
                                     c("hero", "team", "third"))
  expect_equal(as.numeric(net_modes(levelled)), 3)
  expect_length(as.numeric(mode_nodes(levelled)), 3)
  expect_match(describe_nodes(levelled), "third")
})

test_that("describe_*() helpers return informative strings", {
  for (d in list(ison_adolescents, ison_southern_women, ison_algebra,
                 fict_starwars)) {
    expect_type(describe_network(d), "character")
    expect_type(describe_nodes(d), "character")
    expect_type(describe_ties(d), "character")
  }
  expect_type(describe_changes(fict_starwars), "character")
  expect_null(describe_changes(ison_adolescents))
})

test_that("describe_transformations() says nothing where nothing is recorded", {
  expect_equal(describe_transformations(ison_adolescents), "")
  # a function with nothing to drop did not transform the network
  expect_equal(describe_transformations(delete_isolates(ison_adolescents)), "")
})

test_that("describe_transformations() gives the detail the console fits", {
  out <- to_component(fict_greys, 2)
  op <- options(cli.width = 60)
  on.exit(options(op), add = TRUE)
  expect_equal(describe_transformations(out),
               "exclusion: not in component 2 (47 nodes excluded)")
  # too narrow for the consequence, but wide enough for the method
  options(cli.width = 40)
  expect_equal(describe_transformations(out),
               "exclusion: not in component 2")
  # too narrow for either, so the name alone reports that it happened
  options(cli.width = 20)
  expect_equal(describe_transformations(out), "exclusion")
})

test_that("describe_transformations() details override the console width", {
  out <- to_component(fict_greys, 2)
  op <- options(cli.width = 20)
  on.exit(options(op), add = TRUE)
  expect_equal(describe_transformations(out, details = TRUE),
               "exclusion: not in component 2 (47 nodes excluded)")
})

test_that("describe_transformations() lists them in the order applied", {
  out <- delete_isolates(to_unweighted(
    to_undirected(add_nodes(ison_networkers, 1))))
  op <- options(cli.width = 30)
  on.exit(options(op), add = TRUE)
  expect_equal(describe_transformations(out),
               "symmetrisation, dichotomisation, and exclusion")
})

test_that("node_measure class prints and summarises", {
  net <- ison_adolescents
  m <- make_node_measure(stats::rnorm(8), net)
  expect_s3_class(m, "node_measure")
  expect_no_error(expect_prints(m, "node_measure"))
  s <- summary(m)
  expect_true(all(c("Minimum", "Maximum", "Mean", "StdDev") %in% names(s)))
  s2 <- summary(m, membership = rep(c("A", "B"), 4))
  expect_setequal(names(s2), c("A", "B"))
  # two-mode variant prints per mode
  m2 <- make_node_measure(
    stats::rnorm(as.numeric(net_nodes(ison_southern_women))),
    ison_southern_women)
  expect_no_error(expect_prints(m2, "node_measure twomode"))
})

test_that("tie_measure class prints", {
  m <- make_tie_measure(stats::rnorm(10), ison_adolescents)
  expect_s3_class(m, "tie_measure")
  expect_no_error(expect_prints(m, "tie_measure"))
})

test_that("network_measure class prints", {
  m <- make_network_measure(0.42, ison_adolescents,
                                      "net_thing(ison_adolescents)")
  expect_s3_class(m, "network_measure")
  expect_no_error(expect_prints(m, "network_measure"))
})

test_that("measure classes print interpretive metadata when present", {
  net <- ison_adolescents
  # measures made without the metadata print exactly as before:
  # no header line, no NULLs, no blank first line
  bare <- make_node_measure(stats::rnorm(8), net)
  expect_true(is.na(measure_header_of(bare)))

  # a measure rescaled in no way is given its range alone
  # (the label is capitalised here, so netrics need only record the name)
  m <- bare
  attr(m, "measure") <- "strength centrality"
  attr(m, "range") <- c(0, Inf)
  attr(m, "normalization") <- "none"
  expect_identical(measure_header_of(m), "# Strength centrality [0, Inf)")

  # a rescaled one names the rescaling first, in netrics' own words,
  # which are surfaced as given rather than translated here
  attr(m, "range") <- c(0, 1)
  for (norm in c("normalised", "scaled", "proportion")) {
    attr(m, "normalization") <- norm
    expect_identical(measure_header_of(m),
                     paste0("# Strength centrality, ", norm, " [0, 1]"))
  }

  # so vocabulary this version has never heard of still surfaces
  attr(m, "normalization") <- "sum to one"
  expect_identical(measure_header_of(m),
                   "# Strength centrality, sum to one [0, 1]")

  # partial metadata yields a partial header
  partial <- bare
  attr(partial, "measure") <- "eigenvector centrality"
  expect_identical(measure_header_of(partial), "# Eigenvector centrality")

  # a label given already capitalised, or irregularly cased, is left alone
  attr(partial, "measure") <- "PageRank"
  expect_identical(measure_header_of(partial), "# PageRank")

  # a normalisation without a range still reads as a phrase
  attr(partial, "normalization") <- "normalised"
  expect_identical(measure_header_of(partial), "# PageRank, normalised")

  # a character range is bracketed unless it brackets itself
  ranged <- bare
  attr(ranged, "range") <- "0-1"
  expect_identical(measure_header_of(ranged), "# [0-1]")
  attr(ranged, "range") <- "(0, 1]"
  expect_identical(measure_header_of(ranged), "# (0, 1]")

  # two-mode node measures print one header, not one per mode
  twomode <- make_node_measure(
    stats::rnorm(as.numeric(net_nodes(ison_southern_women))),
    ison_southern_women)
  attr(twomode, "measure") <- "degree"
  out <- cli::ansi_strip(expect_prints(twomode, "node_measure twomode header"))
  expect_length(grep("^# Degree", out), 1)

  # tie and network measures carry the same header
  tm <- make_tie_measure(stats::rnorm(10), net)
  expect_true(is.na(measure_header_of(tm)))
  attr(tm, "measure") <- "edge betweenness"
  attr(tm, "range") <- c(0, 1)
  expect_identical(measure_header_of(tm), "# Edge betweenness [0, 1]")

  nm <- make_network_measure(0.42, net, "net_thing(net)")
  expect_true(is.na(measure_header_of(nm)))
  attr(nm, "measure") <- "degree centralization"
  attr(nm, "normalization") <- "normalised"
  expect_match(measure_header_of(nm), "Degree centralization")

  # mode_measure objects (made by netrics) inherit the network_measure method
  mode <- nm
  class(mode) <- c("mode_measure", class(mode))
  attr(mode, "mode") <- c(10, 8)
  expect_match(measure_header_of(mode), "Degree centralization")
})

test_that("node_mark and tie_mark classes print", {
  net <- ison_adolescents
  nm <- make_node_mark(stats::runif(8) > 0.5, net)
  expect_no_error(expect_prints(nm, "node_mark"))
  tm <- make_tie_mark(stats::runif(10) > 0.5, net)
  expect_no_error(expect_prints(tm, "tie_mark"))
})

test_that("node_member class prints and summarises", {
  net <- ison_adolescents
  mb <- make_node_member(rep(c(1, 2), 4), net)
  expect_no_error(expect_prints(mb, "node_member"))
  expect_no_error(capture.output(summary(mb)))
})

test_that("tie_member class prints and summarises (#168)", {
  net <- ison_adolescents
  mb <- make_tie_member(rep(c(1, 2), 5), net)
  expect_s3_class(mb, "tie_member")
  # a tie is named by the pair of nodes it joins
  expect_equal(unname(head(names(mb), 2)), c("Betty-Sue", "Sue-Alice"))
  out <- cli::ansi_strip(expect_prints(mb, "tie_member"))
  expect_match(out[1], "^2 groups")
  summ <- cli::ansi_strip(capture.output(summary(mb)))
  expect_match(paste(summ, collapse = "\n"), "Class A:")
  expect_match(paste(summ, collapse = "\n"), "Betty-Sue")
  # a directed network names the pair with an arrow, and an unlabelled one
  # names each end by its place in the network
  arrows <- make_tie_member(c(1, 1, 2, 2), to_directed(create_ring(4)))
  expect_true(all(grepl("^[0-9]+->[0-9]+$", names(arrows))))
  # an object made another way holds no names, and lists the ties by place
  bare <- structure(c("A", "B", "A"), class = c("tie_member", "character"))
  expect_match(paste(cli::ansi_strip(capture.output(summary(bare))),
                     collapse = "\n"),
               "Class A:  1, 3")
})

test_that("diff_model prints and summarises", {
  set.seed(1234)
  d <- suppressWarnings(play_diffusion(create_ring(8), seeds = 1, steps = 5,
                                       old_version = TRUE))
  expect_s3_class(d, "diff_model")
  expect_no_error(expect_prints(d, "diff_model"))
  expect_s3_class(summary(d), "data.frame")
})

test_that("learn_model prints and summarises", {
  set.seed(1234)
  l <- play_learning(create_ring(8), beliefs = stats::runif(8), steps = 10)
  expect_no_error(expect_prints(l, "learn_model"))
  expect_no_error(capture.output(summary(l)))
})
