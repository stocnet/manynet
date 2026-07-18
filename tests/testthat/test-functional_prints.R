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

test_that("node_measure class prints and summarises", {
  net <- ison_adolescents
  m <- manynet:::make_node_measure(stats::rnorm(8), net)
  expect_s3_class(m, "node_measure")
  expect_no_error(expect_prints(m, "node_measure"))
  s <- summary(m)
  expect_true(all(c("Minimum", "Maximum", "Mean", "StdDev") %in% names(s)))
  s2 <- summary(m, membership = rep(c("A", "B"), 4))
  expect_setequal(names(s2), c("A", "B"))
  # two-mode variant prints per mode
  m2 <- manynet:::make_node_measure(
    stats::rnorm(as.numeric(net_nodes(ison_southern_women))),
    ison_southern_women)
  expect_no_error(expect_prints(m2, "node_measure twomode"))
})

test_that("tie_measure class prints", {
  m <- manynet:::make_tie_measure(stats::rnorm(10), ison_adolescents)
  expect_s3_class(m, "tie_measure")
  expect_no_error(expect_prints(m, "tie_measure"))
})

test_that("network_measure class prints", {
  m <- manynet:::make_network_measure(0.42, ison_adolescents,
                                      "net_thing(ison_adolescents)")
  expect_s3_class(m, "network_measure")
  expect_no_error(expect_prints(m, "network_measure"))
})

test_that("node_mark and tie_mark classes print", {
  net <- ison_adolescents
  nm <- manynet:::make_node_mark(stats::runif(8) > 0.5, net)
  expect_no_error(expect_prints(nm, "node_mark"))
  tm <- manynet:::make_tie_mark(stats::runif(10) > 0.5, net)
  expect_no_error(expect_prints(tm, "tie_mark"))
})

test_that("node_member class prints and summarises", {
  net <- ison_adolescents
  mb <- manynet:::make_node_member(rep(c(1, 2), 4), net)
  expect_no_error(expect_prints(mb, "node_member"))
  expect_no_error(capture.output(summary(mb)))
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
