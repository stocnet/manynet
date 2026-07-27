# Test split functions

egos <- ison_adolescents %>%
    tidygraph::activate(edges)

test_that("to_ and from_ egos works", {
  expect_length(to_egos(ison_brandes), length(ison_brandes))
  expect_length(to_egos(ison_brandes), length(to_egos(ison_brandes, 2)))
  expect_length(egos, length(from_egos(to_egos(egos))))
  expect_s3_class(to_egos(egos)[[1]], "tbl_graph")
  expect_s3_class(from_egos(to_egos(egos)), "tbl_graph")
})

unicorn <- ison_adolescents |> 
    tidygraph::activate(nodes) |> 
    mutate(unicorn = rep(c("yes", "no"), 4))

test_that("to_ and from_ subgraphs works", {
  expect_length(to_subgraphs(unicorn, "unicorn"), 2)
  expect_length(from_subgraphs(to_subgraphs(unicorn, "unicorn")),
               length(unicorn))
  expect_s3_class(to_subgraphs(unicorn, "unicorn")[[1]],
                  "tbl_graph")
  expect_s3_class(from_subgraphs(to_subgraphs(unicorn, "unicorn")),
                  "tbl_graph")
})

test_that("to_components works", {
  expect_length(to_components(to_uniplex(fict_marvel,"relationship")), 4)
  expect_length(to_components(ison_adolescents), 1)
  expect_s3_class(to_components(ison_adolescents)[[1]], "tbl_graph")
})

set.seed(1234)
wave <- ison_adolescents %>%
    tidygraph::activate(edges) %>%
    mutate(wave = sample(1995:1998, 10, replace = TRUE))

test_that("to_waves works", {
  expect_length(to_waves(wave), 4)
  expect_length(to_waves(wave, panels = c(1995, 1996)), 2)
  expect_length(from_waves(to_waves(wave)), 8)
  expect_s3_class(to_waves(wave)[[1]], "tbl_graph")
  expect_s3_class(from_waves(to_waves(wave)), "tbl_graph")
})

set.seed(1234)
yearly <- ison_adolescents %>%
    tidygraph::activate(edges) %>%
    mutate(year = sample(1:12, 10, replace = TRUE))

test_that("to_waves honours an explicitly named attribute (stocnet/autograph#40)", {
  # "year" is not one of the attributes that mark a network longitudinal,
  # but naming it explicitly should still split the network by it.
  expect_length(to_waves(yearly, attribute = "year"),
                length(unique(tie_attribute(yearly, "year"))))
  expect_s3_class(to_waves(yearly, attribute = "year")[[1]], "tbl_graph")
})

test_that("cumulative waves accumulate and stay in natural order", {
  cumul <- to_waves(yearly, attribute = "year", cumulative = TRUE)
  expect_true(is.list(cumul) && !is_manynet(cumul))
  # Numeric wave names must sort numerically, not lexicographically
  expect_equal(names(cumul),
               as.character(sort(unique(tie_attribute(yearly, "year")))))
  # Cumulative: each successive wave has at least as many ties as the last
  nties <- vapply(cumul, net_ties, numeric(1))
  expect_true(all(diff(nties) >= 0))
})

test_that("to_waves works for diff_model objects", {
  skip_on_cran()
  skip_on_ci()
  expect_warning(wave_diff <- play_diffusion(ison_brandes, old_version = TRUE))
  expect_length(to_waves(wave_diff), length(wave_diff$t))
  expect_values(net_nodes(to_waves(wave_diff)[[1]]),
               net_nodes(to_waves(wave_diff)[[12]]))
  expect_values(net_ties(to_waves(wave_diff)[[1]]),
               net_ties(to_waves(wave_diff)[[12]]))
  expect_values(net_nodes(to_waves(wave_diff)[[1]]),
               net_nodes(ison_brandes))
  expect_true(node_attribute(to_waves(wave_diff)[[1]], "Infected")[1])
  expect_false(node_attribute(to_waves(wave_diff)[[7]], "Exposed")[1])
  expect_false(node_attribute(to_waves(wave_diff)[[10]], "Recovered")[1])
})

slice <- ison_adolescents %>%
    mutate_ties(time = 1:10, increment = 1) %>%
    add_ties(c(1,2), list(time = 3, increment = -1))

test_that("to_ and from_ slices works", {
  expect_length(to_slices(slice, slice = 7), length(ison_adolescents))
  expect_lt(length(igraph::edge_attr(to_slices(slice, slice = 7), "weight")), 7)
  expect_length(to_slices(slice, slice = c(5, 7)), 2)
  expect_s3_class(to_slices(slice, slice = 7), "igraph")
})

test_that("to_time slices an interval (begin/end) network at each change", {
  # irps_wwi is a spell network: ties carry begin/end lifespans, not a `time`.
  changes <- sort(unique(c(tie_attribute(irps_wwi, "begin"),
                           tie_attribute(irps_wwi, "end"))))
  sl <- to_time(irps_wwi)
  # One slice per change point (each tie beginning or end), named and in order.
  expect_type(sl, "list")
  expect_length(sl, length(changes))
  expect_equal(names(sl), as.character(changes))
  expect_s3_class(sl[[1]], "tbl_graph")
  # Half-open [begin, end): a tie active at 1904 is one begun by then, not ended.
  expect_true(all(tie_attribute(sl[["1904"]], "begin") <= 1904))
  expect_true(all(tie_attribute(sl[["1904"]], "end") > 1904))
  # Supplying a single time returns one snapshot of the ties active then.
  one <- to_time(irps_wwi, 1901)
  expect_s3_class(one, "tbl_graph")
  expect_false(is.list(one) && !is_graph(one))
  expect_equal(as.numeric(net_ties(one)),
               sum(tie_attribute(irps_wwi, "begin") <= 1901 &
                     tie_attribute(irps_wwi, "end") > 1901))
  # igraph input yields a list of igraphs (mirroring to_slices()/to_waves()).
  sli <- to_time(as_igraph(irps_wwi))
  expect_length(sli, length(changes))
  expect_s3_class(sli[[1]], "igraph")
})
