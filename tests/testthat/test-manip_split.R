# How the splitting functions divide a network up: how many pieces each
# returns, which nodes and ties each piece holds, and what each records.
# That every to_*()/from_*() pair roundtrips, in every object class, is
# swept in test-functional_to.R and test-functional_from.R.

test_that("to_egos returns one ego network per node, at any distance", {
  expect_length(to_egos(ison_brandes), length(ison_brandes))
  expect_length(to_egos(ison_brandes), length(to_egos(ison_brandes, 2)))
})

unicorn <- ison_adolescents |> 
    tidygraph::activate(nodes) |> 
    mutate(unicorn = rep(c("yes", "no"), 4))

test_that("to_subgraphs returns one subgraph per value of the attribute", {
  expect_length(to_subgraphs(unicorn, "unicorn"), 2)
})

test_that("to_components works", {
  expect_length(to_components(to_uniplex(fict_marvel,"relationship")), 4)
  expect_length(to_components(ison_adolescents), 1)
  expect_s3_class(to_components(ison_adolescents)[[1]], "tbl_graph")
})

test_that("to_components is ordered from largest to smallest", {
  sizes <- vapply(to_components(to_uniplex(fict_marvel,"relationship")),
                  function(x) c(net_nodes(x)), numeric(1))
  expect_equal(sizes, c(50, 1, 1, 1))
  expect_false(is.unsorted(rev(sizes)))
})

test_that("to_components respects connectivity", {
  # fict_starwars is a single weak component, but not strongly connected
  expect_length(to_components(fict_starwars), 1)
  expect_length(to_components(fict_starwars, connectivity = "strong"), 64)
  expect_equal(c(net_nodes(to_components(fict_starwars,
                                         connectivity = "strong")[[1]])), 46)
})

set.seed(1234)
wave <- ison_adolescents |>
    tidygraph::activate(edges) |>
    mutate(wave = sample(1995:1998, 10, replace = TRUE))

test_that("to_waves works", {
  expect_length(to_waves(wave), 4)
  expect_length(to_waves(wave, panels = c(1995, 1996)), 2)
  expect_length(from_waves(to_waves(wave)), 8)
  expect_s3_class(to_waves(wave)[[1]], "tbl_graph")
  expect_s3_class(from_waves(to_waves(wave)), "tbl_graph")
})

set.seed(1234)
yearly <- ison_adolescents |>
    tidygraph::activate(edges) |>
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

test_that("to_waves returns networks without the attribute unchanged", {
  # Nothing to split on: the network is returned as it came in, in its own
  # class, rather than erroring or being iterated over as a list of waves.
  expect_s3_class(to_waves(ison_adolescents), "tbl_graph")
  expect_s3_class(to_waves(as_igraph(ison_adolescents)), "igraph")
  expect_equal(as.numeric(net_ties(to_waves(as_igraph(ison_adolescents)))),
               as.numeric(net_ties(ison_adolescents)))
  expect_equal(nrow(to_waves(as_edgelist(ison_adolescents))),
               nrow(as_edgelist(ison_adolescents)))
})

test_that("to_waves returns a single network for a single wave", {
  expect_s3_class(to_waves(wave, panels = 1995), "tbl_graph")
  expect_s3_class(to_waves(as_igraph(wave), panels = 1995), "igraph")
  expect_s3_class(to_waves(as_edgelist(wave), panels = 1995), "data.frame")
})

test_that("to_waves splits changing longitudinal networks by their ties", {
  waves <- to_waves(fict_starwars)
  # Waves are the waves of the ties, not just those in which nodes change
  expect_length(waves, net_waves(fict_starwars))
  expect_equal(sum(vapply(waves, net_ties, numeric(1))),
               as.numeric(net_ties(fict_starwars)))
  expect_false(any(vapply(waves, is_changing, logical(1))))
  expect_s3_class(to_waves(as_igraph(fict_starwars))[[1]], "igraph")
})

test_that("to_waves splits a changing network with no wave attribute", {
  # A diffusion result is both changing and longitudinal, but carries no tie
  # attributes at all, so its waves come from its changelist.
  set.seed(123)
  diff <- play_diffusion(create_ring(8), seeds = 1)
  expect_equal(net_tie_attributes(diff), character(0))
  waves <- to_waves(diff)
  expect_length(waves, length(unique(as_changelist(diff)$time)))
  expect_named(waves, paste("Wave", sort(unique(as_changelist(diff)$time))))
  expect_false(any(vapply(waves, is_changing, logical(1))))
  # Every wave keeps the whole network, and the seed is infected throughout
  expect_true(all(vapply(waves, function(w) as.numeric(net_nodes(w)),
                         numeric(1)) == 8))
  expect_true(all(vapply(waves, function(w)
    node_attribute(w, "diffusion")[1], character(1)) == "I"))
  expect_s3_class(to_waves(as_igraph(diff))[[1]], "igraph")
})

test_that("to_waves splits a panel whose waves are named 'time'", {
  # "time" marks a panel longitudinal as much as "wave" does, so a network
  # spelling its waves that way splits by them rather than erroring.
  expect_true(is_longitudinal(ison_monks))
  waves <- to_waves(ison_monks)
  expect_length(waves, length(unique(tie_attribute(ison_monks, "time"))))
  expect_equal(sum(vapply(waves, net_ties, numeric(1))),
               as.numeric(net_ties(ison_monks)))
  expect_s3_class(waves[[1]], class(ison_monks)[1])
  # Naming the attribute gives the same waves as letting it be found
  expect_length(to_waves(ison_monks, attribute = "time"), length(waves))
})

test_that("applied changes keep their type and cover every changing variable", {
  waves <- to_waves(fict_starwars)
  cl <- as_changelist(fict_starwars)
  # Every variable that changes is updated, not just the last one
  expect_true(all(unique(cl$var) %in% net_node_attributes(waves[[7]])))
  # A logical attribute stays logical rather than becoming character
  expect_type(node_attribute(waves[[7]], "active"), "logical")
  expect_type(node_attribute(waves[[7]], "faction"), "character")
  # The latest value at or before each wave wins
  active4 <- node_attribute(fict_starwars, "active")
  chg <- cl[cl$time <= 4 & cl$var == "active", ]
  chg <- chg[order(chg$time), ]
  chg <- chg[!duplicated(chg$node, fromLast = TRUE), ]
  active4[chg$node] <- as.logical(chg$value)
  expect_equal(as.vector(node_attribute(waves[[4]], "active")), active4)
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

slice <- ison_adolescents |>
    mutate_ties(time = 1:10, increment = 1) |>
    add_ties(c(1,2), list(time = 3, increment = -1))

test_that("to_ and from_ slices works", {
  expect_length(to_slices(slice, slice = 7), length(ison_adolescents))
  expect_lt(length(igraph::edge_attr(to_slices(slice, slice = 7), "weight")), 7)
  expect_length(to_slices(slice, slice = c(5, 7)), 2)
  expect_s3_class(to_slices(slice, slice = 7), "igraph")
})

test_that("to_slices returns networks without the attribute unchanged", {
  # No time attribute to slice on: return the network rather than filtering
  # ties on a variable that does not exist.
  expect_s3_class(to_slices(ison_adolescents), "tbl_graph")
  expect_equal(as.numeric(net_ties(to_slices(ison_adolescents))),
               as.numeric(net_ties(ison_adolescents)))
  expect_s3_class(to_slices(as_igraph(ison_adolescents)), "igraph")
})

test_that("to_slices works on unweighted, unincremented networks", {
  # Ties can only be dropped for summing to zero where they have a weight
  timed <- mutate_ties(ison_adolescents, time = seq_len(10) %% 3 + 1)
  sl <- to_slices(timed)
  expect_length(sl, length(unique(tie_attribute(timed, "time"))))
  expect_s3_class(sl[[1]], "tbl_graph")
  # Slices are cumulative in time: ties up to and including each moment
  expect_equal(as.numeric(net_ties(sl[["3"]])), 10)
  expect_s3_class(from_slices(sl), "tbl_graph")
})

test_that("to_times slices an interval (begin/end) network at each change", {
  # irps_wwi is an interval network: ties carry begin/end lifespans.
  changes <- sort(unique(c(tie_attribute(irps_wwi, "begin"),
                           tie_attribute(irps_wwi, "end"))))
  sl <- to_times(irps_wwi)
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
  sli <- to_times(as_igraph(irps_wwi))
  expect_length(sli, length(changes))
  expect_s3_class(sli[[1]], "igraph")
})

test_that("to_time scopes a timestamped network to one time point", {
  # ison_fraternity stamps each tie with the wave it was observed in, in a
  # `time` column rather than the `wave` column that panel networks use.
  one <- to_time(ison_fraternity, 3)
  expect_s3_class(one, "stocnet")
  expect_equal(as.numeric(net_ties(one)),
               sum(tie_attribute(ison_fraternity, "time") == 3))
  # The moment is no longer a variable of a network scoped to one moment.
  expect_false("time" %in% net_tie_attributes(one))
  expect_false(is_dynamic(one))
  # Asking for a moment beyond the last reverts to the last.
  expect_equal(as_matrix(to_time(ison_fraternity, 99)),
               as_matrix(to_time(ison_fraternity,
                                 max(tie_attribute(ison_fraternity, "time")))))
})

test_that("to_times returns one network per moment, always as a list", {
  ts <- to_times(ison_tailorshop)
  expect_type(ts, "list")
  expect_length(ts, 2)
  expect_equal(names(ts), c("1", "2"))
  expect_s3_class(ts[[1]], "stocnet")
  expect_equal(as_matrix(ts[[2]]), as_matrix(to_time(ison_tailorshop, 2)))
  # a subset of the moments, and a single moment, are still a list
  expect_length(to_times(ison_fraternity, 1:3), 3)
  expect_type(to_times(ison_fraternity, 1), "list")
  expect_length(to_times(ison_fraternity, 1), 1)
  # a network that records no moment records itself at one
  expect_length(to_times(ison_karateka), 1)
  # every class returns its own
  expect_s3_class(to_times(as_igraph(ison_monks))[[1]], "igraph")
  expect_s3_class(to_times(as_tidygraph(ison_monks))[[1]], "tbl_graph")
})

test_that("from_times rejoins what to_times returns", {
  out <- from_times(to_times(ison_tailorshop))
  expect_s3_class(out, "stocnet")
  expect_equal(as.numeric(net_ties(out)),
               as.numeric(net_ties(ison_tailorshop)))
  expect_equal(net_name(out), net_name(ison_tailorshop))
  expect_true("time" %in% net_tie_attributes(out))
  expect_setequal(out$ties$time, ison_tailorshop$ties$time)
  # an interval network already holds its moments in its ties
  expect_equal(as.numeric(net_ties(from_times(to_times(irps_wwi)))),
               as.numeric(net_ties(irps_wwi)))
})

test_that("to_slices and to_waves are unchanged by the time-form work", {
  # to_slices() accumulates an event network, one state per moment
  d <- sort(unique(tie_attribute(irps_nuclear, "time")))[c(10, 50)]
  sl <- to_slices(irps_nuclear, slice = d)
  expect_length(sl, 2)
  expect_named(sl, as.character(d))
  expect_lt(as.numeric(net_ties(sl[[1]])), as.numeric(net_ties(sl[[2]])))
  # to_waves() splits a changing panel by its tie waves
  wv <- to_waves(fict_starwars)
  expect_length(wv, net_waves(fict_starwars))
  expect_equal(names(wv), paste("Wave", 1:7))
})

test_that("the temporal family keeps what a stocnet knows about itself", {
  # A fixture built for this, since no shipped stocnet holds a missings
  # table, and the missings are what a round trip through another class
  # silently drops.
  x <- make_stocnet(
    info = list(name = "Fixture", layers = c("a", "b"),
                directed = c(a = TRUE, b = FALSE),
                observation = "panel", update = "replace"),
    nodes = data.frame(label = c("A", "B", "C", "D")),
    ties = data.frame(from = c(1, 2, 1, 2, 3),
                      to   = c(2, 3, 3, 3, 4),
                      layer = c("a", "a", "b", "b", "a"),
                      time = c(1, 1, 1, 2, 2)),
    missings = data.frame(from = 1, to = 4, layer = "a", time = 1))
  outs <- list(to_time = to_time(x, 1),
               to_wave = to_wave(x, 1),
               to_times = to_times(x)[["1"]])
  for(nm in names(outs)){
    out <- outs[[nm]]
    expect_s3_class(out, "stocnet")
    expect_equal(net_name(out), "Fixture", label = nm)
    expect_equal(layer_names(out), c("a", "b"), label = nm)
    expect_equal(as_infolist(out)$directed, c(a = TRUE, b = FALSE), label = nm)
    # what was scoped away is recorded, per GRAND item 4.4
    expect_match(as_infolist(out)$transformations$exclusion,
                 "not tied at time 1", label = nm)
  }
  # the missings are scoped the way the ties are, and not dropped
  expect_equal(nrow(to_time(x, 1)$missings), 1)
  expect_null(to_time(x, 2)$missings)
  # and the multiplex, changing case the round trip loses today
  y <- to_time(ison_classmates, 2)
  expect_equal(net_name(y), net_name(ison_classmates))
  expect_equal(layer_names(y), layer_names(ison_classmates))
  expect_false(is.null(as_infolist(y)$observation))
})

# What exclusion records ####

test_that("the splitting functions record what each piece left out", {
  # GRAND item 4.4: each piece leaves out what the others hold, so each
  # records its own exclusion against the network they were split from
  comps <- to_components(fict_greys)
  expect_equal(as_infolist(comps[[1]])$transformations$exclusion,
               "not in component 1 (13 nodes excluded)")
  expect_equal(as_infolist(comps[[2]])$transformations$exclusion,
               "not in component 2 (47 nodes excluded)")
  subs <- to_subgraphs(fict_greys, "sex")
  expect_match(as_infolist(subs[[1]])$transformations$exclusion,
               "^sex != [FM] \\([0-9]+ nodes excluded\\)$")
  egos <- to_egos(ison_adolescents)
  expect_match(as_infolist(egos[[1]])$transformations$exclusion,
               "^outside the ego network of [A-Za-z]+ \\([0-9]+ nodes excluded\\)$")
})

test_that("the splitting functions record which moment each piece holds", {
  waves <- ison_adolescents |> mutate_ties(wave = rep(1:2, 5)) |> to_waves()
  expect_equal(as_infolist(waves[[1]])$transformations$exclusion,
               "not tied at wave 1 (5 ties excluded)")
  slices <- ison_adolescents |> mutate_ties(time = rep(1:2, 5)) |> to_slices()
  expect_equal(as_infolist(slices[[1]])$transformations$exclusion,
               "after 1 (5 ties excluded)")
  # the last slice holds every tie, so it left nothing out
  expect_length(as_infolist(slices[[2]])$transformations, 0)
})

test_that("the splitting stocnet methods record without a round trip", {
  g <- as_stocnet(fict_greys)
  for (out in list(to_components(g)[[2]], to_subgraphs(g, "sex")[[1]],
                   to_egos(g)[[1]])) {
    expect_s3_class(out, "stocnet")
    expect_length(as_infolist(out)$transformations, 1)
  }
  expect_equal(c(net_nodes(to_components(g)[[1]])),
               c(net_nodes(to_components(fict_greys)[[1]])))
})
