names <- c("Lisa", "John", "Lily", "Ben", "Adam")
net <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                               to = c("B", "C", "D", "E", "A"))) |>
  mutate(name = names) |>
  mutate(gender = c("female", "male", "female", "male", "male"))

net2 <- as_tidygraph(data.frame(from = c("A", "B", "C", "D","E"),
                                to = c("B", "C", "D", "E", "A"))) |>
  mutate(friends = c("yes", "yes", "no", "no", "yes")) |>
  igraph::set_edge_attr("weight", value = 1:5)

net3 <- as_matrix(data.frame(from = c("A", "A", "B", "C", "D", "D", "E", "E"),
                   to = c("B", "G", "C", "D", "E", "G", "A", "H")))

friends <- c("yes", "yes", "no", "no", "yes")

test_that("node_names works", {
  expect_equal(node_names(net), names)
  expect_length(node_names(net), 5)
})

test_that("node_attribute works", {
  expect_equal(node_attribute(net2, "friends"), friends)
  expect_length(node_attribute(net2, "friends"), igraph::vcount(net2))
})

test_that("node_mode works", {
  expect_equal(as.logical(node_is_mode(ison_southern_women)[1]),
               as.logical(!node_is_mode(ison_southern_women)[length(ison_southern_women)]))
  expect_s3_class(node_is_mode(ison_southern_women), c("node mark", "logical"))
})

test_that("tie_attribute works", {
  expect_equal(unname(c(tie_attribute(net2, "weight"))), c(1, 2, 3, 4, 5))
})

test_that("tie_weights works", {
  expect_equal(c(tie_weights(net2)), c(tie_attribute(net2, "weight")))
})

test_that("net_nodes works", {
  expect_equal(c(net_nodes(net)), 5)
})

test_that("net_ties works", {
  expect_equal(c(net_ties(net)), 5)
})

test_that("net_dims works", {
  expect_equal(net_dims(ison_karateka), 34)
  expect_equal(net_dims(ison_southern_women), c(18,14))
})

test_that("layer_ties works", {
  # single-layer network returns the total tie count
  expect_equal(layer_ties(ison_southern_women), c(net_ties(ison_southern_women)))
  # multiplex network returns per-layer counts aligned to layer_names()
  lt <- layer_ties(fict_marvel)
  expect_length(lt, length(layer_names(fict_marvel)))
  expect_equal(sum(lt), c(net_ties(fict_marvel)))
  expect_equal(lt[match("affiliation", layer_names(fict_marvel))], 683L)
  expect_equal(lt[match("relationship", layer_names(fict_marvel))], 558L)
  # a curated single layer name not matching finer tie types returns the total
  expect_equal(layer_ties(fict_thrones), c(net_ties(fict_thrones)))
})

test_that("net_layers and layer_ties agree across network forms", {
  # layers are held in a 'type' tie attribute in igraph/tidygraph objects
  # and in a 'layer' column in stocnet objects, so both must be read
  for (nw in list(fict_marvel, ison_monks, ison_algebra)) {
    forms <- list(nw, as_igraph(nw), as_stocnet(nw), as_igraph(as_stocnet(nw)))
    layers <- vapply(forms, net_layers, numeric(1))
    expect_equal(layers, rep(net_layers(nw), length(forms)))
    for (form in forms) {
      expect_equal(layer_ties(form), layer_ties(nw))
      expect_equal(layer_names(form), layer_names(nw))
    }
  }
})

test_that("describe_ties reports per-layer counts for multiplex networks", {
  expect_match(describe_ties(fict_marvel), "558 relationship ties")
  expect_match(describe_ties(fict_marvel), "683 affiliation ties")
  expect_no_match(describe_ties(fict_marvel), "1241")
})

test_that("net_waves counts the waves of a panel, however it stamps them", {
  expect_equal(net_waves(ison_monks), 3)
  expect_equal(net_waves(ison_karateka), 1)
  # The waves come from the ties, so a change recorded after the last wave
  # states what became of a node without adding a wave to observe it in.
  expect_equal(net_waves(fict_potter), 6)
  # A panel that dates its waves in a 'time' column is still a panel
  expect_equal(net_waves(ison_tailorshop), 2)
  expect_equal(net_waves(ison_classmates), 4)
  # ison_fraternity records no moment 10, so it holds 15 waves and not 16
  expect_equal(net_waves(ison_fraternity), 15)
  # A dynamic network is not a panel, and observes itself once
  expect_equal(net_waves(irps_nuclear), 1)
  expect_equal(net_waves(irps_wwi), 1)
})

test_that("net_times counts the moments a network records, in any form", {
  temporal <- list(fict_potter, fict_starwars, ison_monks, ison_tailorshop,
                   ison_classmates, ison_fraternity, irps_nuclear, irps_wwi,
                   ison_karateka)
  # what `to_times()` returns one of is what `net_times()` counts
  for(x in temporal) expect_equal(net_times(x), length(to_times(x)))
  expect_equal(net_times(ison_tailorshop), 2)
  expect_equal(net_times(irps_nuclear),
               length(unique(tie_attribute(irps_nuclear, "time"))))
  # an interval network changes at every tie beginning and ending
  expect_equal(net_times(irps_wwi),
               length(unique(c(tie_attribute(irps_wwi, "begin"),
                               tie_attribute(irps_wwi, "end")))))
  # a network that records no moment records itself at one
  expect_equal(net_times(ison_karateka), 1)
  # nodal changes are moments too, which is where these two part company
  expect_equal(net_times(fict_potter), 7)
  expect_equal(net_waves(fict_potter), 6)
})

test_that("net_node_attributes works", {
  expect_equal(net_node_attributes(net), c("name", "gender"))
  expect_length(net_node_attributes(net), 2)
})

test_that("net_tie_attributes works", {
  expect_equal(net_tie_attributes(net2), "weight")
  expect_length(net_tie_attributes(net2), 1)
})

net_named <- add_info(ison_adolescents, name = "Adolescents", year = 1994)

test_that("as_infolist works", {
  out <- as_infolist(net_named)
  expect_type(out, "list")
  expect_equal(out$name, "Adolescents")
  expect_equal(out$year, 1994)
})

test_that("net_attributes works", {
  out <- net_attributes(net_named)
  expect_type(out, "character")
  expect_true("name" %in% out)
  expect_true("year" %in% out)
  expect_type(net_attributes(as_matrix(ison_adolescents)), "character")
})

test_that("tie_is_parallel marks ties that coexist on a pair of nodes", {
  # The Koenigsberg bridges are the classic case: two distinct bridges join
  # the same pair of banks at the same moment, so all four of the bridges in
  # the two bundles are parallel, and not just the second of each pair.
  out <- tie_is_parallel(ison_koenigsberg)
  expect_type(as.vector(out), "logical")
  expect_length(out, net_ties(ison_koenigsberg))
  expect_equal(sum(out), 4)
  expect_equal(unname(which(as.logical(out))), c(1, 2, 5, 6))
  # Every class the network can be held in agrees.
  expect_equal(sum(tie_is_parallel(as_igraph(ison_koenigsberg))), 4)
  expect_equal(sum(tie_is_parallel(as_tidygraph(ison_koenigsberg))), 4)
  expect_equal(sum(tie_is_parallel(as_stocnet(ison_koenigsberg))), 4)
  # A simple network has no parallel ties, and a network without ties is not
  # an error.
  expect_false(any(tie_is_parallel(ison_adolescents)))
  expect_length(tie_is_parallel(create_empty(3)), 0)
})

test_that("tie_is_parallel does not mark ties recorded at different moments", {
  # A panel re-states its ties at every wave, so a tie observed in two waves
  # follows itself rather than runs alongside itself. See #158.
  expect_false(any(tie_is_parallel(fict_potter)))
  expect_false(any(tie_is_parallel(ison_fraternity)))
  # Spells that abut, one beginning in the year the other ends, are
  # consecutive. Every repeated dyad in irps_wwi is of this kind.
  expect_false(any(tie_is_parallel(irps_wwi)))
  # Two events on one pair of nodes with one timestamp do coexist, though.
  expect_equal(sum(tie_is_parallel(irps_nuclear)), 16)
  # Where a network records no time at all, repeated ties are parallel.
  expect_equal(sum(tie_is_parallel(irps_blogs)), 130)
})

test_that("tie_is_parallel does not mark ties of different types", {
  # Several types of tie between a pair of nodes is what is_multiplex()
  # marks, so counting them here would report the same thing twice.
  expect_true(is_multiplex(ison_monks))
  expect_false(any(tie_is_parallel(ison_monks)))
  expect_false(any(tie_is_parallel(ison_bankwiring)))
  expect_false(any(tie_is_parallel(ison_tailorshop)))
  expect_false(any(tie_is_parallel(fict_actually)))
})

test_that("describe_ties counts the parallel ties", {
  expect_match(describe_ties(ison_koenigsberg), "\\(4 parallel\\)")
  expect_no_match(describe_ties(ison_adolescents), "parallel")
  expect_no_match(describe_ties(ison_monks), "parallel")
})

test_that("tie_attribute and node_attribute return every attribute where none is named", {
  # `igraph::edge_attr()` and `igraph::vertex_attr()` do this, so the other
  # classes' methods do too.
  expect_type(tie_attribute(irps_nuclear), "list")
  expect_equal(names(tie_attribute(irps_nuclear)),
               names(tie_attribute(as_igraph(irps_nuclear))))
  expect_equal(names(node_attribute(as_igraph(irps_nuclear))),
               net_node_attributes(as_igraph(irps_nuclear)))
  # 'from' and 'to' identify a tie rather than describe it, so a stocnet
  # object reports the attributes alone.
  expect_false(any(c("from", "to") %in% names(tie_attribute(irps_nuclear))))
  expect_equal(names(tie_attribute(as_network(irps_nuclear))),
               net_tie_attributes(as_network(irps_nuclear)))
})

test_that("a mark inside filter_ties reads the filtered network", {
  # `filter_ties()` on an igraph object sets a tidygraph context, which names
  # a nearer network than any context an outer stocnet call has stored.
  like <- to_uniplex(ison_monks, "like")
  seen <- NULL
  count_ties <- function() {
    seen <<- as.integer(net_ties(expect_ties()))
    TRUE
  }
  wave1 <- filter_ties(as_igraph(like), time == 1)
  expect_lt(net_ties(wave1), net_ties(like))
  mutate_ties(like, all = {
    filter_ties(wave1, count_ties())
    TRUE
  })
  expect_equal(seen, as.integer(net_ties(wave1)))
})
