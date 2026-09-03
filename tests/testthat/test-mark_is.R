# What the is_*() sweep in test-functional_marks.R cannot assert: what tells
# apart the marks that a single fixture, or a single object class, gives the
# same answer for.

test_that("is_multilevel distinguishes interlocking from plain two-mode networks", {
  # fict_marvel interlocks a signed one-mode layer among its characters with a
  # two-mode layer of affiliations to their teams; fict_actually does the same
  # with a smaller one-mode layer. ison_southern_women is two-mode but has no
  # ties within either mode, so it is not multilevel.
  expect_true(is_multilevel(fict_marvel))
  expect_true(is_multilevel(fict_actually))
  expect_false(is_multilevel(ison_southern_women))
  expect_false(is_multilevel(irps_revere))
  # One-mode networks are never multilevel.
  expect_false(is_multilevel(ison_adolescents))
  # to_multilevel() records levels in 'lvl' and deletes 'type', so an igraph
  # is no longer two-mode and has to be recognised by its levels instead.
  # fict_marvel is a stocnet, which holds its levels in 'mode' either way,
  # so the igraph behaviour is tested on a coerced copy.
  expect_false(is_twomode(to_multilevel(as_igraph(fict_marvel))))
  expect_true(is_multilevel(to_multilevel(as_igraph(fict_marvel))))
  # A two-mode network without any ties cannot have ties within a mode.
  expect_false(is_multilevel(create_empty(c(3,3))))
})

test_that("is_multilevel and to_multilevel keep a stocnet's modes", {
  marvel <- as_stocnet(fict_marvel)
  women <- as_stocnet(ison_southern_women)
  expect_true(is_multilevel(marvel))
  expect_false(is_multilevel(women))
  expect_false(is_multilevel(as_stocnet(ison_adolescents)))
  expect_false(is_multilevel(as_stocnet(create_empty(c(3,3)))))
  # A 'stocnet' holds its levels in 'mode' and its ties table already allows
  # ties within a mode, so `to_multilevel()` has nothing to reformat.
  expect_identical(to_multilevel(marvel), marvel)
  expect_true(is_twomode(to_multilevel(marvel)))
  expect_equal(net_modes(to_multilevel(marvel)), 2)
  # A 'mode' variable can name more than two levels, unlike an igraph 'type'.
  three <- marvel
  three$nodes$mode[1:5] <- "third"
  expect_equal(net_modes(three), 3)
  expect_true(is_multilevel(three))
  # as_stocnet() maps the 'lvl' attribute that to_multilevel.igraph() writes
  # back onto 'mode', naming the levels from the network's info where it can,
  # so that a round trip through an igraph loses neither of the two modes.
  levelled <- as_stocnet(to_multilevel(as_igraph(fict_marvel)))
  expect_false("lvl" %in% names(levelled$nodes))
  expect_equal(net_modes(levelled), 2)
  expect_setequal(levelled$nodes$mode, marvel$nodes$mode)
  expect_true(is_multilevel(levelled))
})

test_that("is_longitudinal marks a stocnet object with waves", {
  # a stocnet object is a list, so without a method of its own it is marked
  # as if it were a list of networks
  expect_equal(is_longitudinal(as_stocnet(fict_starwars)),
               is_longitudinal(fict_starwars))
  expect_true(is_longitudinal(as_stocnet(fict_starwars)))
  expect_false(is_longitudinal(as_stocnet(ison_adolescents)))
})

test_that("is_connected respects connectivity", {
  # fict_starwars is weakly but not strongly connected
  expect_false(is_connected(fict_starwars))
  expect_false(is_connected(fict_starwars, connectivity = "strong"))
  expect_true(is_connected(fict_starwars, connectivity = "weak"))
  # the two notions coincide for undirected networks
  expect_true(is_connected(ison_adolescents))
  expect_true(is_connected(ison_adolescents, connectivity = "weak"))
  expect_true(is_connected(ison_adolescents, connectivity = "strong"))
  expect_error(is_connected(ison_adolescents, connectivity = "bloop"))
})

test_that("is_longitudinal does not mark a network whose ties carry no moment", {
  # A panel re-observes the ties, so the ties carry the stamp. A diffusion
  # records only how the nodes change, on a network whose ties never change,
  # so it is not a panel.
  diff <- play_diffusion(create_ring(12), seeds = 1)
  expect_true(is_changing(diff))
  expect_equal(net_tie_attributes(diff), character(0))
  expect_false(is_longitudinal(diff))
  # The panels still mark TRUE, whether or not they also record changes.
  expect_true(is_longitudinal(ison_monks))
  expect_true(is_longitudinal(ison_classmates))
  expect_true(is_longitudinal(fict_starwars))
})

# A network of interstate trade and of state membership in intergovernmental
# organisations: two modes, a directed within-level layer, an undirected
# between-level one. See #170 and #171.
trade_igos <- function(){
  make_stocnet(
    info = list(modes = c("states", "IGOs"),
                layers = c("trade", "membership"),
                directed = c(trade = TRUE, membership = FALSE)),
    nodes = tibble::tibble(label = c("a", "b", "x"),
                           mode = c("states", "states", "IGOs")),
    ties = tibble::tibble(from = c(1L, 2L, 1L), to = c(2L, 1L, 3L),
                          weight = c(5, 9, 1),
                          layer = c("trade", "trade", "membership"))
  )
}

test_that("is_directed marks a multilevel network by its layers", {
  net <- trade_igos()
  # two modes, but tied within a level as well as between
  expect_true(is_twomode(net))
  expect_true(is_multilevel(net))
  expect_true(is_directed(net))
  # and the same once coerced
  expect_true(is_directed(as_igraph(net)))
  expect_true(is_directed(as_tidygraph(net)))
  # a network tied only between its modes has no direction to report
  expect_false(is_directed(ison_southern_women))
  expect_false(is_directed(as_igraph(ison_southern_women)))
})

test_that("layer_is_directed reports each layer of a mixed network", {
  net <- trade_igos()
  expect_equal(layer_is_directed(net),
               c(trade = TRUE, membership = FALSE))
  expect_equal(layer_is_directed(as_igraph(net)),
               c(trade = TRUE, membership = FALSE))
  # a single layer can be asked about on its own
  expect_true(layer_is_directed(net, "trade"))
  expect_false(layer_is_directed(net, "membership"))
  # where a network records nothing per layer, it reports its own direction
  expect_equal(unname(layer_is_directed(ison_adolescents)), FALSE)
})
