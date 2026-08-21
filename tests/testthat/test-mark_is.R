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
