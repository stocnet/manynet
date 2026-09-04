test_that("table_data returns a tibble with expected columns", {
  result <- table_data(pkg = "manynet")
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("dataset", "nodes", "ties", "directed") %in% names(result)))
})
test_that("the converted networks are stocnet objects that validate", {
  # fict_marvel, irps_blogs, irps_nuclear, and ison_koenigsberg were 'mnet'
  # objects until the parallel ties in each were marked. See #158.
  for(nm in c("fict_marvel", "irps_blogs", "irps_nuclear", "ison_koenigsberg")){
    x <- get(nm)
    expect_s3_class(x, "stocnet")
    expect_no_error(validate_stocnet(x))
    # Every tie is recorded, and none is left with an unknown value that
    # manynet would read as a missing tie.
    expect_equal(net_tie_missing(x), 0)
    if("weight" %in% names(x$ties)) expect_false(anyNA(x$ties$weight))
  }
})

test_that("the converted networks carry their collection metadata", {
  expect_equal(as_infolist(ison_koenigsberg)$location, "Koenigsberg, Prussia")
  expect_equal(as_infolist(ison_koenigsberg)$date, 1735)
  expect_equal(as_infolist(irps_blogs)$boundary, "snowball")
  expect_equal(as_infolist(irps_blogs)$doi, "10.1145/1134271.1134277")
  expect_equal(as_infolist(irps_nuclear)$observation, "event")
  expect_equal(as_infolist(irps_nuclear)$update, "increment")
  expect_equal(as_infolist(fict_marvel)$layers, c("relationship", "affiliation"))
  expect_equal(as_infolist(fict_marvel)$modes, c("characters", "teams"))
})

test_that("a stocnet keeps its mode names through a coercion to igraph", {
  # 'nodes' was the name an mnet gave the modes, so reading only that name
  # lost the modes of every network converted to a stocnet.
  expect_equal(mode_names(as_igraph(fict_marvel)), c("characters", "teams"))
  expect_equal(mode_names(as_igraph(irps_nuclear)), c("speakers", "concepts"))
  # to_multilevel() moves the modes into 'lvl', and as_stocnet() moves them
  # back onto 'mode' under their own names, losing no tie on the way.
  out <- as_stocnet(to_multilevel(as_igraph(fict_marvel)))
  expect_setequal(out$nodes$mode, c("characters", "teams"))
  expect_equal(net_ties(out), net_ties(fict_marvel))
  expect_equal(net_tie_missing(out), 0)
  expect_true(is_multilevel(out))
})

test_that("irps_nuclear records each claim's valence as a sign", {
  # A claim is supportive or critical, which the reserved 'weight' column
  # holds as 1 or -1.
  expect_true(is_signed(irps_nuclear))
  expect_false(is_weighted(irps_nuclear))
  expect_setequal(unique(irps_nuclear$ties$weight), c(1, -1))
  # The claims are a stream of events, not a panel.
  expect_true(is_dynamic(irps_nuclear))
  expect_false(is_longitudinal(irps_nuclear))
})

test_that("describe_network reports a multilevel network as multilevel", {
  # A multilevel network has more than one mode, so "multilevel" says
  # everything "two-mode" says and more.
  expect_match(describe_network(fict_marvel), "multilevel network")
  expect_no_match(describe_network(fict_marvel), "two-mode")
  expect_match(describe_network(ison_southern_women), "two-mode network")
  expect_no_match(describe_network(ison_southern_women), "multilevel")
  # fict_marvel's two layers are its two levels, one within the characters
  # and one between the characters and the teams, so it is multiplex only
  # because it is multilevel and the word is not repeated.
  expect_true(is_multiplex(fict_marvel))
  expect_true(is_twomode(fict_marvel))
  expect_no_match(describe_network(fict_marvel), "multiplex")
  # fict_actually runs four of its five layers within the characters, so
  # being multiplex says something that being multilevel does not.
  expect_match(describe_network(fict_actually), "multiplex, multilevel network")
})

test_that("layers are levels only where each layer holds one position", {
  expect_true(.layers_are_levels(fict_marvel))
  expect_true(.layers_are_levels(as_igraph(fict_marvel)))
  expect_false(.layers_are_levels(fict_actually))
  # A network that is not multilevel has no levels for its layers to be.
  expect_false(.layers_are_levels(ison_monks))
  expect_false(.layers_are_levels(ison_southern_women))
  expect_false(.layers_are_levels(ison_adolescents))
  # Each of fict_marvel's ties sits within the characters or between levels.
  expect_setequal(.tie_positions(fict_marvel),
                  c("characters", "between"))
  expect_null(.tie_positions(ison_adolescents))
})
