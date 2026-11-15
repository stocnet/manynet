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

test_that("a diffusion on ison_lawfirm and irps_usgeo does not depend on class", {
  # These two were 'mnet' objects until a diffusion on a stocnet could be
  # drawn (#186). The diffusion must give the same record on the stocnet as on
  # the tbl_graph that `as_tidygraph()` makes of it. Seed 2 is connected in both;
  # seed 1 of irps_usgeo is Alaska, an isolate.
  for(nm in c("ison_lawfirm", "irps_usgeo")){
    x <- get(nm)
    expect_s3_class(x, "stocnet")
    expect_no_error(validate_stocnet(x))
    old <- as_tidygraph(x)
    expect_s3_class(old, "tbl_graph")
    set.seed(186)
    on_tidygraph <- play_diffusion(old, seeds = 2, transmissibility = 0.5, steps = 5)
    set.seed(186)
    on_stocnet <- play_diffusion(x, seeds = 2, transmissibility = 0.5, steps = 5)
    expect_s3_class(on_stocnet, "stocnet")
    expect_gt(nrow(as_changelist(on_stocnet)), 1)
    expect_equal(as_changelist(on_stocnet), as_changelist(on_tidygraph))
  }
  expect_equal(as_infolist(ison_lawfirm)$layers, c("friends", "advice", "cowork"))
  expect_equal(as_infolist(irps_usgeo)$doi, "10.5539/cis.v10n1p54")
})

test_that("the last single mnet datasets are stocnet objects that validate", {
  # These six waited on downstream releases that no longer rely on the 'mnet'
  # class or on the 'type' tie attribute (#186).
  for(nm in c("ison_centrality", "ison_networkers", "irps_revere",
              "ison_adolescents", "fict_lotr", "ison_algebra")){
    x <- get(nm)
    expect_s3_class(x, "stocnet")
    expect_no_error(validate_stocnet(x))
  }
  # irps_revere's modes are named, not "FALSE" and "TRUE".
  expect_equal(as_infolist(irps_revere)$modes, c("members", "organisations"))
  expect_setequal(irps_revere$nodes$mode, c("members", "organisations"))
  expect_true(is_twomode(irps_revere))
  # ison_algebra's layers are in 'layer', not 'type'.
  expect_setequal(layer_names(ison_algebra), c("social", "tasks", "friends"))
  # The two-mode version of ison_centrality is one rename away.
  expect_true(is_twomode(rename_nodes(ison_centrality, mode = twomode_type)))
})

test_that("ison_physicians is one stocnet that splits into its four cities", {
  expect_s3_class(ison_physicians, "stocnet")
  expect_no_error(validate_stocnet(ison_physicians))
  expect_equal(c(net_nodes(ison_physicians)), 246)
  expect_equal(c(net_ties(ison_physicians)), 1099)
  expect_true(all(c("freetime", "discussions", "clubs", "friends", "proximity") %in%
                    net_node_attributes(ison_physicians)))
  expect_setequal(layer_names(ison_physicians),
                  c("advice", "discussion", "friendship"))
  cities <- to_subgraphs(ison_physicians, "city")
  expect_length(cities, 4)
  expect_equal(unname(vapply(cities, function(x) c(net_nodes(x)), numeric(1))),
               c(117, 50, 44, 35))
  expect_equal(unname(vapply(cities, function(x) c(net_ties(x)), numeric(1))),
               c(543, 211, 174, 171))
})
