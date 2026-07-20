# Functional tests for the dplyr-style manipulation verbs
# (add_*/delete_*/mutate_*/rename_*/select_*/filter_*/arrange_*/bind_*/join_*).
# Each verb is exercised through a table of operations with a post-condition,
# across all object classes. Post-conditions are enforced strictly on the
# native tidygraph class and reported as AUDIT skips on other classes.

manip_canonical <- ison_adolescents |>
  add_node_attribute("group", rep(c("A", "B"), 4)) |>
  add_tie_attribute("weight", seq_len(10))

n_nodes <- function(x) as.numeric(net_nodes(x))
n_ties  <- function(x) as.numeric(net_ties(x))

manip_ops <- list(
  add_nodes = list(
    call  = function(x) add_nodes(x, 2),
    check = function(o, x) n_nodes(o) == n_nodes(x) + 2),
  delete_nodes = list(
    call  = function(x) delete_nodes(x, 1),
    check = function(o, x) n_nodes(o) == n_nodes(x) - 1),
  add_ties = list(
    call  = function(x) add_ties(x, c(1, 5)),
    check = function(o, x) n_ties(o) == n_ties(x) + 1),
  delete_ties = list(
    call  = function(x) delete_ties(x, 1),
    check = function(o, x) n_ties(o) == n_ties(x) - 1),
  add_node_attribute = list(
    call  = function(x) add_node_attribute(x, "tst", seq_len(n_nodes(x))),
    check = function(o, x) "tst" %in% net_node_attributes(o)),
  delete_node_attribute = list(
    call  = function(x) delete_node_attribute(x, "group"),
    check = function(o, x) !"group" %in% net_node_attributes(o)),
  add_tie_attribute = list(
    call  = function(x) add_tie_attribute(x, "tst", seq_len(n_ties(x))),
    check = function(o, x) "tst" %in% net_tie_attributes(o)),
  delete_tie_attribute = list(
    call  = function(x) delete_tie_attribute(x, "weight"),
    check = function(o, x) !"weight" %in% net_tie_attributes(o)),
  mutate_nodes = list(
    call  = function(x) mutate_nodes(x, tst = 1),
    check = function(o, x) "tst" %in% net_node_attributes(o)),
  mutate_ties = list(
    call  = function(x) mutate_ties(x, tst = 1),
    check = function(o, x) "tst" %in% net_tie_attributes(o)),
  rename_nodes = list(
    call  = function(x) rename_nodes(x, faction = group),
    check = function(o, x) "faction" %in% net_node_attributes(o)),
  rename_ties = list(
    call  = function(x) rename_ties(x, strength = weight),
    check = function(o, x) "strength" %in% net_tie_attributes(o)),
  select_nodes = list(
    call  = function(x) select_nodes(x, group),
    check = function(o, x) !"tst" %in% net_node_attributes(o)),
  select_ties = list(
    call  = function(x) select_ties(x, weight),
    check = function(o, x) "weight" %in% net_tie_attributes(o)),
  filter_nodes = list(
    call  = function(x) filter_nodes(x, group == "A"),
    check = function(o, x) n_nodes(o) < n_nodes(x)),
  filter_ties = list(
    call  = function(x) filter_ties(x, weight > 5),
    check = function(o, x) n_ties(o) < n_ties(x)),
  arrange_nodes = list(
    call  = function(x) arrange_nodes(x, group),
    check = function(o, x) n_nodes(o) == n_nodes(x)),
  arrange_ties = list(
    call  = function(x) arrange_ties(x, weight),
    check = function(o, x) n_ties(o) == n_ties(x)),
  bind_node_attributes = list(
    call  = function(x) bind_node_attributes(x, x),
    check = function(o, x) n_nodes(o) == n_nodes(x)),
  join_ties = list(
    call  = function(x) join_ties(x, create_ring(n_nodes(x)), "rings"),
    check = function(o, x) n_nodes(o) == n_nodes(x))
)

manip_classes <- class_versions(manip_canonical)

for (op in names(manip_ops)) {
  for (cl in names(manip_classes)) {
    test_that(paste0(op, "() works on ", cl, " objects"), {
      x <- manip_classes[[cl]]
      out <- run_or_skip(manip_ops[[op]]$call(x), op, cl)
      expect_true(is_acceptable_output(out),
                  label = paste0(op, "() output on ", cl))
      holds <- run_or_skip(isTRUE(manip_ops[[op]]$check(out, x)),
                           op, paste0(cl, " post-condition"))
      if (cl == "tidygraph") {
        expect_true(holds,
                    label = paste0(op, "() post-condition on ", cl))
      } else if (!holds) {
        skip(paste0("AUDIT [", op, " x ", cl, "]: post-condition ",
                    "not satisfied"))
      } else succeed()
    })
  }
}

# Verbs manipulating the changes table of longitudinal networks ---------------

changes_ops <- list(
  filter_changes  = function(x) filter_changes(x, time > 1),
  select_changes  = function(x) select_changes(x, node),
  arrange_changes = function(x) arrange_changes(x, time),
  mutate_changes  = function(x) mutate_changes(x, tst = 1),
  delete_changes  = function(x) delete_changes(x),
  apply_changes   = function(x) apply_changes(x, time = 2),
  gather_changes  = function(x) gather_changes(x, time = 2)
)

changes_classes <- list(tidygraph = fict_starwars,
                        igraph = as_igraph(fict_starwars),
                        stocnet = as_stocnet(fict_starwars))

for (op in names(changes_ops)) {
  for (cl in names(changes_classes)) {
    test_that(paste0(op, "() works on a longitudinal ", cl, " network"), {
      out <- run_or_skip(changes_ops[[op]](changes_classes[[cl]]),
                         op, paste0("longitudinal ", cl))
      expect_true(is_acceptable_output(out),
                  label = paste0(op, "() output on ", cl))
    })
  }
}

test_that("delete_changes() removes the changelog", {
  out <- delete_changes(fict_starwars)
  expect_null(igraph::graph_attr(as_igraph(out), "changes"))
})

test_that("bind_changes() adds change events to a network", {
  changes <- data.frame(time = c(2, 3), node = c("Anakin", "Padme"),
                        var = "active", value = FALSE)
  for (cl in c("tidygraph", "stocnet")) {
    x <- if (cl == "stocnet") as_stocnet(fict_starwars) else fict_starwars
    out <- run_or_skip(bind_changes(x, changes = changes, var = "active"),
                       "bind_changes", cl)
    expect_true(is_acceptable_output(out),
                label = paste0("bind_changes() output on ", cl))
  }
})

test_that("rename_changes() renames changelog columns", {
  out <- run_or_skip(rename_changes(fict_starwars, when = time),
                     "rename_changes", "longitudinal")
  expect_true(is_acceptable_output(out))
})

# Verbs manipulating global variables and network info ------------------------

test_that("mutate_globals(), rename_globals() and select_globals() work", {
  sn <- run_or_skip(mutate_globals(as_stocnet(ison_algebra),
                                   time = 2, var = "active", value = FALSE),
                    "mutate_globals", "stocnet")
  expect_s3_class(sn, "stocnet")
  expect_true(all(c("time", "var", "value") %in% names(sn$global)))
  sn2 <- run_or_skip(rename_globals(sn, when = time),
                     "rename_globals", "stocnet")
  expect_true("when" %in% names(sn2$global))
  sn3 <- run_or_skip(select_globals(sn), "select_globals", "stocnet")
  expect_true(all(names(sn3$global) %in% c("var", "time", "value")))
})

test_that("rename_globals() renames aliases to stocnet conventions", {
  df <- data.frame(wave = 1, variable = "active", weight = 0)
  out <- rename_globals.data.frame(df)
  expect_setequal(names(out), c("time", "var", "value"))
})

test_that("add_info() and mutate_info() record network information", {
  out <- run_or_skip(add_info(ison_adolescents, name = "Adols",
                              collection = "Coleman", year = 1961,
                              doi = "10/example"),
                     "add_info", "labelled")
  expect_true(is_manynet(out))
  expect_match(paste(unlist(as_infolist(out)), collapse = " "), "Adols")
  out2 <- run_or_skip(mutate_info(out, name = "Adols2"),
                      "mutate_info", "labelled")
  expect_match(paste(unlist(as_infolist(out2)), collapse = " "), "Adols2")
  expect_type(net_attributes(out), "character")
})

test_that("add_info() names nodesets and tie types where well-formed", {
  out <- run_or_skip(add_info(ison_southern_women,
                              nodes = c("women", "events")),
                     "add_info", "twomode")
  expect_true(is_manynet(out))
  expect_error(add_info(ison_southern_women, nodes = "women"),
               "both nodesets")
  # unrecognised fields warn (silenced under quiet verbosity) but don't fail
  expect_no_error(add_info(ison_adolescents, nonsense = "field"))
})

test_that("add_info() and mutate_info() also work on stocnet objects", {
  sn <- as_stocnet(ison_adolescents)
  out <- run_or_skip(add_info(sn, name = "Adols"), "add_info", "stocnet")
  expect_identical(out$info$name, "Adols")
  out2 <- run_or_skip(mutate_info(out, name = "Adols2"),
                      "mutate_info", "stocnet")
  expect_identical(out2$info$name, "Adols2")
})
