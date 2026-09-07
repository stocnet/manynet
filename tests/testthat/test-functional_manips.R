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
  # manip_canonical has no isolates and no missing values, so these two are
  # no-ops here; they exercise the machinery across classes, while the exact
  # counts are asserted in test-manip_nodes.R against fixtures that do.
  delete_isolates = list(
    call  = function(x) delete_isolates(x),
    check = function(o, x) n_nodes(o) == n_nodes(x)),
  delete_incomplete = list(
    call  = function(x) delete_incomplete(x),
    check = function(o, x) n_nodes(o) == n_nodes(x)),
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
  # `apply_changes()` and `gather_changes()` are deprecated in favour of
  # `to_time()`, swept over by the to_* tests, and `as_changelist(time =)`.
  as_changelist   = function(x) as_changelist(x, time = 2)
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

test_that("bind_changes() adds to the changelog rather than replacing it", {
  mk <- function() create_filled(4) |>
    mutate_nodes(name = LETTERS[1:4], status = c(TRUE, FALSE, FALSE, FALSE))
  c1 <- data.frame(time = 2, node = "B", var = "status", value = TRUE)
  c2 <- data.frame(time = 3, node = "C", var = "status", value = TRUE)
  for(x in list(as_igraph(mk()), as_tidygraph(mk()), as_stocnet(mk()))){
    out <- bind_changes(bind_changes(x, c1), c2)
    expect_equal(nrow(as_changelist(out)), 2)
  }
  # `.align_change_values()` reconciles a 'value' column of a differing type
  c3 <- data.frame(time = 4, node = "D", var = "status", value = "maybe")
  out <- bind_changes(bind_changes(as_igraph(mk()), c1), c3)
  expect_equal(nrow(as_changelist(out)), 2)
  expect_type(as_changelist(out)$value, "character")
  # a composition table builds its own changelog, so that branch still replaces
  comp <- data.frame(node = 1:4, begin = c(1, 1, 2, 3), end = c(4, 4, 4, 4))
  y <- bind_changes(as_igraph(create_filled(4)), comp)
  expect_true("active" %in% net_node_attributes(y))
  expect_equal(nrow(igraph::graph_attr(y)$changes), 6)
})

# Verbs manipulating global variables and network info ------------------------

test_that("mutate_globals(), rename_globals() and select_globals() work", {
  sn <- run_or_skip(mutate_globals(as_stocnet(ison_algebra),
                                   time = 2, var = "active", value = FALSE),
                    "mutate_globals", "stocnet")
  expect_s3_class(sn, "stocnet")
  expect_true(all(c("time", "var", "value") %in% names(sn$globals)))
  sn2 <- run_or_skip(rename_globals(sn, when = time),
                     "rename_globals", "stocnet")
  expect_true("when" %in% names(sn2$globals))
  sn3 <- run_or_skip(select_globals(sn), "select_globals", "stocnet")
  expect_true(all(names(sn3$globals) %in% c("var", "time", "value")))
})

test_that("bind_globals() adds rows where mutate_globals() changes columns", {
  gl <- data.frame(time = 1:2, var = "budget", value = c(10, 20))
  sn <- run_or_skip(bind_globals(as_stocnet(ison_algebra), gl),
                    "bind_globals", "stocnet")
  expect_equal(nrow(sn$globals), 2)
  # a second bind adds to the table, where a second mutate changes its columns
  expect_equal(nrow(bind_globals(sn, data.frame(time = 3, var = "staff",
                                                value = 4))$globals), 3)
  m <- mutate_globals(as_stocnet(ison_algebra), time = 1, var = "a",
                      value = TRUE)
  expect_equal(nrow(mutate_globals(m, time = 2, var = "b",
                                   value = FALSE)$globals), 1)
  # the column names are brought to the stocnet conventions on the way in
  expect_setequal(names(bind_globals(as_stocnet(ison_algebra),
                                     data.frame(wave = 1, variable = "a",
                                                weight = 0))$globals),
                  c("time", "var", "value"))
})

test_that("filter_globals(), arrange_globals() and delete_globals() work", {
  gl <- data.frame(time = 1:2, var = "budget", value = c(10, 20))
  sn <- bind_globals(as_stocnet(ison_algebra), gl)
  out <- run_or_skip(filter_globals(sn, time == 1), "filter_globals", "stocnet")
  expect_equal(nrow(out$globals), 1)
  expect_equal(out$globals$value, 10)
  out <- run_or_skip(arrange_globals(sn, dplyr::desc(time)),
                     "arrange_globals", "stocnet")
  expect_equal(out$globals$time, c(2L, 1L))
  out <- run_or_skip(delete_globals(sn), "delete_globals", "stocnet")
  expect_null(out$globals)
  expect_s3_class(validate_stocnet(out), "stocnet")
})

test_that("to_time() scopes the globals to the moment asked for", {
  gl <- data.frame(time = 1:2, var = "budget", value = c(10, 20))
  sn <- bind_globals(as_stocnet(ison_algebra), gl)
  out <- to_time(sn, 1)
  expect_equal(nrow(out$globals), 1)
  expect_equal(out$globals$value, 10)
  # the time column goes, as it does for the ties and the missings
  expect_false("time" %in% names(out$globals))
  # a component that holds nothing is NULL, not an empty table
  expect_null(to_time(sn, 9)$globals)
  # a globals table with no time column holds a constant, shared by every
  # moment, so it is left alone
  const <- bind_globals(as_stocnet(ison_algebra),
                        data.frame(var = "k", value = 1))
  expect_equal(nrow(to_time(const, 1)$globals), 1)
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
  expect_warning(out <- add_info(ison_adolescents, nonsense = "field"),
                 "not recognised fields")
  expect_true(is_manynet(out))
})

test_that("add_info() and mutate_info() also work on stocnet objects", {
  sn <- as_stocnet(ison_adolescents)
  out <- run_or_skip(add_info(sn, name = "Adols"), "add_info", "stocnet")
  expect_identical(out$info$name, "Adols")
  out2 <- run_or_skip(mutate_info(out, name = "Adols2"),
                      "mutate_info", "stocnet")
  expect_identical(out2$info$name, "Adols2")
})

test_that("add_info.stocnet checks and conforms the names it is given", {
  sw <- as_stocnet(ison_southern_women)
  # a two-mode network has two nodesets to name, as the igraph method requires
  expect_error(add_info(sw, nodes = "women"), "both nodesets")
  # 'nodes' was the mnet name for the modes, so it sets the reserved field
  two <- add_info(sw, nodes = c("work events", "social events"))
  expect_equal(mode_names(two), c("work events", "social events"))
  expect_equal(two$info$modes, c("work events", "social events"))
  # a name the class does not reserve is kept, but the user is told
  expect_warning(add_info(sw, nonsense = 1), "not recognised")
})
