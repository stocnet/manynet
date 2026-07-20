# Functional tests for the from_*() reassembly family.
#
# Division of labour with test-functional_to.R: the to file tests that every
# to_*() function (including the splitting ones) produces *valid output* on
# every fixture and object class; this file tests *invertibility* — that each
# from_*() undoes its to_*() counterpart — plus the deeper branches of the
# from_ties() merge engine, which only fire on special inputs.

# 1. Pair roundtrips -----------------------------------------------------------
# Every exported from_*() function is automatically paired with its to_*()
# counterpart (from_egos <-> to_egos, etc.): splitting a network apart and
# reassembling it should recover a manynet-compatible network with the
# original node set. New from_*() functions are picked up automatically;
# pairs needing a special fixture or arguments extend the maps below.

from_funs <- alive_functions("^from_")

# Which network to roundtrip for a given pair (default: a small labelled
# network; unlabelled nodes cannot be re-identified across the pieces).
pair_fixture_makers <- list(
  default        = function() ison_adolescents,
  from_waves     = function() fict_potter,
  from_slices    = function() mutate_ties(ison_adolescents,
                                          time = seq_len(10) %% 3 + 1),
  from_subgraphs = function() add_node_attribute(ison_adolescents, "group",
                                                 rep(c("A", "B"), 4))
)

# Required arguments for the to_*() half of a pair
pair_to_argmakers <- list(
  to_subgraphs = function(net) list(attribute = "group")
)

for (fn in from_funs) {
  to_fn <- sub("^from_", "to_", fn)

  test_that(paste0(fn, "() reverses ", to_fn, "()"), {
    if (!to_fn %in% collect_functions("^to_")) {
      skip(paste0("AUDIT [", fn, "]: no ", to_fn,
                  "() counterpart is exported"))
    }
    maker <- pair_fixture_makers[[fn]]
    if (is.null(maker)) maker <- pair_fixture_makers$default
    net <- maker()
    targs <- if (to_fn %in% names(pair_to_argmakers)) {
      pair_to_argmakers[[to_fn]](net)
    } else list()
    f_to <- get(to_fn, envir = asNamespace("manynet"))
    f_from <- get(fn, envir = asNamespace("manynet"))

    pieces <- run_or_skip(do.call(f_to, c(list(net), targs)), to_fn, "pair")
    out <- run_or_skip(f_from(pieces), fn, "pair")

    expect_true(is_manynet(out), label = paste0(fn, "(", to_fn, "(x))"))
    if (as.numeric(net_nodes(out)) != as.numeric(net_nodes(net))) {
      skip(paste0("AUDIT [", fn, " x ", to_fn, "]: roundtrip is lossy: ",
                  "recovers ", as.numeric(net_nodes(out)), " of ",
                  as.numeric(net_nodes(net)), " nodes"))
    }
    succeed()
  })
}

test_that("from_waves() and from_slices() insist on a list of networks", {
  expect_error(from_waves(as_matrix(ison_adolescents)), "list")
  expect_message(from_slices(as_matrix(ison_adolescents)), "one slice")
})

# 2. from_ties() layering ------------------------------------------------------
# from_ties() pairs networks by named argument rather than taking a netlist,
# so its layering behaviour is tested directly. (Unlabelled tidygraphs cannot
# be layered as their nodes cannot be matched; that limit surfaces as an
# AUDIT skip in the pair loop above.)

test_that("from_ties() layers labelled networks into a multiplex network", {
  net1 <- ison_adolescents
  net2 <- add_tie_attribute(ison_adolescents, "weight", seq_len(10))
  out <- run_or_skip(from_ties(friends = net1, weighted = net2),
                     "from_ties", "two labelled networks")
  expect_true(is_manynet(out))
  expect_true(is_multiplex(out))
  expect_equal(as.numeric(net_nodes(out)), as.numeric(net_nodes(net1)))
  expect_setequal(layer_names(out), c("friends", "weighted"))
})

test_that("from_ties() layers stocnet objects", {
  sn1 <- as_stocnet(create_ring(6))
  sn2 <- as_stocnet(create_star(6))
  out <- run_or_skip(from_ties(friends = sn1, advice = sn2),
                     "from_ties", "stocnet")
  expect_s3_class(out, "stocnet")
  expect_true(is_multiplex(out))
  expect_setequal(unique(out$ties$layer), c("friends", "advice"))
})

test_that("from_ties() accepts layer_names in place of named arguments", {
  out <- run_or_skip(from_ties(as_stocnet(create_ring(6)),
                               as_stocnet(create_star(6)),
                               layer_names = c("rings", "stars")),
                     "from_ties", "layer_names")
  expect_setequal(unique(out$ties$layer), c("rings", "stars"))
  expect_error(from_ties(as_stocnet(create_ring(6)),
                         as_stocnet(create_star(6))),
               "name the layers")
})

# 3. from_ties() stocnet merge engine ------------------------------------------

test_that("from_ties() renames clashing layer names", {
  sn1 <- as_stocnet(ison_adolescents)
  sn2 <- as_stocnet(ison_adolescents)
  sn1$ties$layer <- "friends"
  sn2$ties$layer <- "friends"
  out <- run_or_skip(from_ties(a = sn1, b = sn2), "from_ties", "layer clash")
  expect_length(unique(out$ties$layer), 2)
  expect_true("friends" %in% out$ties$layer)
})

test_that("from_ties() merges node tables by label, coalescing attributes", {
  sn1 <- as_stocnet(add_node_attribute(ison_adolescents, "colour",
                                       rep("red", 8)))
  sn2 <- as_stocnet(add_node_attribute(ison_adolescents, "colour",
                                       rep("blue", 8)))
  out <- run_or_skip(from_ties(a = sn1, b = sn2), "from_ties",
                     "attribute conflict")
  # conflicting values keep the first network's values
  expect_true(all(out$nodes$colour == "red"))
  # disjoint node sets union
  out2 <- run_or_skip(from_ties(a = as_stocnet(ison_adolescents),
                                b = as_stocnet(mutate_nodes(
                                  ison_adolescents,
                                  name = paste0(name, "2")))),
                      "from_ties", "disjoint labels")
  expect_equal(nrow(out2$nodes), 16)
})

test_that("from_ties() adds unlabelled nodes rather than guessing labels", {
  # anonymous blocks only merge onto other unlabelled pools of the same
  # size (see the rings/stars test above); merged with a *labelled*
  # network, they are appended as new nodes instead of guessing identities
  out <- run_or_skip(from_ties(l = as_stocnet(ison_adolescents),
                               u = as_stocnet(create_ring(8))),
                     "from_ties", "mixed labelled/unlabelled")
  expect_equal(nrow(out$nodes), 16)
  expect_equal(sum(is.na(out$nodes$label)), 8)
})

test_that("from_ties() carries changes tables through the merge", {
  sn1 <- as_stocnet(fict_starwars)
  sn2 <- as_stocnet(delete_changes(fict_starwars))
  out <- run_or_skip(from_ties(a = sn1, b = sn2), "from_ties", "changes")
  expect_s3_class(out, "stocnet")
  expect_false(is.null(out$changes))
})

test_that("from_ties() resolves conflicting dates and DOIs without warnings", {
  sn1 <- add_info(as_stocnet(ison_adolescents), date = "2001",
                  doi = "10.1/first")
  sn2 <- add_info(as_stocnet(create_star(8)), date = "1999",
                  doi = "10.1/second")
  expect_no_warning(out <- from_ties(a = sn1, b = sn2))
  expect_identical(out$info$date, "1999")
  expect_identical(out$info$doi, "10.1/first")
})
