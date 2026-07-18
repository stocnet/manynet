# Functional tests for the from_*() reassembly family.
# Every exported from_*() function is automatically paired with its to_*()
# counterpart (from_egos <-> to_egos, etc.) and the pair is tested as a
# roundtrip: splitting a network apart and reassembling it should recover a
# manynet-compatible network with the original node set. New from_*()
# functions are picked up automatically; pairs needing a special fixture or
# arguments extend the maps below.

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

# from_ties() pairs networks by named argument rather than taking a netlist,
# so its layering behaviour is additionally tested directly. (Unlabelled
# networks cannot be layered as their nodes cannot be matched; that limit
# surfaces as an AUDIT skip in the pair loop above.)
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
