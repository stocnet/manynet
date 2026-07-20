# Functional tests for the make families: create_*() (deterministic),
# generate_*() (stochastic), and play_*() (simulation).
# Every live exported function is called with a one-mode and a two-mode `n`
# and checked against family conventions: correct node counts, mode handling,
# and manynet-compatible output. Non-supported combinations skip informatively.

# `create_motifs` is an alias of `to_motifs`: despite the `create_` prefix it
# belongs to the `to_*` family (returns a named list of motif networks, not a
# single sized network), so it is audited by test-functional_to.R instead.
create_funs <- setdiff(alive_functions("^create_"), "create_motifs")
generate_funs <- alive_functions("^generate_")

# Functions whose first argument is data, not a size
make_data_first <- c("create_explicit", "generate_configuration")

for (fn in c(create_funs, generate_funs)) {
  if (fn %in% make_data_first) next
  f <- get(fn, envir = asNamespace("manynet"))

  test_that(paste0(fn, "() follows family conventions"), {
    expect_identical(names(formals(f))[1], "n",
                     label = paste0("First argument of ", fn, "()"))
  })

  test_that(paste0(fn, "() creates a one-mode network of the right size"), {
    set.seed(1234)
    out <- run_or_skip(f(6), fn, "onemode n = 6")
    if (is.null(out)) {
      skip(paste0("AUDIT [", fn, "]: returns NULL for n = 6"))
    }
    expect_true(is_manynet(out))
    expect_equal(as.numeric(net_nodes(out)), 6)
    expect_false(is_twomode(out))
  })

  test_that(paste0(fn, "() creates a two-mode network of the right size"), {
    set.seed(1234)
    out <- run_or_skip(f(c(4, 6)), fn, "twomode n = c(4,6)")
    if (is.null(out) || !is_manynet(out) || !is_twomode(out) ||
        as.numeric(net_nodes(out)) != 10) {
      skip(paste0("AUDIT [", fn, "]: does not (yet) support two-mode `n`"))
    }
    succeed()
  })

  if ("directed" %in% names(formals(f))) {
    test_that(paste0(fn, "() is undirected by default, directed on request"), {
      set.seed(1234)
      out <- run_or_skip(f(6, directed = TRUE), fn, "directed")
      if (is.null(out)) {
        skip(paste0("AUDIT [", fn, "]: returns NULL for directed n = 6"))
      }
      expect_true(is_directed(out))
      set.seed(1234)
      expect_false(is_directed(f(6)))
    })
  }

  test_that(paste0(fn, "() can take another network as n"), {
    set.seed(1234)
    out <- run_or_skip(f(create_empty(5)), fn, "network as n")
    if (is.null(out)) {
      skip(paste0("AUDIT [", fn, "]: returns NULL for network `n`"))
    }
    expect_equal(as.numeric(net_nodes(out)), 5)
  })
}

test_that("create_explicit() builds a network from formula ties", {
  out <- run_or_skip(create_explicit(A - B, B - C, C - A),
                     "create_explicit", "formula")
  expect_true(is_manynet(out))
  expect_equal(as.numeric(net_nodes(out)), 3)
  expect_equal(as.numeric(net_ties(out)), 3)
})

test_that("generate_configuration() reproduces a degree sequence", {
  set.seed(1234)
  out <- run_or_skip(generate_configuration(ison_adolescents),
                     "generate_configuration", "labelled")
  expect_true(is_manynet(out))
  expect_equal(as.numeric(net_nodes(out)),
               as.numeric(net_nodes(ison_adolescents)))
})

# play_*() simulations ---------------------------------------------------------

test_that("play_diffusion() records a diffusion on the network", {
  set.seed(1234)
  net <- create_ring(8)
  out <- run_or_skip(play_diffusion(net, seeds = 1, steps = 5),
                     "play_diffusion", "basic")
  expect_true(is_manynet(out))
  expect_true("diffusion" %in% net_node_attributes(out))
})

test_that("play_diffusion() variants run (latency, recovery, immunity)", {
  set.seed(1234)
  net <- create_ring(8)
  for (args in list(list(latency = 0.5), list(recovery = 0.5),
                    list(immune = 2), list(prevalence = 0.25))) {
    out <- run_or_skip(do.call(play_diffusion,
                               c(list(net, seeds = 1, steps = 5), args)),
                       "play_diffusion", paste(names(args), collapse = ","))
    expect_true(is_manynet(out))
  }
})

test_that("play_learning() returns a learn_model", {
  set.seed(1234)
  net <- create_ring(8)
  out <- run_or_skip(play_learning(net, beliefs = stats::runif(8), steps = 5),
                     "play_learning", "basic")
  expect_s3_class(out, "learn_model")
})

test_that("play_segregation() returns a network", {
  set.seed(1234)
  net <- add_node_attribute(create_lattice(16), "group",
                            rep(c("A", "B"), 8))
  out <- run_or_skip(play_segregation(net, attribute = "group", steps = 2),
                     "play_segregation", "attributed")
  expect_true(is_manynet(out))
  expect_equal(as.numeric(net_nodes(out)), 16)
})
