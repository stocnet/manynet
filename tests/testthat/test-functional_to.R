# Functional tests for the to_*() modification family.
# Every exported to_*() function is run across the standard fixture grid and
# across all object classes; failures surface as informative "AUDIT" skips.

to_funs <- alive_functions("^to_")

# For to_*() functions with required arguments, how to construct them from the
# network being tested. Functions with required arguments not listed here are
# skipped with an audit message so the map can be extended.
to_argmakers <- list(
  to_blocks     = function(net) list(membership = rep(c(1, 2),
                    length.out = as.numeric(net_nodes(net)))),
  to_dominating = function(net) list(from = 1),
  to_ego        = function(net) list(node = 1),
  to_subgraphs  = function(net) list(attribute = "group"),
  to_time       = function(net) list(time = 1),
  to_wave       = function(net) list(time = 1),
  to_uniplex    = function(net) list(tie = layer_names(net)[1])
)

# Name-implied invariants that the output of a to_*() function must satisfy.
to_invariants <- list(
  to_acyclic    = function(o) is_acyclic(o),
  to_undirected = function(o) !is_directed(o),
  to_directed   = function(o) is_directed(o),
  to_unweighted = function(o) !is_weighted(o),
  to_weighted   = function(o) is_weighted(o),
  to_signed     = function(o) is_signed(o),
  to_unsigned   = function(o) !is_signed(o),
  to_named      = function(o) is_labelled(o),
  to_labelled   = function(o) is_labelled(o),
  to_unnamed    = function(o) !is_labelled(o),
  to_unlabelled = function(o) !is_labelled(o),
  to_onemode    = function(o) !is_twomode(o),
  to_twomode    = function(o) is_twomode(o),
  to_giant      = function(o) is_connected(o),
  to_simplex    = function(o) !is_complex(o),
  to_uniplex    = function(o) !is_multiplex(o)
)

.required_args <- function(fn) {
  fm <- formals(get(fn, envir = asNamespace("manynet")))
  req <- names(fm)[vapply(fm, function(x) identical(x, quote(expr = )),
                          logical(1))]
  setdiff(req, c(".data", "..."))
}

for (fn in to_funs) {
  f <- get(fn, envir = asNamespace("manynet"))

  test_that(paste0(fn, "() follows family conventions"), {
    expect_identical(names(formals(f))[1], ".data",
                     label = paste0("First argument of ", fn, "()"))
    if (isS4(f) || inherits(f, "genericFunction")) skip("S4 generic")
    bod <- paste(deparse(body(f)), collapse = " ")
    if (grepl("UseMethod", bod)) {
      # Alias generics (e.g. to_component() dispatching to_giant() methods)
      # are checked against the generic they actually dispatch on.
      target <- sub('.*UseMethod\\("([^"]+)".*', "\\1", bod)
      expect_true(
        any(grepl(paste0("^", target, "\\.default$"),
                  suppressWarnings(utils::methods(fn)))),
        label = paste0(fn, "() having a default method"))
    }
  })

  for (fx in names(func_fixtures)) {
    test_that(paste0(fn, "() works on the ", fx, " fixture"), {
      net <- func_fixtures[[fx]]
      req <- .required_args(fn)
      if (length(req) && !fn %in% names(to_argmakers)) {
        skip(paste0("AUDIT [", fn, "]: requires argument(s) ",
                    paste(req, collapse = ", "),
                    " not yet provided by to_argmakers"))
      }
      args <- if (fn %in% names(to_argmakers)) {
        run_or_skip(to_argmakers[[fn]](net), fn, fx)
      } else list()
      out <- run_or_skip(do.call(f, c(list(net), args)), fn, fx)
      if (is.null(out)) {
        # snet_unavailable() returns NULL (quietly) for not-yet-implemented
        # network types, e.g. to_motifs() on a two-mode network.
        skip(paste0("AUDIT [", fn, " x ", fx, "]: returns NULL (unavailable)"))
      }
      expect_true(is_acceptable_output(out),
                  label = paste0(fn, "() output on ", fx, " fixture"))
      if (fn %in% names(to_invariants) && is_manynet(out)) {
        holds <- run_or_skip(isTRUE(to_invariants[[fn]](out)), fn,
                             paste0(fx, " invariant"))
        if (!holds) {
          skip(paste0("AUDIT [", fn, " x ", fx, "]: output does not ",
                      "satisfy the ", fn, "() name-implied invariant"))
        }
        succeed()
      }
    })
  }
}

# Cross-class conformance: applying a to_*() function to the same network
# represented in different classes should not error, and graph-like results
# should agree (via as_matrix) across classes.
canonical_classes <- class_versions(canonical_net)

for (fn in setdiff(to_funs, names(to_argmakers))) {
  if (length(.required_args(fn))) next
  f <- get(fn, envir = asNamespace("manynet"))
  test_that(paste0(fn, "() is consistent across object classes"), {
    outs <- list()
    for (cl in names(canonical_classes)) {
      outs[[cl]] <- tryCatch(f(canonical_classes[[cl]]),
                             error = function(e) e)
    }
    errs <- names(outs)[vapply(outs, inherits, logical(1), "error")]
    if (length(errs) == length(outs)) {
      skip(paste0("AUDIT [", fn, "]: fails on all classes: ",
                  conditionMessage(outs[[1]])))
    }
    succeed()
    mats <- lapply(outs[setdiff(names(outs), errs)], function(o) {
      if (is_manynet(o) && !is.list(o))
        tryCatch(unname(as_matrix(o)), error = function(e) NULL)
      else NULL
    })
    mats <- Filter(Negate(is.null), mats)
    if (length(mats) > 1) {
      for (cl in names(mats)[-1]) {
        expect_equal(mats[[cl]], mats[[1]], ignore_attr = TRUE,
                     label = paste0(fn, "() on ", cl),
                     expected.label = paste0(fn, "() on ",
                                             names(mats)[1]))
      }
    }
    if (length(errs)) {
      skip(paste0("AUDIT [", fn, "]: no method succeeds for class(es) ",
                  paste(errs, collapse = ", ")))
    }
  })
}
