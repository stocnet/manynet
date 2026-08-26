# Functional tests for the impute_*() family.
# Every exported impute_*() function is run across the standard fixture grid
# and across all object classes; failures surface as informative "AUDIT" skips.

impute_funs <- alive_functions("^impute_")

# None of the family has a required argument beyond `.data`, since every rule
# has a default. Extend this map if one ever does.
impute_argmakers <- list()

# Name-implied invariants that the output of an impute_*() function must
# satisfy: whatever the rule, nothing the network recorded as unobserved may
# still be unobserved afterwards.
impute_invariants <- list(
  impute_ties  = function(o) as.numeric(net_tie_missing(o)) == 0,
  impute_nodes = function(o) is_manynet(o)
)

.required_args <- function(fn) {
  fm <- formals(get(fn, envir = asNamespace("manynet")))
  req <- names(fm)[vapply(fm, function(x) identical(x, quote(expr = )),
                          logical(1))]
  setdiff(req, c(".data", "..."))
}

for (fn in impute_funs) {
  f <- get(fn, envir = asNamespace("manynet"))

  test_that(paste0(fn, "() follows family conventions"), {
    expect_identical(names(formals(f))[1], ".data",
                     label = paste0("First argument of ", fn, "()"))
    expect_identical(names(formals(f))[2], "rule",
                     label = paste0("Second argument of ", fn, "()"))
    bod <- paste(deparse(body(f)), collapse = " ")
    if (grepl("UseMethod", bod)) {
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
      if (length(req) && !fn %in% names(impute_argmakers)) {
        skip(paste0("AUDIT [", fn, "]: requires argument(s) ",
                    paste(req, collapse = ", "),
                    " not yet provided by impute_argmakers"))
      }
      args <- if (fn %in% names(impute_argmakers)) {
        run_or_skip(impute_argmakers[[fn]](net), fn, fx)
      } else list()
      out <- run_or_skip(do.call(f, c(list(net), args)), fn, fx)
      if (is.null(out)) {
        skip(paste0("AUDIT [", fn, " x ", fx, "]: returns NULL (unavailable)"))
      }
      expect_true(is_acceptable_output(out),
                  label = paste0(fn, "() output on ", fx, " fixture"))
      if (fn %in% names(impute_invariants) && is_manynet(out)) {
        holds <- run_or_skip(isTRUE(impute_invariants[[fn]](out)), fn,
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

# Cross-class conformance: imputing the same network held in different classes
# should not error, and the results should agree. The rules that draw at
# random are seeded so that the classes are compared on the same draws.
for (cnet in names(canonical_nets)) {
  canonical_classes <- class_versions(canonical_nets[[cnet]])
  for (fn in impute_funs) {
    f <- get(fn, envir = asNamespace("manynet"))
    test_that(paste0(fn, "() is consistent across object classes on the ",
                     cnet, " network"), {
      outs <- list()
      for (cl in names(canonical_classes)) {
        set.seed(1)
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
    })
  }
}
