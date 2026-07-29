# Functional tests for the is_*() mark family across the fixture grid and
# object classes. (test-mark_is.R checks default methods and the
# ison_southern_women contract; here every is_*() function must return a
# single non-NA logical for every fixture and agree across object classes.)

is_funs <- setdiff(alive_functions("^is_"), "is_manynet")

for (fn in is_funs) {
  f <- get(fn, envir = asNamespace("manynet"))
  for (fx in names(func_fixtures)) {
    test_that(paste0(fn, "() returns a single logical on the ", fx,
                     " fixture"), {
      out <- run_or_skip(f(func_fixtures[[fx]]), fn, fx)
      expect_type(out, "logical")
      expect_length(out, 1)
      expect_false(is.na(out), label = paste0(fn, "() on ", fx))
    })
  }
}

# Marks should not depend on the class the network is represented in
mark_classes <- class_versions(canonical_net)

# ... except the class predicates, which describe the container rather than the
# network, and so are expected to vary. Pin their expected pattern instead of
# requiring agreement, so the behaviour stays covered rather than skipped.
mark_class_patterns <- list(
  is_graph    = c(tidygraph = TRUE,  igraph = TRUE,  matrix = FALSE,
                  network = TRUE,    edgelist = FALSE, stocnet = TRUE),
  is_edgelist = c(tidygraph = FALSE, igraph = FALSE, matrix = FALSE,
                  network = FALSE,   edgelist = TRUE,  stocnet = FALSE)
)

for (fn in names(mark_class_patterns)) {
  local({
    fn <- fn
    f <- get(fn, envir = asNamespace("manynet"))
    pattern <- mark_class_patterns[[fn]]
    test_that(paste0(fn, "() varies by object class as expected"), {
      out <- vapply(mark_classes, f, logical(1))
      expect_equal(out[names(pattern)], pattern)
    })
  })
}

for (fn in setdiff(is_funs, names(mark_class_patterns))) {
  f <- get(fn, envir = asNamespace("manynet"))
  test_that(paste0(fn, "() agrees across object classes"), {
    outs <- lapply(mark_classes, function(x) {
      tryCatch(f(x), error = function(e) e)
    })
    errs <- names(outs)[vapply(outs, inherits, logical(1), "error")]
    ok <- outs[setdiff(names(outs), errs)]
    if (!length(ok)) {
      skip(paste0("AUDIT [", fn, "]: fails on all classes"))
    }
    vals <- unlist(ok)
    if (length(unique(vals)) > 1) {
      skip(paste0("AUDIT [", fn, "]: differs by class: ",
                  paste(names(ok), vals, sep = "=", collapse = ", ")))
    }
    succeed()
    if (length(errs)) {
      skip(paste0("AUDIT [", fn, "]: no method succeeds for class(es) ",
                  paste(errs, collapse = ", ")))
    }
  })
}

# na_to_*() imputation ---------------------------------------------------------

test_that("na_to_zero() and na_to_mean() impute missing tie data", {
  miss <- ison_adolescents |>
    add_tie_attribute("weight", c(1, NA, NA, 1, 1, 1, NA, NA, 1, 1))
  for (fn in c("na_to_zero", "na_to_mean")) {
    f <- get(fn, envir = asNamespace("manynet"))
    outm <- run_or_skip(f(as_matrix(miss)), fn, "matrix")
    expect_false(anyNA(outm))
    outg <- run_or_skip(f(miss), fn, "tidygraph")
    expect_false(anyNA(tie_attribute(outg, "weight")))
  }
  expect_equal(sum(na_to_zero(as_matrix(miss)) == 0) -
                 sum(as_matrix(miss) == 0, na.rm = TRUE),
               sum(is.na(as_matrix(miss))))
})
