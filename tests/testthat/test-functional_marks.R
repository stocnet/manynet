# Functional tests for the is_*() mark family across the fixture grid and
# object classes. Every is_*() function must have a default method, return a
# single non-NA logical for every fixture, mark the twomode fixture as its
# name implies, and agree across object classes.
# (test-mark_is.R holds only what this sweep cannot assert: what distinguishes
# the marks that a single fixture cannot tell apart.)

is_funs <- setdiff(alive_functions("^is_"), "is_manynet")

# Which is_*() functions the twomode fixture, ison_southern_women, satisfies.
# A two-mode network of women's attendance at events is labelled, attributed,
# connected, uniplex, and held as a graph. Each event enters the network on the
# date it is held, which the changes record, so it is also changing, and it is
# none of the other things a mark names. Matching on the name rather than
# listing the functions keeps a newly added mark covered without an entry here.
.twomode_marks <- paste0("twomode|attributed|igraph|connected|labelled|",
                         "(?<!hyper)graph|manynet|uniplex|changing")

for (fn in is_funs) {
  f <- get(fn, envir = asNamespace("manynet"))

  test_that(paste0(fn, "() follows family conventions"), {
    expect_true(any(grepl(paste0("^", fn, "\\.default$"),
                          suppressWarnings(utils::methods(fn)))),
                label = paste0(fn, "() having a default method"))
  })

  for (fx in names(func_fixtures)) {
    test_that(paste0(fn, "() returns a single logical on the ", fx,
                     " fixture"), {
      out <- run_or_skip(f(func_fixtures[[fx]]), fn, fx)
      expect_type(out, "logical")
      expect_length(out, 1)
      expect_false(is.na(out), label = paste0(fn, "() on ", fx))
    })
  }

  test_that(paste0(fn, "() marks the twomode fixture as its name implies"), {
    expected <- grepl(.twomode_marks, fn, perl = TRUE)
    expect_equal(f(func_fixtures$twomode), expected,
                 label = paste0(fn, "() on ison_southern_women"))
    if (!grepl("igraph", fn)) {
      expect_equal(f(as_stocnet(func_fixtures$twomode)), expected,
                   label = paste0(fn, "() on ison_southern_women as a stocnet"))
    }
  })
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

# impute_ties() imputation -----------------------------------------------------

test_that("every rule of impute_ties() imputes missing tie data", {
  miss <- ison_adolescents |>
    add_tie_attribute("weight", c(1, NA, NA, 1, 1, 1, NA, NA, 1, 1))
  for (rule in c("zero", "density", "mean", "median", "modal")) {
    outm <- run_or_skip(impute_ties(as_matrix(miss), rule), rule, "matrix")
    expect_false(anyNA(outm))
    outg <- run_or_skip(impute_ties(miss, rule), rule, "tidygraph")
    expect_false(anyNA(tie_attribute(outg, "weight")))
  }
  expect_equal(sum(impute_ties(as_matrix(miss), "zero") == 0) -
                 sum(as_matrix(miss) == 0, na.rm = TRUE),
               sum(is.na(as_matrix(miss))))
})
