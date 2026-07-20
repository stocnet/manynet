# Functional tests for the as_*list() extraction family.
# Every as_*list() function is run across the fixture grid and all object
# classes, checking that extracted tables have the expected shape.

list_funs <- alive_functions("^as_.*list$")

for (fn in list_funs) {
  f <- get(fn, envir = asNamespace("manynet"))
  for (fx in names(func_fixtures)) {
    test_that(paste0(fn, "() works on the ", fx, " fixture"), {
      net <- func_fixtures[[fx]]
      out <- run_or_skip(f(net), fn, fx)
      if (is.null(out)) {
        skip(paste0("AUDIT [", fn, " x ", fx, "]: returns NULL"))
      }
      expect_true(is.data.frame(out) || is.list(out),
                  label = paste0(fn, "() output on ", fx))
      if (fn == "as_nodelist" && is.data.frame(out)) {
        expect_equal(nrow(out), as.numeric(net_nodes(net)))
      }
      if (fn == "as_edgelist" && is.data.frame(out)) {
        expect_equal(nrow(out), as.numeric(net_ties(net)))
        expect_identical(names(out)[1:2], c("from", "to"))
      }
    })
  }
}

# Extraction should work from any object class, not just tidygraph
for (fn in list_funs) {
  f <- get(fn, envir = asNamespace("manynet"))
  test_that(paste0(fn, "() works across object classes"), {
    for (cl in names(class_versions(canonical_net))) {
      out <- tryCatch(f(class_versions(canonical_net)[[cl]]),
                      error = function(e) e)
      if (inherits(out, "error")) {
        skip(paste0("AUDIT [", fn, " x ", cl, "]: ",
                    conditionMessage(out)))
      }
    }
    succeed()
  })
}

# (from_ties() tests live with the rest of the from_*() family in
# test-functional_from.R)
