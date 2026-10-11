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
      if (!list_holds_info(fn, net)) {
        # Nothing of this kind to extract: NULL is the expected return
        expect_null(out)
      } else {
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
      }
    })
  }
}

# Extraction should work from any object class, not just tidygraph
for (fn in list_funs) {
  f <- get(fn, envir = asNamespace("manynet"))
  test_that(paste0(fn, "() works across object classes"), {
    versions <- class_versions(canonical_net)
    for (cl in names(versions)) {
      out <- quietly(tryCatch(f(versions[[cl]]), error = function(e) e))
      if (inherits(out, "error")) {
        skip(paste0("AUDIT [", fn, " x ", cl, "]: ",
                    conditionMessage(out)))
      }
      if (is.null(out) &&
          list_holds_info(fn, canonical_net) &&
          list_class_holds_info(fn, cl)) {
        skip(paste0("AUDIT [", fn, " x ", cl, "]: returns NULL"))
      }
    }
    succeed()
  })
}

# (from_ties() tests live with the rest of the from_*() family in
# test-functional_from.R)

test_that("as_nodelist() and net_tie_attributes() agree across classes", {
  # A stocnet records its size in a table of rows without columns, and where
  # a tie is in 'from' and 'to', but neither is something to list.
  plain <- create_filled(4)
  for(net in list(plain, as_tidygraph(plain), as_igraph(plain))){
    expect_null(as_nodelist(net))
    expect_equal(net_tie_attributes(net), character(0))
  }
  weighted <- add_tie_attribute(plain, "weight", 1:6)
  marked <- mutate_nodes(weighted, group = c("a", "a", "b", "b"))
  for(net in list(marked, as_tidygraph(marked), as_igraph(marked))){
    expect_equal(net_tie_attributes(net), "weight")
    expect_named(as_nodelist(net), "group")
    expect_equal(nrow(as_nodelist(net)), 4)
  }
  # the mode of a node is listed, whichever class records it
  for(net in list(create_filled(c(3, 2)), as_tidygraph(create_filled(c(3, 2)))))
    expect_equal(dim(as_nodelist(net)), c(5, 1))
  # isolates have no ties to be counted by, and are not lost for that
  alone <- create_empty(5)
  expect_null(as_nodelist(alone))
  expect_equal(igraph::vcount(as_igraph(alone)), 5)
  expect_equal(as.numeric(net_nodes(as_stocnet(as_igraph(alone)))), 5)
  expect_equal(as.numeric(net_nodes(as_tidygraph(alone))), 5)
})
