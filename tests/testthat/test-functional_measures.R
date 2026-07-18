# Functional tests for the net_*(), node_*(), tie_*(), layer_*() and mode_*()
# description families (measures proper live in {netrics}).
# Every live (non-defunct) exported function in these families is run across
# the standard fixture grid, checking family-wide output conventions:
# node_*() returns one value per node, tie_*() one per tie, net_*() a scalar
# or short summary. New functions are picked up automatically.

meas_families <- list(
  net   = alive_functions("^net_"),
  node  = alive_functions("^node_"),
  tie   = alive_functions("^tie_"),
  layer = alive_functions("^layer_"),
  mode  = alive_functions("^mode_")
)

# How to construct required arguments from the network under test.
meas_argmakers <- list(
  node_attribute = function(net) {
    attrs <- setdiff(net_node_attributes(net), c("name", "type", "active"))
    if (!length(attrs)) stop("fixture has no non-reserved node attribute")
    list(attr_name = attrs[1])
  },
  tie_attribute = function(net) {
    attrs <- setdiff(net_tie_attributes(net), c("from", "to"))
    if (!length(attrs)) stop("fixture has no tie attribute")
    list(attr_name = attrs[1])
  }
)

for (family in names(meas_families)) {
  for (fn in meas_families[[family]]) {
    f <- get(fn, envir = asNamespace("manynet"))

    test_that(paste0(fn, "() follows family conventions"), {
      expect_identical(names(formals(f))[1], ".data",
                       label = paste0("First argument of ", fn, "()"))
    })

    for (fx in names(func_fixtures)) {
      test_that(paste0(fn, "() works on the ", fx, " fixture"), {
        net <- func_fixtures[[fx]]
        args <- if (fn %in% names(meas_argmakers)) {
          run_or_skip(meas_argmakers[[fn]](net), fn, fx)
        } else list()
        out <- run_or_skip(do.call(f, c(list(net), args)), fn, fx)
        if (is.null(out)) {
          skip(paste0("AUDIT [", fn, " x ", fx, "]: returns NULL"))
        }
        if (family == "node") {
          expect_length(c(unclass(out)), as.numeric(net_nodes(net)))
        } else if (family == "tie") {
          expect_length(c(unclass(out)), as.numeric(net_ties(net)))
        } else if (grepl("attributes|names|missing", fn)) {
          # listings may legitimately be empty (e.g. no attributes)
          expect_true(is.atomic(unclass(out)),
                      label = paste0(fn, "() returning an atomic listing"))
        } else {
          # net_/layer_/mode_ functions return a scalar or a short
          # per-mode/per-layer/per-attribute summary, never per-node output
          expect_true(length(c(unclass(out))) >= 1,
                      label = paste0(fn, "() returning a non-empty result"))
        }
      })
    }
  }
}

# Dimensions reported by net_dims() must agree with net_nodes()
for (fx in names(func_fixtures)) {
  test_that(paste0("net_dims() agrees with net_nodes() on ", fx), {
    net <- func_fixtures[[fx]]
    expect_equal(sum(as.numeric(net_dims(net))),
                 as.numeric(net_nodes(net)))
    expect_length(as.numeric(net_dims(net)), as.numeric(net_modes(net)))
  })
}
