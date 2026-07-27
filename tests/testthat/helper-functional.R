# Infrastructure for the test-functional_*.R files.
# These tests automatically enumerate exported functions by family prefix
# (to_*, from_*, net_*, create_*, etc.) and run them across a standard grid
# of fixture networks and object classes, so that any *new* function added to
# a family is picked up and audited without writing new tests.
# Non-conformant function/fixture combinations are skipped with a structured
# "AUDIT [...]" message rather than failed, so `devtools::test()` output can
# be grepped to find where implementations still need work.

# Exported functions in a family, excluding deprecated/defunct shims
alive_functions <- function(pattern) {
  fns <- sort(collect_functions(pattern))
  keep <- vapply(fns, function(f) {
    fun <- get(f, envir = asNamespace("manynet"))
    is.function(fun) &&
      !grepl("Deprecated|Defunct|fn_moved",
             paste(deparse(body(fun)), collapse = " "))
  }, logical(1))
  fns[keep]
}

# Evaluate expr; on error, skip with a structured, greppable audit message.
run_or_skip <- function(expr, fn, fixture) {
  tryCatch(
    expr,
    error = function(e) {
      testthat::skip(paste0("AUDIT [", fn, " x ", fixture, "]: ",
                            conditionMessage(e)))
    }
  )
}

# Standard grid of fixture networks covering the main formats manynet
# functions are expected to handle. All are tidygraph/mnet objects;
# class coverage is handled separately via class_versions().
func_fixtures <- local({
  set.seed(1234)
  list(
    basic        = create_ring(8),
    directed     = generate_random(8, directed = TRUE),
    labelled     = ison_adolescents,
    twomode      = ison_southern_women,
    weighted     = add_tie_attribute(create_ring(8), "weight",
                                     rep(c(1, 2), each = 4)),
    signed       = to_signed(create_ring(8)),
    attributed   = add_node_attribute(create_ring(8), "group",
                                      rep(c("A", "B"), each = 4)),
    multiplex    = ison_algebra,
    longitudinal = fict_starwars
  )
})

# The same network represented in each of the main object classes.
class_versions <- function(net) {
  list(
    tidygraph = as_tidygraph(net),
    igraph    = as_igraph(net),
    matrix    = as_matrix(net),
    network   = as_network(net),
    edgelist  = as_edgelist(net),
    stocnet   = as_stocnet(net)
  )
}

# A small, labelled, undirected canonical network for cross-class tests
canonical_net <- ison_adolescents

# Does this network actually hold the information that `fn` extracts?
# as_changelist()/as_globallist() return NULL where the network holds no
# changes/global attributes, and as_nodelist() returns NULL where there are
# no nodal attributes, not even labels. NULL is then expected behaviour
# rather than something to audit.
list_holds_info <- function(fn, net) {
  g <- as_igraph(net)
  switch(fn,
         as_changelist = "changes" %in% igraph::graph_attr_names(g),
         as_globallist = "global" %in% igraph::graph_attr_names(g),
         as_nodelist = length(igraph::vertex_attr_names(g)) > 0,
         TRUE)
}

# Can this object class hold the information that `fn` extracts?
# Matrices and edgelists have nowhere to keep network-level information,
# so as_infolist()/as_changelist()/as_globallist() correctly return NULL
# for them (node labels do survive, so as_nodelist() is not excused).
list_class_holds_info <- function(fn, cl) {
  !(cl %in% c("matrix", "edgelist") &&
      fn %in% c("as_infolist", "as_changelist", "as_globallist"))
}

# Is `out` an acceptable return for a modif/manip function? Either a
# manynet-compatible object, a list of them, or tabular/matrix output.
is_acceptable_output <- function(out) {
  if (is.null(out)) return(FALSE)
  if (is_manynet(out)) return(TRUE)
  if (is.list(out) && !is.data.frame(out)) {
    return(all(vapply(out, function(x) is_manynet(x) || is.data.frame(x),
                      logical(1))))
  }
  is.matrix(out) || is.data.frame(out) || is.atomic(out)
}
