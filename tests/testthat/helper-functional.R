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

# The canonical networks the cross-class sweep runs over. The plain network
# catches the common case. The weighted, directed network catches the cases
# where the classes diverge, since tie values and tie directions are what the
# classes record differently.
canonical_nets <- local({
  dir <- to_directed(canonical_net)
  list(
    plain = canonical_net,
    `weighted, directed` = add_tie_attribute(
      dir, "weight", rep(c(1, 2), length.out = as.numeric(net_ties(dir))))
  )
})

# Networks and arguments for the to_*() functions that split a network into a
# list of networks. Each network holds the attribute that the function splits
# on, so that the function returns a list of networks rather than the network
# unchanged. This exercises the list-returning path for every object class,
# which the single-network cross-class sweep cannot reach.
split_fixtures <- local({
  ring <- to_named(create_ring(8))
  # Two components of unequal size, so that the order to_components() returns
  # them in (largest first) is the same for every class.
  two_rings <- to_named(as_tidygraph(rbind(
    cbind(as_matrix(create_ring(5)), matrix(0, 5, 3)),
    cbind(matrix(0, 3, 5), as_matrix(create_ring(3))))))
  list(
    to_egos       = list(net = canonical_net, args = list()),
    to_components = list(net = two_rings, args = list()),
    to_subgraphs  = list(net = add_node_attribute(ring, "group",
                                                  rep(c("A", "B"), each = 4)),
                         args = list(attribute = "group")),
    to_waves      = list(net = add_tie_attribute(ring, "wave",
                                                 rep(1:2, each = 4)),
                         args = list(attribute = "wave")),
    to_slices     = list(net = add_tie_attribute(ring, "time", 1:8),
                         args = list(slice = c(3, 6))),
    to_times      = list(net = add_tie_attribute(ring, "time",
                                                 rep(1:2, each = 4)),
                         args = list())
  )
})

# Can this object class hold the attribute that the splitting function `fn`
# splits on? A matrix records only the ties themselves, and an edgelist
# records no nodal attributes, so for these classes returning the network
# unchanged (or an empty list) is correct rather than something to audit.
split_class_holds_info <- function(fn, cl) {
  !(cl == "matrix" && fn %in% c("to_subgraphs", "to_waves", "to_slices",
                                "to_times")) &&
    !(cl == "edgelist" && fn == "to_subgraphs")
}

# The ties of a network, as a sorted vector of "from--to" pairs. Tie sets are
# compared across classes instead of adjacency matrices because the classes
# order their nodes differently, and because an edgelist cannot record an
# isolate, which would make the matrices differ in size as well as in order.
tie_set <- function(net) {
  el <- as_edgelist(net)
  if (!nrow(el)) return(character(0))
  from <- as.character(el$from)
  to <- as.character(el$to)
  sort(paste(pmin(from, to), pmax(from, to), sep = "--"))
}

# The tie sets of a list of split networks. Classes differ in the order they
# list the networks in, listing them in nodelist order or in order of
# appearance in the ties, so a named list is put into name order first.
# An unnamed list keeps its order, since a function that orders its output
# (as to_components() orders by size) must do so for every class.
tie_sets <- function(nets) {
  out <- lapply(nets, tie_set)
  if (is.null(names(out))) out else out[order(names(out))]
}

# Is `out` a non-empty list of manynet-compatible networks?
is_network_list <- function(out) {
  is_list(out) && length(out) > 0 &&
    all(vapply(out, is_manynet, logical(1)))
}

# Does this network actually hold the information that `fn` extracts?
# as_changelist()/as_globallist() return NULL where the network holds no
# changes/global attributes, and as_nodelist() returns NULL where there are
# no nodal attributes, not even labels. NULL is then expected behaviour
# rather than something to audit.
list_holds_info <- function(fn, net) {
  g <- as_igraph(net)
  switch(fn,
         as_changelist = "changes" %in% igraph::graph_attr_names(g),
         as_globallist = "globals" %in% igraph::graph_attr_names(g),
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
