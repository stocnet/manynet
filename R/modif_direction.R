# Direction ####

#' Modifying networks by formatting their directionality
#' @name modif_direction
#' @description
#'   These functions reformat manynet-consistent data.
#' 
#'   - `to_directed()` reformats undirected network data to a directed network.
#'   - `to_undirected()` reformats directed network data to an undirected network,
#'   so that any pair of nodes with at least one directed edge will be
#'   connected by an undirected edge in the new network.
#'   By default this is equivalent to the "collapse" mode in `{igraph}`,
#'   but `rule` offers the other ways of reconciling a pair of ties running in
#'   opposite directions, which matters where the network is weighted.
#'   - `to_redirected()` formats directed network data by flipping/transposing
#'   any existing direction such that senders become receivers and
#'   receivers become senders.
#'   This essentially has no effect on undirected networks or reciprocated ties.
#'   - `to_reciprocated()` reformats directed network data such that every 
#'   directed tie is reciprocated.
#'   - `to_acyclic()` reformats network data to an acyclic graph.
#' 
#'   If the format condition is not met,
#'   for example `to_undirected()` is used on a network that is already undirected,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(direct|recip|acyc)"))
#'   ```
#' @template param_data
#' @family ties
#' @template fam_modif
NULL

#' @rdname modif_direction 
#' @importFrom igraph as.directed
#' @export
to_directed <- function(.data) UseMethod("to_directed")

#' @export
to_directed.default <- function(.data){
  as_input(.data, to_directed)
}

#' @export
to_directed.igraph <- function(.data) {
  if(!is_directed.igraph(.data)){
    snet_info("Directions are assigned to existing ties at random.")
    igraph::as_directed(.data, mode = "random")
  } else .data
}

#' @rdname modif_direction
#' @param rule How the values of a pair of ties running in opposite directions
#'   are reconciled into the single value of an undirected tie:
#'   - "collapse" (the default) sums them, so that a tie exists wherever a tie
#'   existed in either direction. For an unweighted network this is igraph's
#'   "collapse" mode, since a tie in either direction gives 1 either way.
#'   - "sum" is the same operation, named for the arithmetic rather than
#'   the intent.
#'   - "min" takes the smaller of the two values, so that a tie is only as
#'   strong as the weaker direction. Use where a relationship needs to be
#'   confirmed from both sides, as in a mutual friendship nomination.
#'   - "max" takes the larger, so that the stronger direction stands for
#'   the pair. Use where a single report is taken as sufficient evidence.
#'   - "mean" averages them, treating the two directions as two readings
#'   of one underlying quantity.
#'   - "product" multiplies them, so that a tie survives only where both
#'   directions are non-zero, and strong ties are rewarded disproportionately.
#'
#'   Values missing in one direction are not treated as agreement:
#'   they propagate, so that `NA` in either direction gives `NA`.
#'   Use the `na_to_*()` functions first to state a different assumption.
#' @examples
#' to_undirected(ison_networkers)
#' to_undirected(ison_networkers, rule = "min")
#' @export
to_undirected <- function(.data,
                          rule = c("collapse","min","max","mean","sum","product")) {
  # note that whether there is anything to reconcile is judged by each method
  # rather than here, since `is_directed()` reports on the network rather than
  # on how it happens to be stored: a graph igraph holds as directed, with
  # every dyad listed in both directions, is undirected by that measure while
  # still carrying the pairs of ties that need collapsing
  UseMethod("to_undirected")
}

#' @export
to_undirected.default <- function(.data,
                                  rule = c("collapse","min","max","mean","sum","product")){
  as_input(.data, to_undirected, rule = rule)
}

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(.data,
                                 rule = c("collapse","min","max","mean","sum","product")) {
  rule <- match.arg(rule)
  # igraph's own flag, rather than `is_directed()`, since it is igraph's
  # representation that says whether opposing pairs of ties are still held
  if(!igraph::is_directed(.data)) return(.data)
  if(rule %in% c("collapse","sum")){
    # igraph's default combination rule discards every tie attribute other
    # than the weight, so sign, type, and time are named explicitly
    igraph::as_undirected(.data, mode = "collapse",
                          edge.attr.comb = .undirected_attr_comb())
  } else {
    # igraph offers no minimum, mean, or product combination, so the
    # arithmetic is done on the matrix and the node attributes restored
    as_igraph(to_undirected(as_matrix(.data), rule = rule)) |>
      bind_node_attributes(.data)
  }
}

#' @export
to_undirected.tbl_graph <- function(.data,
                                    rule = c("collapse","min","max","mean","sum","product")) {
  rule <- match.arg(rule)
  as_tidygraph(to_undirected(as_igraph(.data), rule = rule)) |>
    add_info(transform = paste0("symmetrised (", rule, ")"))
}

#' @export
to_undirected.network <- function(.data,
                                  rule = c("collapse","min","max","mean","sum","product")) {
  # this delegates rather than setting `$gal$directed`, which would declare
  # the network undirected while leaving its asymmetric dyads untouched
  as_network(to_undirected(as_tidygraph(.data), rule = rule))
}

#' @export
to_undirected.data.frame <- function(.data,
                                     rule = c("collapse","min","max","mean","sum","product")) {
  as_edgelist(to_undirected(as_tidygraph(.data), rule = rule))
}

#' @export
to_undirected.matrix <- function(.data,
                                 rule = c("collapse","min","max","mean","sum","product")) {
  rule <- match.arg(rule)
  if (is_twomode(.data)) return(.data)
  # a symmetric matrix already holds one value per dyad, so reconciling it
  # again would e.g. double every tie value under the default rule
  if (isSymmetric(unname(.data))) return(.data)
  out <- switch(rule,
                "collapse" = ,
                "sum"      = .data + t(.data),
                "min"      = pmin(.data, t(.data)),
                "max"      = pmax(.data, t(.data)),
                "mean"     = (.data + t(.data))/2,
                "product"  = .data * t(.data))
  # `pmin()` and `pmax()` return a vector, so the shape is restored here
  matrix(out, nrow(.data), ncol(.data), dimnames = dimnames(.data))
}

#' @rdname modif_direction 
#' @importFrom igraph reverse_edges
#' @importFrom tidygraph reroute
#' @export
to_redirected <- function(.data) UseMethod("to_redirected")

#' @export
to_redirected.default <- function(.data){
  as_input(.data, to_redirected)
}

#' @export
to_redirected.igraph <- function(.data) {
  igraph::reverse_edges(.data)
}

#' @export
to_redirected.data.frame <- function(.data) {
  out <- .data
  out$from <- .data$to
  out$to <- .data$from
  out
}

#' @export
to_redirected.matrix <- function(.data) {
  t(.data)
}

#' @rdname modif_direction
#' @importFrom igraph as_directed
#' @export
to_reciprocated <- function(.data) UseMethod("to_reciprocated")

#' @export
to_reciprocated.default <- function(.data){
  as_input(.data, to_reciprocated)
}

#' @export
to_reciprocated.igraph <- function(.data) {
  igraph::as_directed(.data, mode = "mutual")
}

#' @export
to_reciprocated.matrix <- function(.data) {
  .data + t(.data)
}

#' @rdname modif_direction
#' @importFrom igraph as_directed feedback_arc_set
#' @export
to_acyclic <- function(.data) UseMethod("to_acyclic")

#' @export
to_acyclic.default <- function(.data){
  as_input(.data, to_acyclic)
}


#' @export
to_acyclic.tbl_graph <- function(.data){
  # only the directed branch excludes anything: for an undirected network
  # `to_acyclic()` orients the ties that are there rather than dropping any
  as_tidygraph(to_acyclic(as_igraph(.data))) |>
    .record_exclusion(.data, "feedback arcs", "ties")
}

#' @export
to_acyclic.igraph <- function(.data) {
  if(is_directed(.data)){
    delete_ties(.data, igraph::feedback_arc_set(.data))
  } else igraph::as_directed(.data, mode = "acyclic")
}


# Helper functions ------------------

# How tie attributes are combined when a pair of opposing ties is collapsed
# into one. igraph's default is list(weight = "sum", name = "concat",
# "ignore"), which silently discards sign, type, time, and everything else,
# so those are kept by taking the first of the pair.
.undirected_attr_comb <- function(){
  list(weight = "sum", name = "concat", "first")
}
