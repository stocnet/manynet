# Direction ####

#' Modifying networks by formatting their directionality
#' @name modif_direction
#' @description
#'   These functions reformat manynet-consistent data.
#' 
#'   - `to_directed()` reformats undirected network data to a directed network.
#'   In a one-mode network each tie is given a direction at random.
#'   In a two-mode network every tie runs from the first mode to the second,
#'   which `to_redirected()` reverses.
#'   A two-mode matrix cannot record direction, so it stays as it is.
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
  if(is_directed.igraph(.data)) return(.data)
  if(is_twomode(.data)){
    # The ties between two modes are directed one way, from the first mode to
    # the second, as the directed two-mode networks collected in practice run.
    # `to_redirected()` turns them all around.
    out <- igraph::as_directed(.data, mode = "arbitrary")
    type <- igraph::V(out)$type
    el <- igraph::as_edgelist(out, names = FALSE)
    back <- which(type[el[, 1]] & !type[el[, 2]])
    if(length(back)) out <- igraph::reverse_edges(out, back)
    snet_info("Ties are directed from the first mode to the second.")
    return(out)
  }
  snet_info("Directions are assigned to existing ties at random.")
  igraph::as_directed(.data, mode = "random")
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
#'   Use `impute_ties()` first to state a different assumption.
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

# A network whose ties name a reporter or a target holds one network for each
# of them, so a tie is reconciled only with the tie running the other way in
# the same report, about the same target, in the same layer and
# wave. igraph knows none of these, and would reconcile a tie in one report
# with its reverse in another.
#' @export
to_undirected.stocnet <- function(.data,
                                  rule = c("collapse","min","max","mean","sum","product")) {
  rule <- match.arg(rule)
  ties <- .data$ties
  if(!.holds_node(ties[["by"]]) && !.holds_node(ties[["about"]]))
    return(as_stocnet(to_undirected(as_tidygraph(.data), rule = rule)))
  # A multilevel network can hold directed ties within a mode, so only a
  # network that is two-mode and nothing more has no direction to remove.
  if(!is_directed(.data) || (is_twomode(.data) && !is_multilevel(.data)))
    return(.data)
  groups <- intersect(c("layer", "time", "by", "about"), names(ties))
  valued <- "weight" %in% names(ties)
  value <- if(valued) ties$weight else rep(1, nrow(ties))
  # A layer already held as undirected holds one row per dyad, which is not a
  # tie in one direction only, so its rows are kept as they are, as loops are.
  if("layer" %in% names(ties)){
    layers <- unique(as.character(ties$layer))
    undirected <- layers[!vapply(layers, function(l) layer_is_directed(.data, l),
                                 logical(1))]
    settled <- as.character(ties$layer) %in% undirected
  } else settled <- rep(FALSE, nrow(ties))
  loops <- ties$from == ties$to | settled
  key <- do.call(paste, c(list(pmin(ties$from, ties$to), pmax(ties$from, ties$to)),
                          lapply(groups, function(g) as.character(ties[[g]])),
                          list(sep = "\r")))
  keys <- unique(key[!loops])
  # the value held in one direction of each pair, and zero where it holds none
  direction <- function(sel){
    out <- rep(0, length(keys))
    if(any(sel)){
      sums <- rowsum(value[sel], key[sel], reorder = FALSE)
      out[match(rownames(sums), keys)] <- sums[, 1]
    }
    out
  }
  ahead <- direction(!loops & ties$from < ties$to)
  behind <- direction(!loops & ties$from > ties$to)
  combined <- switch(rule,
                     "collapse" = ,
                     "sum"      = ahead + behind,
                     "min"      = pmin(ahead, behind),
                     "max"      = pmax(ahead, behind),
                     "mean"     = (ahead + behind)/2,
                     "product"  = ahead * behind)
  present <- function(x) is.na(x) | x != 0
  connected <- sum(present(ahead) | present(behind))
  asymmetric <- sum(xor(present(ahead), present(behind)))
  first <- match(keys, key)
  out <- ties[first, , drop = FALSE]
  out[c("from", "to")] <- list(pmin(out$from, out$to), pmax(out$from, out$to))
  # An unvalued network stays unvalued, except where averaging the two
  # directions gives a half, as it does for a matrix.
  if(valued || rule == "mean"){
    out$weight <- combined
    ties$weight <- value
  }
  keep <- present(combined)
  order_kept <- order(c(first[keep], which(loops)))
  out <- dplyr::bind_rows(out[keep, , drop = FALSE],
                          ties[loops, , drop = FALSE])[order_kept, , drop = FALSE]
  info <- .data$info
  # A rule such as "min" can leave a layer without any tie, and a layer
  # without ties is no longer a layer of the network.
  if("layer" %in% names(out))
    info <- .prune_layer_info(info, unique(as.character(out$layer)))
  info$directed <- if(is.null(names(info$directed))) FALSE else
    stats::setNames(rep(FALSE, length(info$directed)), names(info$directed))
  entry <- if(connected == 0) rule else
    paste0(rule, " (", round(asymmetric / connected * 100),
           "% of connected dyads non-reciprocal)")
  make_stocnet(info = info, nodes = .data$nodes, ties = out,
               changes = .data$changes, globals = .data$globals,
               missings = .data$missings) |>
    .record_transformation("symmetrisation", entry)
}

#' @export
to_undirected.tbl_graph <- function(.data,
                                    rule = c("collapse","min","max","mean","sum","product")) {
  rule <- match.arg(rule)
  # the percent is of the network as it was, so it is taken before the collapse
  pct <- .non_reciprocal_percent(.data)
  entry <- if(is.null(pct)) rule else
    paste0(rule, " (", pct, "% of connected dyads non-reciprocal)")
  as_tidygraph(to_undirected(as_igraph(.data), rule = rule)) |>
    .record_transformation("symmetrisation", entry)
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
to_acyclic.stocnet <- function(.data){
  if(!is_directed(.data)) return(as_stocnet(to_acyclic(as_tidygraph(.data))))
  # the ties table is the order igraph is built from, so the edge ids the
  # feedback arc set names index its rows
  arcs <- as.integer(igraph::feedback_arc_set(as_igraph(.data)))
  keep_ties(.data, setdiff(seq_len(nrow(.data$ties)), arcs)) |>
    .record_exclusion(.data, "feedback arcs", "ties")
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
