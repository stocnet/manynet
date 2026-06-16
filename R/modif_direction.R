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
#'   This is equivalent to the "collapse" mode in `{igraph}`..
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
#' @export
to_undirected <- function(.data) UseMethod("to_undirected")

#' @export
to_undirected.default <- function(.data){
  as_input(.data, to_undirected)
}

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(.data) {
  igraph::as_undirected(.data)
}

#' @export
to_undirected.network <- function(.data) {
  .data$gal$directed <- FALSE
  .data
}

#' @export
to_undirected.matrix <- function(.data) {
  if (is_twomode(.data)) {
    .data
  } else ((.data + t(.data)) > 0) * 1
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
to_acyclic.igraph <- function(.data) {
  if(is_directed(.data)){
    delete_ties(.data, igraph::feedback_arc_set(.data))
  } else igraph::as_directed(.data, mode = "acyclic")
}

