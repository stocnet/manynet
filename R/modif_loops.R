# Loops ####

#' Modifying network loops
#' @name modif_loops
#' @description
#'   These functions reformat the loops and the parallel ties of a network,
#'   keeping its nodes.
#'
#'   - `to_anti()` reformats network data into its complement, where only ties _not_ present in the original network
#'   are included in the new network.
#'   - `to_unlooped()` removes the loops of a network, the ties from a node
#'   to itself.
#'   Parallel ties are kept, since a second tie between two nodes is not a
#'   loop; `tie_is_parallel()` marks them.
#'   - `to_looped()` adds a loop to every node, as a diagonal of ones,
#'   so that each node is one of its own neighbours.
#'   A node that has a loop already gets no second one.
#'   A multiplex network gets the loops in each layer, and a network with
#'   moments gets them at each moment, for the nodes that are active then.
#'   - `to_simplified()` removes the loops of a network and combines its
#'   parallel ties by the `rule`, so that `is_simplified()` marks it TRUE.
#'   Every layer stays: use `to_layer()` to keep one,
#'   or `to_aggregated(over = "layer")` to combine them.
#'
#'   If the format condition is not met,
#'   for example `to_unlooped()` is used on a network that has no loops,
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
#'   available_methods(collect_functions("to_(anti|unlooped|looped|simplified)$"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_loops
#' @importFrom igraph complementer
#' @examples
#' to_anti(ison_southern_women)
#' @export
to_anti <- function(.data) UseMethod("to_anti")

#' @export
to_anti.default <- function(.data) {
  as_input(.data, to_anti)
}

#' @export
to_anti.matrix <- function(.data){
  matrix(1, nrow(.data), ncol(.data)) - .data
}

#' @export
to_anti.data.frame <- function(.data){
  as_edgelist.matrix(to_anti.matrix(as_matrix(.data)))
}

#' @export
to_anti.igraph <- function(.data){
  if(is_twomode(.data)){
    as_igraph(to_anti.matrix(as_matrix(.data)))
  } else {
    igraph::complementer(as_igraph(.data), 
                         loops = is_looped(.data))
  }
}

#' @export
to_anti.tbl_graph <- function(.data){
  if(is_twomode(.data)){
    as_tidygraph(to_anti.matrix(as_matrix(.data)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(.data), 
                                      loops = is_looped(.data)))
  }
}

#' @rdname modif_loops
#' @importFrom igraph simplify
#' @export
to_unlooped <- function(.data) UseMethod("to_unlooped")

#' @export
to_unlooped.default <- function(.data) {
  as_input(.data, to_unlooped)
}

#' @export
to_unlooped.igraph <- function(.data) {
  # Only the loops are removed. A second tie between two nodes is not a loop,
  # and may be a second record, a second observation, or a second report.
  igraph::simplify(.data, remove.multiple = FALSE, remove.loops = TRUE)
}

#' @export
to_unlooped.stocnet <- function(.data) {
  if(is.null(.data$ties) || nrow(.data$ties) == 0) return(.data)
  ties <- .data$ties
  # Only the loops are removed, as in every other method.
  # a hyperedge names no node in either column, and is no loop
  keep_ties(.data, which(is.na(ties$from) | ties$from != ties$to)) |>
    .record_exclusion(.data, "loops", "ties")
}

#' @export
to_unlooped.tbl_graph <- function(.data) {
  # the record is written here rather than in the igraph method because
  # `.record_transformation()` returns a 'tbl_graph', which would change what
  # the igraph method gives back to the methods that delegate to it
  as_tidygraph(to_unlooped(as_igraph(.data))) |>
    .record_exclusion(.data, "loops", "ties")
}

#' @export
to_unlooped.matrix <- function(.data) {
  out <- .data
  diag(out) <- 0
  out
}

#' @export
to_unlooped.data.frame <- function(.data) {
  out <- .data[.data$from != .data$to,]
  out
}

#' @rdname modif_loops
#' @examples
#' to_looped(create_ring(4))
#' @export
to_looped <- function(.data) UseMethod("to_looped")

#' @export
to_looped.default <- function(.data) {
  as_input(.data, to_looped)
}

#' @export
to_looped.matrix <- function(.data) {
  if(is_twomode(.data))
    snet_abort("A two-mode network cannot hold loops, since a node cannot be",
               "tied to itself across two modes.")
  out <- .data
  # a loop that is there already, of whatever value, is kept as it is
  diag(out)[is.na(diag(out)) | diag(out) == 0] <- 1
  out
}

#' @export
to_looped.stocnet <- function(.data) {
  if(is_twomode(.data))
    snet_abort("A two-mode network cannot hold loops, since a node cannot be",
               "tied to itself across two modes.")
  n <- as.numeric(net_nodes(.data))
  if(n == 0) return(.data)
  ties <- .data$ties
  times <- .stocnet_times(.data)
  active <- .node_state(.data, "active", times, default = TRUE)
  occasions <- .stocnet_occasions(.data)
  # Each layer, and each moment the ties record, gets its loops, but only for
  # the nodes in the network at that moment, since a node that has left could
  # not have been its own neighbour then.
  loops <- lapply(seq_len(nrow(occasions)), function(o){
    layer <- occasions$layer[[o]]; time <- occasions$time[[o]]
    at <- if(is.na(time)) 1L else match(time, times)
    nodes <- which(active[, at])
    if(!is.null(ties) && nrow(ties)){
      here <- dplyr::tibble(
        layer = if(is.null(ties[["layer"]])) NA_character_ else as.character(ties$layer),
        time = if(is.null(ties[["time"]])) NA else ties$time,
        .rows = nrow(ties))
      looped <- ties$from[.same_occasion(here, layer, time) &
                            !is.na(ties$from) & ties$from == ties$to]
      nodes <- setdiff(nodes, looped)
    }
    if(!length(nodes)) return(NULL)
    out <- dplyr::tibble(from = as.integer(nodes), to = as.integer(nodes))
    if(!is.null(ties[["layer"]])) out$layer <- layer
    if(!is.null(ties[["time"]])) out$time <- rep(time, length(nodes))
    out
  })
  loops <- dplyr::bind_rows(loops)
  if(!nrow(loops)) return(.data)
  # A loop is a tie of one, whatever the tie values record.
  if(!is.null(ties[["weight"]])) loops$weight <- 1
  if(!is.null(ties[["sign"]])) loops$sign <- 1
  if(!is.null(ties[["time"]])) loops$time <- .match_class(loops$time, ties$time)
  # Adding loops is none of the transformations that GRAND names, so it is
  # not recorded among them.
  .data$ties <- dplyr::bind_rows(ties, loops)
  validate_stocnet(.data)
}

#' @rdname modif_loops
#' @examples
#' to_simplified(ison_koenigsberg)
#' @export
to_simplified <- function(.data, rule = c("max","min","mean","sum","product")) {
  rule <- match.arg(rule)
  # A matrix holds one value for each dyad, and so no parallel ties.
  if(is.matrix(.data)) return(to_unlooped(.data))
  # Parallel ties are combined within each layer, moment, and report, so a
  # multiplex network keeps all of its layers.
  to_unlooped(to_aggregated(.data, over = NULL, rule = rule))
}

