# Simplifying ####

#' Modifying network complexity
#' @name modif_plexity
#' @description
#'   These functions reformat manynet-consistent data.
#' 
#'   - `to_anti()` reformats network data into its complement, where only ties _not_ present in the original network
#'   are included in the new network.
#'   - `to_simplex()` reformats complex network data, containing loops, to simplex network data, without any loops.
#'   - `to_uniplex()` reformats multiplex network data to a single type of tie.
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
#'   available_methods(collect_functions("to_.*(anti|plex)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_plexity
#' @importFrom igraph complementer
#' @examples
#' to_anti(ison_southern_women)
#' @export
to_anti <- function(.data) UseMethod("to_anti")

#' @export
to_anti.default <- function(.data){
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
                         loops = is_complex(.data))
  }
}

#' @export
to_anti.tbl_graph <- function(.data){
  if(is_twomode(.data)){
    as_tidygraph(to_anti.matrix(as_matrix(.data)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(.data), 
                                      loops = is_complex(.data)))
  }
}

#' @export
to_anti.network <- function(.data){
  as_network(to_anti(as_igraph(.data)))
}

#' @rdname modif_plexity
#' @importFrom igraph simplify
#' @export
to_simplex <- function(.data) UseMethod("to_simplex")

#' @export
to_simplex.default <- function(.data){
  as_input(.data, to_simplex)
}

#' @export
to_simplex.tbl_graph <- function(.data) {
  as_tidygraph(to_simplex(as_igraph(.data)))
}

#' @export
to_simplex.igraph <- function(.data) {
  igraph::simplify(.data)
}

#' @export
to_simplex.matrix <- function(.data) {
  out <- .data
  diag(out) <- 0
  out
}

#' @export
to_simplex.data.frame <- function(.data) {
  out <- .data[.data$from != .data$to,]
  out
}

#' @export
to_simplex.network <- function(.data) {
  as_network(to_simplex(as_igraph(.data)))
}

#' @rdname modif_plexity
#' @param tie Character string naming a tie attribute to retain from a graph.
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#'   E edge_attr_names
#' @examples
#' as_tidygraph(create_filled(5)) |>
#'   mutate_ties(type = sample(c("friend", "enemy"), 10, replace = TRUE)) |>
#'   to_uniplex("friend")
#' @export
to_uniplex <- function(.data, tie) UseMethod("to_uniplex")

#' @export
to_uniplex.default <- function(.data, tie){
  as_input(.data, to_uniplex, tie = tie)
}

#' @export
to_uniplex.tbl_graph <- function(.data, tie){
  out <- dplyr::filter(.data = tidygraph::activate(.data, "edges"), 
                       type == tie) |> dplyr::select(-type)
  if(is_signed(out) && all(tie_signs(out)==1) || all(is.na(tie_signs(out)))) 
    out <- out |> dplyr::select(-sign)
  if(is_weighted(out) && all(tie_weights(out)==1)) 
    out <- out |> dplyr::select(-weight)
  if(is_longitudinal(out) && length(unique(tie_attribute(out, "wave")))==1) 
    out <- out |> dplyr::select(-wave)
  if(is_twomode(out) && all(!tie_is_twomode(out))){ # if only one-mode left
    retain <- node_is_mode(out)[igraph::as_edgelist(out, names = FALSE)[1,1]]
    out <- tidygraph::activate(out, "nodes") |> 
      filter_nodes(type == retain) |> 
      mutate_nodes(type = NULL)
  }
  out <- out |> mutate_info(ties = tie)
  tidygraph::activate(out, "nodes")
}

#' @export
to_uniplex.igraph <- function(.data, tie){
  as_igraph(to_uniplex(as_tidygraph(.data), tie))
}

#' @export
to_uniplex.network <- function(.data, tie){
  as_network(to_uniplex(as_igraph(.data), tie))
}

#' @export
to_uniplex.data.frame <- function(.data, tie){
  as_edgelist(to_uniplex(as_igraph(.data), tie))
}

#' @export
to_uniplex.matrix <- function(.data, tie){
  as_matrix(to_uniplex(as_igraph(.data), tie))
}
