# Levelling ####

#' Modifying network levels
#' @name modif_levels
#' @description
#'   These functions reformat the levels in manynet-consistent network data.
#' 
#'   - `to_onemode()` reformats two-mode network data into one-mode network data by simply removing the nodeset 'type' information.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`.
#'   - `to_twomode()` reformats one-mode network data into two-mode network data, using a mark to distinguish the two sets of nodes.
#'   - `to_multilevel()` reformats two-mode network data into multimodal network data, which allows for more levels and ties within modes.
#' 
#'   If the format condition is not met,
#'   for example `to_onemode()` is used on a network that is already one-mode,
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
#'   available_methods(collect_functions("to_.*(onemode|twomode|level)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_levels
#' @importFrom igraph delete_vertex_attr vertex_attr_names
#' @export
to_onemode <- function(.data) UseMethod("to_onemode")

#' @export
to_onemode.default <- function(.data){
  as_input(.data, to_onemode)
}

#' @export
to_onemode.matrix <- function(.data) {
  if (is_twomode(.data)){
    .data <- rbind(cbind(matrix(0, nrow(.data), nrow(.data)), .data),
                   cbind(t(.data), matrix(0, ncol(.data), ncol(.data))))
    colnames(.data) <- rownames(.data)
  }
  .data
}

#' @export
to_onemode.tbl_graph <- function(.data) {
  as_tidygraph(to_onemode(as_igraph(.data)))
}

#' @export
to_onemode.igraph <- function(.data) {
  if ("type" %in% igraph::vertex_attr_names(.data)) 
    .data <- igraph::delete_vertex_attr(.data, "type")
  .data
}

#' @rdname modif_levels
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph V
#' @export
to_twomode <- function(.data, mark) UseMethod("to_twomode")

#' @export
to_twomode.default <- function(.data, mark){
  as_input(.data, to_twomode, mark)
}

#' @export
to_twomode.igraph <- function(.data, mark){
  igraph::V(.data)$type <- mark
  to_undirected(.data)
}

#' @export
to_twomode.tbl_graph <- function(.data, mark){
  as_tidygraph(to_twomode.igraph(.data, mark))
}

#' @export
to_twomode.network <- function(.data, mark){
  as_network(to_twomode(as_igraph(.data), mark), twomode = TRUE)
}

#' @rdname modif_levels 
#' @importFrom igraph V delete_vertex_attr
#' @export
to_multilevel <- function(.data) UseMethod("to_multilevel")

#' @export
to_multilevel.default <- function(.data){
  as_input(.data, to_multilevel)
}

#' @export
to_multilevel.tbl_graph <- function(.data) {
  as_tidygraph(to_multilevel(as_igraph(.data)))
}

#' @export
to_multilevel.igraph <- function(.data) {
  if(is_twomode(.data)){
    igraph::V(.data)$lvl <- ifelse(igraph::V(.data)$type, 2, 1)
    .data <- igraph::delete_vertex_attr(.data, "type")
  }
  .data
}

#' @export
to_multilevel.matrix <- function(.data) {
  top <- cbind(matrix(0, nrow(.data), nrow(.data)), .data)
  bottom <- cbind(t(.data), matrix(0, ncol(.data), ncol(.data)))
  out <- rbind(top, bottom)
  colnames(out) <- rownames(out)
  out
}

