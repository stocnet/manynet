#' Modifying node labels
#' @name modif_labels
#' @description
#'   These functions add some format to manynet-consistent data.
#' 
#'   - `to_named()` reformats unlabelled network data to labelled network data 
#'   from a vector of names or random baby names.
#'   - `to_unnamed()` reformats labelled network data to unlabelled network data.
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
#'   available_methods(collect_functions("to_.*(named)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_labels
#' @param names Character vector of the node names. NULL by default.
#' @importFrom dplyr mutate
#' @importFrom igraph vcount V
#' @export
to_named <- function(.data, names = NULL) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(.data, names = NULL) {
  if (!is.null(names)) {
    out <- .data |> mutate(name = names)
  } else {
    n <- net_nodes(.data)
    out <- .data |>
      mutate(name = .get_babynames(n))
  }
  out
}

#' @export
to_named.igraph <- function(.data, names = NULL) {
  if (!is.null(names)) {
    igraph::V(.data)$name  <- names
  } else {
    igraph::V(.data)$name  <- .get_babynames(net_nodes(.data))
  }
  .data
}

#' @export
to_named.data.frame <- function(.data, names = NULL) {
  if (!is.null(names)) {
    .data[,1]  <- names[as.numeric(.data[,1])]
    .data[,2]  <- names[as.numeric(.data[,2])]
  } else {
    .data[,1]  <- .get_babynames(net_nodes(.data))[as.numeric(.data[,1])]
    .data[,2]  <- .get_babynames(net_nodes(.data))[as.numeric(.data[,2])]
  }
  .data
}

#' @export
to_named.matrix <- function(.data, names = NULL) {
  if(is.null(names)) names <- .get_babynames(net_nodes(.data))
  if(is_twomode(.data)){
    rownames(.data)  <- names[seq_len(nrow(.data))]
    colnames(.data)  <- names[(nrow(.data)+1):length(names)]
  } else {
    rownames(.data)  <- names
    colnames(.data)  <- names
  }
  .data
}

#' @export
to_named.network <- function(.data, names = NULL) {
  as_network(to_named(as_igraph(.data), names))
}

.get_babynames <- function(n){
  indic <- seq(from=1, length.out=n) %% 26
  indic[indic == 0] <- 26
  snet_info("Assigning alphabetic baby names at random.")
  # table(stringr::str_extract(manynet:::baby_names, "^."))
  vapply(indic, 
         function(x){
           let <- LETTERS[x]
           sample(baby_names[startsWith(baby_names, let)], 1)
         }, FUN.VALUE = character(1))
}

#' @rdname modif_labels
#' @importFrom igraph delete_vertex_attr
#' @importFrom tidygraph as_tbl_graph
#' @importFrom network delete.vertex.attribute
#' @importFrom dplyr as_tibble
#' @export
to_unnamed <- function(.data) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(.data) {
  if ("name" %in% igraph::vertex_attr_names(.data)) {
    igraph::delete_vertex_attr(.data, "name")
  } else .data
}

#' @export
to_unnamed.tbl_graph <- function(.data) {
  if ("name" %in% igraph::vertex_attr_names(.data)) {
    as_tidygraph(igraph::delete_vertex_attr(.data, "name"))
  } else .data
}

#' @export
to_unnamed.network <- function(.data) {
  out <- network::delete.vertex.attribute(.data, "vertex.names")
  out
}

#' @export
to_unnamed.matrix <- function(.data) {
  out <- .data
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @export
to_unnamed.data.frame <- function(.data) {
  out <- .data
  names <- unique(unlist(c(out[,1],out[,2])))
  out[,1] <- match(unlist(.data[,1]), names)
  out[,2] <- match(unlist(.data[,2]), names)
  dplyr::as_tibble(out)
}

