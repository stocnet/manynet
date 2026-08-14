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
#'   `to_layer()` is an alias, using the layer-based vocabulary of
#'   `layer_names()`, `net_layers()`, and `to_layers()`.
#'   Use `to_layers()` to split a network into all of its layers at once.
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
#'   available_methods(collect_functions("to_.*(anti|plex|layer$)"))
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

#' @rdname modif_plexity
#' @importFrom igraph simplify
#' @export
to_simplex <- function(.data) UseMethod("to_simplex")

#' @export
to_simplex.default <- function(.data) {
  as_input(.data, to_simplex)
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

# Layers are held in a 'type' tie attribute in tidygraph/igraph objects and in
# a 'layer' column in stocnet objects; both survive coercion. Note that
# layer_names() returns the names of the layers, not the tie attribute in
# which they are held, and falls back to the network's tie label where there
# are no layers, so it cannot be used to detect this.
.layer_attribute <- function(.data) {
  intersect(c("type", "layer"), net_tie_attributes(.data))[1]
}

#' @rdname modif_plexity
#' @param tie Character string naming one of the tie types, or layers,
#'   in the network, i.e. one of those returned by `layer_names()`,
#'   to which the network should be reduced.
#'   Where a network holds no tie types, it is already uniplex
#'   and is returned unchanged.
#' @examples
#' as_tidygraph(create_filled(5)) |>
#'   mutate_ties(type = sample(c("friend", "enemy"), 10, replace = TRUE)) |>
#'   to_uniplex("friend")
#' @export
to_uniplex <- function(.data, tie) UseMethod("to_uniplex")

#' @export
to_uniplex.default <- function(.data, tie) {
  as_input(.data, to_uniplex, tie = tie)
}

#' @export
to_uniplex.tbl_graph <- function(.data, tie){
  layer_attr <- .layer_attribute(.data)
  if(is.na(layer_attr)){
    snet_info("This network holds no tie types, so is already uniplex.")
    return(.data)
  }
  types <- tie_attribute(.data, layer_attr)
  ties_avail <- unique(types)
  if(missing(tie) || is.null(tie) || length(tie) != 1){
    snet_abort("Please name the tie type to which the network should be",
               "reduced, one of {.val {ties_avail}} (see {.fn layer_names}).")
  } else if(!tie %in% ties_avail){
    snet_abort("There is no tie type {.val {tie}} in this network.",
               "Please name one of {.val {ties_avail}}",
               "(see {.fn layer_names}).")
  }
  out <- delete_ties(.data, which(!types %in% tie))
  out <- delete_tie_attribute(out, layer_attr)
  if(is_signed(out) && "sign" %in% net_tie_attributes(out) &&
     (all(tie_signs(out)==1) || all(is.na(tie_signs(out)))))
    out <- delete_tie_attribute(out, "sign")
  if(is_weighted(out) && all(tie_weights(out)==1))
    out <- delete_tie_attribute(out, "weight")
  if(is_longitudinal(out) && length(unique(tie_attribute(out, "wave")))==1)
    out <- delete_tie_attribute(out, "wave")
  if(is_twomode(out) && all(!tie_is_twomode(out))){ # if only one-mode left
    retain <- node_is_mode(out)[igraph::as_edgelist(out, names = FALSE)[1,1]]
    out <- tidygraph::activate(out, "nodes") |> 
      filter_nodes(type == retain) |> 
      mutate_nodes(type = NULL)
  }
  out <- out |> mutate_info(ties = tie)
  tidygraph::activate(out, "nodes")
}

#' @rdname modif_plexity
#' @export
to_layer <- to_uniplex
