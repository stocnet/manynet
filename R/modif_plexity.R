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
#'   - `to_flat()` reduces multiplex network data to a single relation by
#'   combining the values of all its layers, dyad by dyad, according to a rule.
#'   Where `to_uniplex()` selects one layer and discards the rest,
#'   `to_flat()` retains what every layer records.
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
#'   available_methods(collect_functions("to_.*(anti|plex|layer$|flat$)"))
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
to_simplex.tbl_graph <- function(.data) {
  # the record is written here rather than in the igraph method because
  # `.record_transformation()` returns a 'tbl_graph', which would change what
  # the igraph method gives back to the methods that delegate to it
  as_tidygraph(to_simplex(as_igraph(.data))) |>
    .record_exclusion(.data, "loops and multiple ties", "ties")
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
#' @param layer Character string naming one of the layers, or tie types,
#'   in the network, i.e. one of those returned by `layer_names()`,
#'   to which the network should be reduced.
#'   Where a network holds no tie types, it is already uniplex
#'   and is returned unchanged.
#' @param tie Deprecated name for `layer`, retained for one version.
#' @examples
#' as_tidygraph(create_filled(5)) |>
#'   mutate_ties(type = sample(c("friend", "enemy"), 10, replace = TRUE)) |>
#'   to_uniplex("friend")
#' @export
to_uniplex <- function(.data, layer, tie) UseMethod("to_uniplex")

#' @export
to_uniplex.default <- function(.data, layer, tie) {
  # `tie` was the name of this argument until version 2.3.0, when it was
  # renamed to agree with `to_layers()`, `from_layers()`, and `layer_names()`
  if(missing(layer) && !missing(tie)){
    snet_warn("The {.arg tie} argument is now called {.arg layer}.")
    layer <- tie
  } else if(missing(layer)) layer <- NULL
  as_input(.data, to_uniplex, layer = layer)
}

#' @export
to_uniplex.tbl_graph <- function(.data, layer, tie){
  if(missing(layer) && !missing(tie)){
    snet_warn("The {.arg tie} argument is now called {.arg layer}.")
    layer <- tie
  } else if(missing(layer)) layer <- NULL
  layer_attr <- .layer_attribute(.data)
  if(is.na(layer_attr)){
    snet_info("This network holds no tie types, so is already uniplex.")
    return(.data)
  }
  types <- tie_attribute(.data, layer_attr)
  ties_avail <- unique(types)
  if(is.null(layer) || length(layer) != 1){
    snet_abort("Please name the layer to which the network should be",
               "reduced, one of {.val {ties_avail}} (see {.fn layer_names}).")
  } else if(!layer %in% ties_avail){
    snet_abort("There is no layer {.val {layer}} in this network.",
               "Please name one of {.val {ties_avail}}",
               "(see {.fn layer_names}).")
  }
  out <- delete_ties(.data, which(!types %in% layer))
  out <- delete_tie_attribute(out, layer_attr)
  if(is_signed(out) && "sign" %in% net_tie_attributes(out) &&
     (all(tie_signs(out)==1) || all(is.na(tie_signs(out)))))
    out <- delete_tie_attribute(out, "sign")
  # Weights of nothing but ones record no more than the ties themselves do,
  # unless some of them are missing, which records which ties are missing
  if("weight" %in% net_tie_attributes(out) && !anyNA(tie_weights(out)) &&
     all(tie_weights(out)==1))
    out <- delete_tie_attribute(out, "weight")
  if(is_longitudinal(out) && length(unique(tie_attribute(out, "wave")))==1)
    out <- delete_tie_attribute(out, "wave")
  if(is_twomode(out) && all(!tie_is_twomode(out))){ # if only one-mode left
    retain <- node_is_mode(out)[igraph::as_edgelist(out, names = FALSE)[1,1]]
    out <- tidygraph::activate(out, "nodes") |> 
      filter_nodes(type == retain) |> 
      mutate_nodes(type = NULL)
  }
  out <- out |> mutate_info(ties = layer)
  out <- .retain_layer_info(out, layer, setdiff(ties_avail, layer))
  # the ties of the other layers and, where a two-mode network is left with
  # one mode, the nodes of the mode that went with them, are two exclusions by
  # two criteria, so each is recorded on its own
  out <- .record_exclusion(out, .data,
                           paste0("layers other than '", layer, "'"), "ties") |>
    .record_exclusion(.data, paste0("not tied by '", layer, "'"), "nodes")
  # A network with both directed and undirected layers is directed as a whole,
  # and holds its undirected layers as reciprocated arcs. Once only such a
  # layer is left, the result is an undirected network, so the arcs collapse
  # back to one tie per dyad.
  if(isFALSE(unname(igraph::graph_attr(out, "directed")[layer])))
    out <- to_undirected(out)
  tidygraph::activate(out, "nodes")
}

# Reduce the per-layer info to the one layer that `to_uniplex()` retains.
# Without this, 'layers' and the named 'directed'/'observation'/'update'
# vectors keep describing layers that are no longer in the network, which
# `validate_stocnet()` rejects once the result is coerced back to a stocnet.
.retain_layer_info <- function(.data, layer, dropped){
  out <- .data
  fields <- intersect(c("layers", "directed", "observation", "update", "focal"),
                      igraph::graph_attr_names(out))
  info <- igraph::graph_attr(out)[fields]
  # `.prune_layer_info()` reads the layers from the information it prunes, so
  # where the network names none they are named here for it, and the network
  # is left without them again afterwards.
  had_layers <- "layers" %in% fields
  if(!had_layers) info$layers <- unique(c(layer, dropped))
  info <- .prune_layer_info(info, layer)
  if(!had_layers) info$layers <- NULL
  for(field in fields) igraph::graph_attr(out, field) <- info[[field]]
  out
}

#' @rdname modif_plexity
#' @export
to_layer <- to_uniplex

#' @rdname modif_plexity
#' @template param_rule
#' @examples
#' to_flat(ison_florentine, rule = "sum")
#' @export
to_flat <- function(.data, rule = c("max","min","mean","sum",
                                    "product")) UseMethod("to_flat")

#' @export
to_flat.default <- function(.data, rule = c("max","min","mean","sum",
                                            "product")) {
  rule <- match.arg(rule)
  as_input(.data, to_flat, rule = rule)
}

#' @export
to_flat.tbl_graph <- function(.data, rule = c("max","min","mean","sum",
                                              "product")) {
  rule <- match.arg(rule)
  layers <- to_layers(.data)
  if(length(layers) > 1) return(.combine_networks(layers, rule))
  # `join_ties()` marks each network's ties in a column of its own rather than
  # naming them all in one 'type' column, and `is_multiplex()` counts such a
  # network as multiplex, so those columns are its layers
  marks <- setdiff(net_tie_attributes(.data), reserved_tie_attr)
  if(length(marks) > 1) return(.combine_marks(.data, marks, rule))
  # a network holding no tie types is already flat; `to_layers()` says so
  .data
}

# Combining ####

# Where the layers are marked one column each, as `join_ties()` marks them,
# each column gives the tie values of one layer. A tie the column does not
# mark is untied in that layer.
.combine_marks <- function(.data, marks, rule){
  el <- as_edgelist(.data)
  n <- as.numeric(net_nodes(.data))
  # an edgelist names its nodes only where the network is labelled
  labels <- if(is_labelled(.data)) node_names(.data) else NULL
  idx <- if(is.character(el$from))
    cbind(match(el$from, labels), match(el$to, labels)) else
      cbind(as.integer(el$from), as.integer(el$to))
  mats <- lapply(marks, function(m){
    v <- el[[m]]
    v[is.na(v)] <- 0
    out <- matrix(0, n, n, dimnames = if(is.null(labels)) NULL else
      list(labels, labels))
    out[idx] <- v
    if(!is_directed(.data)) out[idx[, 2:1, drop = FALSE]] <- v
    out
  })
  out <- .combine_networks(mats, rule)
  as_tidygraph(out) |> bind_node_attributes(.data) |>
    .record_transformation("aggregation", paste0("layers (", rule, ")"))
}

# Reconciles networks' tie values into a single value per dyad, cell by cell,
# and returns the result in the class of the first network given. This is the
# engine behind `to_flat()`, which combines one network's layers, and
# `join_ties(rule = )`, which combines two networks.
.combine_networks <- function(netlist, rule){
  first <- netlist[[1]]
  out <- Reduce(function(x, y) .combine_matrices(x, y, rule),
                lapply(netlist, as_matrix))
  if(is.matrix(first)) return(out)
  if(is.data.frame(first) && !inherits(first, "stocnet"))
    return(as_edgelist(out))
  net <- bind_node_attributes(as_tidygraph(out), as_tidygraph(first)) |>
    .record_transformation("aggregation", paste0("layers (", rule, ")"))
  if(inherits(first, "stocnet")) as_stocnet(net)
  else if(inherits(first, "network")) as_network(net)
  else if(inherits(first, "tbl_graph")) net
  else if(inherits(first, "igraph")) as_igraph(net)
  else net
}

.combine_matrices <- function(x, y, rule) {
  # nodes are matched by name where both networks are labelled, since two
  # networks recording the same nodes in a different order would otherwise
  # be combined cell by cell and give a result for dyads that do not exist.
  # Combining over the union of the two node sets also means an edgelist,
  # which carries only the nodes that happen to be tied, can be combined
  # with one that names a different subset of the same nodes.
  if(is_labelled(x) && is_labelled(y)){
    rn <- union(rownames(x), rownames(y))
    cn <- union(colnames(x), colnames(y))
    if(!setequal(rownames(x), rownames(y)) ||
       !setequal(colnames(x), colnames(y)))
      snet_info(paste0("The networks name different nodes, ",
                       "so they are combined over all {length(rn)} of them, ",
                       "counting a node absent from one as untied there."))
    x <- .align_matrix(x, rn, cn)
    y <- .align_matrix(y, rn, cn)
  } else if(!identical(dim(x), dim(y)))
    snet_abort(paste0("The two networks must be the same size, ",
                      "or else labelled so that their nodes can be matched, ",
                      "but they are {nrow(x)}x{ncol(x)} and ",
                      "{nrow(y)}x{ncol(y)}."))
  out <- switch(rule,
                "max"     = pmax(x, y),
                "min"     = pmin(x, y),
                "mean"    = (x + y)/2,
                "sum"     = x + y,
                "product" = x * y)
  # `pmin()` and `pmax()` return a vector, so the shape is restored here
  matrix(out, nrow(x), ncol(x), dimnames = dimnames(x))
}

# Places a matrix into one spanning the given row and column names, so that
# two networks over overlapping but unequal node sets can be combined. Nodes
# a matrix does not name are absent from it, and so untied.
.align_matrix <- function(x, rn, cn){
  out <- matrix(0, length(rn), length(cn), dimnames = list(rn, cn))
  out[rownames(x), colnames(x)] <- x
  out
}
