# Dimensions ####

#' Describing network dimensions
#' @name measure_dims
#' @description 
#'   These functions extract certain attributes from given network data:
#'   
#'   - `net_nodes()` returns the total number of nodes (of any mode) in a network.
#'   - `net_ties()` returns the number of ties in a network.
#'   - `mode_nodes()` returns the dimensions of a network in a vector
#'   as long as the number of modes in the network.
#'   - `net_layers()` returns the number of layers in a multiplex network.
#'   - `layer_ties()` returns the number of ties in a vector
#'   as long as the number of layers in the network.
#'   - `net_waves()` returns the number of waves a panel network records,
#'   see [is_longitudinal()]. A network that is not a panel has one wave.
#'   - `net_times()` returns the number of distinct moments a network records,
#'   however it records them: the waves of a panel, the events of a dynamic
#'   network, or the moments an interval network begins and ends a tie at.
#'   See the Time section of [to_time()].
#'
#'   These functions are also often used as helpers within other functions.
#' @return `net_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `mode_nodes()` returns an integer of the number of nodes in a one-mode network,
#'   or two integers representing the number of nodes in each nodeset
#'   in the case of a two-mode network.
#'   `layer_ties()` returns an integer of the number of ties in a single-layer
#'   network, or one integer per layer (in `layer_names()` order)
#'   in the case of a multiplex network.
#' @family measures
#' @template param_data
NULL

#' @rdname measure_dims
#' @examples
#' net_nodes(ison_southern_women)
#' @export
net_nodes <- function(.data) UseMethod("net_nodes")

#' @export
net_nodes.default <- function(.data){
  net_nodes(as_igraph(.data))
}

#' @export
net_nodes.stocnet <- function(.data){
  dplyr::coalesce(nrow(.data$nodes),
                  length(unique(c(.data$ties$from, .data$ties$to))))
}

#' @export
net_nodes.matrix <- function(.data){
  if(is_twomode(.data)){
    sum(dim(.data))
  } else nrow(.data)
}

#' @export
net_nodes.igraph <- function(.data){
  if(is_list(.data)){
    nodes <- vapply(.data, function(x) igraph::vcount(as_igraph(x)), 
                    FUN.VALUE = numeric(1))
    make_network_measure(max(nodes), .data[[1]], call = deparse(sys.call()))
  } else make_network_measure(igraph::vcount(as_igraph(.data)), .data, 
                              call = deparse(sys.call()))
}

#' @export
net_nodes.network <- function(.data){
  network::network.size(.data)
}

#' @export
net_nodes.data.frame <- function(.data){
  length(unique(c(.data$from, .data$to)))
}

#' @rdname measure_dims
#' @examples
#' net_modes(ison_southern_women)
#' @export
net_modes <- function(.data) UseMethod("net_modes")

#' @export
net_modes.default <- function(.data){
  net_modes(as_igraph(.data))
}

#' @export
net_modes.stocnet <- function(.data){
  if("mode" %in% names(.data$nodes)){
    length(unique(.data$nodes$mode))
  } else 1L
}

#' @export
net_modes.igraph <- function(.data){
  if(is_twomode(.data)) 2L else 1L
}

#' @rdname measure_dims
#' @examples
#' net_ties(ison_southern_women)
#' @export
net_ties <- function(.data) UseMethod("net_ties")

#' @export
net_ties.default <- function(.data){
  net_ties(as_igraph(.data))
}

#' @export
net_ties.stocnet <- function(.data){
  nrow(.data$ties)
}

#' @export
net_ties.igraph <- function(.data){
  make_network_measure(igraph::ecount(.data), .data,
                       call = deparse(sys.call()))
}

#' @export
net_ties.network <- function(.data){
  # A 'network' object is the one class that holds the ties it records as
  # missing among its edges, so those are omitted here, as `{network}` omits
  # them itself. `net_tie_missing()` counts them instead.
  make_network_measure(network::network.edgecount(.data), .data,
                       call = deparse(sys.call()))
}

#' @rdname measure_dims
#' @examples
#' net_layers(ison_southern_women)
#' @export
net_layers <- function(.data) UseMethod("net_layers")

#' @export
net_layers.default <- function(.data){
  net_layers(as_igraph(.data))
}

#' @export
net_layers.stocnet <- function(.data){
  if("layer" %in% names(.data$ties)){
    length(unique(.data$ties$layer))
  } else 1L
}

#' @export
net_layers.igraph <- function(.data){
  if("type" %in% net_tie_attributes(.data)){
    length(unique(tie_attribute(.data, "type")))
  } else 1L
}

#' @rdname measure_dims
#' @examples
#' layer_ties(fict_marvel)
#' @export
layer_ties <- function(.data) UseMethod("layer_ties")

#' @export
layer_ties.default <- function(.data){
  layer_ties(as_igraph(.data))
}

#' @export
layer_ties.igraph <- function(.data){
  types <- if("type" %in% net_tie_attributes(.data))
    tie_attribute(.data, "type") else NULL
  .layer_ties(layer_names(.data), types, net_ties(.data))
}

#' @export
layer_ties.stocnet <- function(.data){
  types <- .data$ties[["type"]] %||% .data$ties[["layer"]]
  .layer_ties(layer_names(.data), types, net_ties(.data))
}

# Counts ties per layer, aligned to `lnames`.
# Only subdivides by the `type`/`layer` values when these correspond one-to-one
# with the layer names; otherwise (e.g. a single curated layer name grouping
# finer tie types, as in `fict_thrones`) returns the total tie count.
.layer_ties <- function(lnames, types, nt){
  if(is.null(lnames) || is.null(types)) return(as.integer(nt))
  utypes <- unique(types)
  if(length(utypes) == length(lnames) && all(utypes %in% lnames)){
    as.integer(table(factor(types, levels = lnames)))
  } else as.integer(nt)
}

#' @rdname measure_dims
#' @examples
#' net_waves(ison_monks)
#' @export
net_waves <- function(.data) UseMethod("net_waves")

#' @export
net_waves.default <- function(.data){
  net_waves(as_igraph(.data))
}

#' @export
net_waves.igraph <- function(.data){
  # A network that is not a panel holds one observation of itself. The waves
  # are counted from the ties alone: a change recorded after the last wave
  # states what became of a node, and does not add a wave to observe it in.
  if(!is_longitudinal(.data)) return(1L)
  moments <- .time_moments(.data, changes = FALSE)
  if(is.null(moments)) 1L else length(moments)
}

#' @export
net_waves.stocnet <- net_waves.igraph

#' @rdname measure_dims
#' @examples
#' net_times(irps_wwi)
#' @export
net_times <- function(.data) UseMethod("net_times")

#' @export
net_times.default <- function(.data){
  net_times(as_igraph(.data))
}

#' @export
net_times.igraph <- function(.data){
  # A network that records no moment records itself at one.
  moments <- .time_moments(.data)
  if(is.null(moments)) 1L else length(moments)
}

#' @export
net_times.stocnet <- net_times.igraph

#' @rdname measure_dims
#' @examples
#' mode_nodes(ison_southern_women)
#' mode_nodes(to_mode1(ison_southern_women))
#' @export
mode_nodes <- function(.data) UseMethod("mode_nodes")

#' @export
mode_nodes.default <- function(.data){
  mode_nodes(as_igraph(.data))
}

#' @export
mode_nodes.data.frame <- function(.data){
  if(is_twomode(.data)){
    c(length(unique(.data[,1])),
      length(unique(.data[,2])))
  } else {
    length(unique(c(.data[,1], .data[,2])))
  }
}

#' @export
mode_nodes.matrix <- function(.data){
  if(is_twomode(.data)){
    c(nrow(.data),
      ncol(.data))
  } else {
    nrow(.data)
  }
}

#' @export
mode_nodes.igraph <- function(.data){
  if(is_twomode(.data)){
    c(sum(!igraph::V(.data)$type),
      sum(igraph::V(.data)$type))
  } else {
    igraph::vcount(.data)
  }
}

#' @importFrom network network.size get.network.attribute
#' @export
mode_nodes.network <- function(.data){
  out <- network::network.size(.data)
  if(is_twomode(.data)){
    bip1 <- network::get.network.attribute(as_network(.data),
                                           "bipartite")
    out <- c(bip1, out - bip1)
  }
  out
}

#' @export
mode_nodes.stocnet <- function(.data){
  if(is_twomode(.data)){
    out <- tabulate(match(.data$nodes$mode, unique(.data$nodes$mode)))
  } else net_nodes(.data)
}

#' @rdname measure_dims
#' @export
net_dims <- mode_nodes

# Names ####

#' Describing network names
#' @name member_names
#' @description 
#'   These functions extract certain attributes from given network data:
#'   
#'   - `net_name()` returns the name of the network, if it has one.
#'   - `mode_names()` returns a vector of the names of the modes in a network,
#'   if they have been defined.
#'   - `net_node_attributes()` returns a vector of nodal attributes in a network.
#'   - `layer_names()` returns a vector of the names of the layers in a network,
#'   if they have been defined.
#'   - `net_tie_attributes()` returns a vector of tie attributes in a network.
#'   
#'   These functions are also often used as helpers within other functions.
#' @return `net_*()` functions always relate to the overall graph or network,
#'   usually returning a scalar.
#'   `net_*_attributes()` returns a string vector with the names
#'   of all node or tie attributes in the network.
#' @family attributes
#' @template param_data
NULL

#' @rdname member_names
#' @param prefix An optional string to be added before the name of the network.
#' @examples
#' net_name(ison_southern_women)
#' @export
net_name <- function(.data, prefix = NULL) UseMethod("net_name")

#' @export
net_name.default <- function(.data, prefix = NULL){
  net_name(as_igraph(.data), prefix = prefix)
}

#' @export
net_name.stocnet <- function(.data, prefix = NULL){
  existname <- ""
  if(!is.null(.data$info$name)) {
    existname <- .data$info$name
  }
  if(existname != "" && !is.null(prefix)) existname <- paste(prefix, existname)
  existname  
}

#' @export
net_name.igraph <- function(.data, prefix = NULL){
  existname <- ""
  if(!is.null(igraph::graph_attr(.data, "name"))) {
    existname <- igraph::graph_attr(.data, 'name')
  } else if(is_grand(.data) && 
            !is.null(igraph::graph_attr(.data, "grand")$name)){
    existname <- igraph::graph_attr(.data, 'grand')$name
  }
  if(existname != "" && !is.null(prefix)) existname <- paste(prefix, existname)
  existname
}

#' @export
net_name.network <- function(.data, prefix = NULL){
  existname <- ""
  if(!is.null(igraph::graph_attr(.data, "name"))) {
    existname <- igraph::graph_attr(.data, 'name')
  } else if(is_grand(.data) && 
            !is.null(igraph::graph_attr(.data, "grand")$name)){
    existname <- igraph::graph_attr(.data, 'grand')$name
  }
  if(existname != "" && !is.null(prefix)) existname <- paste(prefix, existname)
  existname
}


#' @rdname member_names
#' @importFrom igraph graph_attr
#' @examples
#'   mode_names(ison_algebra)
#' @export
mode_names <- function(.data) UseMethod("mode_names")

#' @export
mode_names.default <- function(.data){
  mode_names(as_igraph(.data))
}

#' @export
mode_names.igraph <- function(.data){
  igraph::graph_attr(.data, "nodes") %||%
    c(igraph::graph_attr(.data, "grand")$vertex1,
      igraph::graph_attr(.data, "grand")$vertex2)
}

#' @export
mode_names.stocnet <- function(.data){
  out <- .data$info$modes %||%
    (if(!is.null(.data$nodes) && "mode" %in% names(.data$nodes))
      unique(.data$nodes$mode))
  out <- out[!is.na(out)]
  if(length(out) == 0) NULL else out
}

#' @rdname member_names
#' @importFrom igraph vertex_attr_names
#' @examples
#'   net_node_attributes(fict_lotr)
#' @export
net_node_attributes <- function(.data) UseMethod("net_node_attributes")

#' @export
net_node_attributes.default <- function(.data){
  net_node_attributes(as_igraph(.data))
}

#' @export
net_node_attributes.igraph <- function(.data){
  igraph::vertex_attr_names(.data)
}

#' @export
net_node_attributes.stocnet <- function(.data){
  names(.data$nodes)
}

#' @export
net_node_attributes.network <- function(.data){
  network::list.vertex.attributes(.data)
}

#' @rdname member_names
#' @importFrom igraph graph_attr
#' @examples
#'   layer_names(ison_algebra)
#' @export
layer_names <- function(.data) UseMethod("layer_names")

#' @export
layer_names.default <- function(.data){
  layer_names(as_igraph(.data))
}

#' @export
layer_names.igraph <- function(.data){
  igraph::graph_attr(.data, "ties") %||%
    igraph::graph_attr(.data, "layers") %||%
    c(igraph::graph_attr(.data, "grand")$edge.pos,
      igraph::graph_attr(.data, "grand")$edge.neg) %||%
    (if (is_multiplex(.data) && "type" %in% igraph::edge_attr_names(.data))
      unique(igraph::edge_attr(.data, "type")))
}

#' @export
layer_names.stocnet <- function(.data){
  .data$info$layers %||% unique(.data$ties[["layer"]])
}

#' @rdname member_names
#' @importFrom igraph edge_attr_names
#' @examples
#'   net_tie_attributes(ison_algebra)
#' @export
net_tie_attributes <- function(.data) UseMethod("net_tie_attributes")

#' @export
net_tie_attributes.default <- function(.data){
  net_tie_attributes(as_igraph(.data))
}

#' @export
net_tie_attributes.igraph <- function(.data){
  igraph::edge_attr_names(.data)
}

#' @export
net_tie_attributes.stocnet <- function(.data){
  names(.data$ties)
}

#' @export
net_tie_attributes.network <- function(.data){
  network::list.edge.attributes(.data)
}

# Missingness ####

#' Describing network missingness
#' @name measure_missingness
#' @description
#'  These functions describe the missingness in network data:
#'  - `net_node_missing()` returns the proportion of nodes that are missing in a network.
#'  - `net_node_incomplete()` returns the proportion of the network's node
#'  attribute values that are unknown.
#'  - `net_tie_missing()` returns the proportion of ties that are missing in a network.
#'  - `net_tie_incomplete()` returns the proportion of the network's ties whose
#'  value is unknown.
#'
#'  A network is *missing* a tie where the tie itself was not observed,
#'  so that whether it exists is not known.
#'  A tie or a node is *incomplete* where it is there and observed,
#'  but an attribute of it is not known.
#'  A weight of `NA` therefore marks an incomplete tie and not a missing one.
#'  [impute_ties()] and [impute_nodes()] impute each of these states.
#'
#'  A tie recorded as missing is one that could have been observed and was not.
#'  It is not a tie, so `net_ties()` does not count it,
#'  and it is not the absence of a tie either.
#'  See [as_missinglist()] for how each class records them,
#'  and `make_stocnet()` for how they differ from a node's absence
#'  and from a tie of unknown value.
#'
#'  For a multiplex or longitudinal network, `net_tie_missing()` counts the ties
#'  that could have been observed over each layer and each moment the network
#'  records. Coercing such a network to a matrix first gives a higher
#'  proportion, since a matrix holds only one cell for each dyad.
#' @family missingness
#' @template param_data
#' @return `net_node_missing()`, `net_tie_missing()`, `net_node_incomplete()`,
#'   and `net_tie_incomplete()` return a scalar.
NULL

#' @rdname measure_missingness
#' @export
net_node_missing <- function(.data) UseMethod("net_node_missing")

#' @export
net_node_missing.default <- function(.data){
  net_node_missing(as_stocnet(.data))
}

#' @export
net_node_missing.stocnet <- function(.data){
  if(!"na" %in% names(.data$nodes)) return(0)
  mean(.data$nodes$na)
}

#' @rdname measure_missingness
#' @export
net_tie_missing <- function(.data) UseMethod("net_tie_missing")

#' @export
net_tie_missing.default <- function(.data){
  net_tie_missing(as_matrix(.data))
}

#' @export
net_tie_missing.matrix <- function(.data){
  mean(is.na(.data)) %||% 0
}

#' @export
net_tie_missing.stocnet <- function(.data){
  miss <- nrow(as_missinglist(.data))
  if(is.null(miss) || miss == 0) return(0)
  miss / .stocnet_dyads(.data)
}

# Incompleteness ####

# A network's node attributes, without the columns that are bookkeeping rather
# than something observed about a node.
.node_attribute_table <- function(.data){
  nodes <- if(inherits(.data, "stocnet")) .data$nodes else
    tibble::as_tibble(as_tidygraph(.data), active = "nodes")
  nodes[setdiff(names(nodes), manynet_reserved_node_attributes)]
}

# Structural or bookkeeping tie attributes that do not count as substantive
# attributes. A weight is not among them, since a weight of `NA` marks a tie
# whose value is not known and so an incomplete tie.
manynet_reserved_tie_attributes <- c("from", "to", "by", "time", "wave",
                                     "begin", "end", "layer", "na")

# A network's tie attributes, without the columns that are bookkeeping rather
# than something observed about a tie.
.tie_attribute_table <- function(.data){
  ties <- if(inherits(.data, "stocnet")) .data$ties else
    tibble::as_tibble(as_tidygraph(.data), active = "edges")
  ties[setdiff(names(ties), manynet_reserved_tie_attributes)]
}

# For each tie, the proportion of its attribute values that are not known.
# A network with no tie attributes gives a zero for each tie, since nothing
# about those ties is unknown.
tie_incomplete <- function(.data){
  ties <- .tie_attribute_table(.data)
  if(!ncol(ties)) return(rep(0, net_ties(.data)))
  rowMeans(is.na(as.data.frame(ties)))
}

#' @rdname measure_missingness
#' @examples
#' net_node_incomplete(fict_lotr)
#' @export
net_node_incomplete <- function(.data) UseMethod("net_node_incomplete")

#' @export
net_node_incomplete.default <- function(.data){
  nodes <- .node_attribute_table(.data)
  if(!ncol(nodes)) return(0)
  mean(is.na(as.data.frame(nodes)))
}

#' @rdname measure_missingness
#' @examples
#' net_tie_incomplete(ison_adolescents)
#' @export
net_tie_incomplete <- function(.data) UseMethod("net_tie_incomplete")

#' @export
net_tie_incomplete.default <- function(.data){
  out <- tie_incomplete(.data)
  if(!length(out)) return(0)
  mean(out)
}

# How many ties a stocnet could have observed on one occasion, an occasion
# being one layer at one moment.
.stocnet_dyads_each <- function(.data) .dyads_possible(.data)

# How many ties a stocnet could have observed altogether. Counted over each
# layer and each moment the network records, since a matrix holds one cell per
# dyad and so cannot hold the several ties a multiplex or longitudinal network
# holds for each of them.
.stocnet_dyads <- function(.data){
  cols <- intersect(c("layer", "time"), names(.data$ties))
  occasions <- if(length(cols)) nrow(unique(.data$ties[cols])) else 1L
  .stocnet_dyads_each(.data) * max(1L, occasions)
}

