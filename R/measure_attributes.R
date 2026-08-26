# Node attributes ####

#' Describing attributes of nodes in a network
#' @name measure_attributes_nodes
#' @description 
#'   These functions extract certain attributes from network data:
#'   
#'   - `node_attribute()` returns an attribute's values for the nodes in a network.
#'   - `node_labels()` returns the names of the nodes in a network.
#'   - `node_is_mode()` returns the mode of the nodes in a network.
#'   
#'   These functions are also often used as helpers within other functions.
#'   `node_*()` always return vectors the same length
#'   as the number of nodes in the network.
#' @family measures
#' @template param_data
#' @template param_attr
NULL

#' @rdname measure_attributes_nodes
#' @examples
#' node_attribute(fict_lotr, "Race")
#' @export
node_attribute <- function(.data, attr_name) UseMethod("node_attribute")

#' @export
node_attribute.default <- function(.data, attr_name){
  out <- igraph::vertex_attr(as_igraph(.data), attr_name)
  if(is.numeric(out)) make_node_measure(out, .data) else out
}

#' @export
node_attribute.stocnet <- function(.data, attr_name){
  # `igraph::vertex_attr()` returns every attribute where none is named,
  # so every other class returns every attribute there too.
  if(missing(attr_name)) return(as.list(.data$nodes))
  out <- .data$nodes[[attr_name]]
  if(is.numeric(out)) make_node_measure(out, .data) else out
}

#' @export
node_attribute.network <- function(.data, attr_name){
  if(missing(attr_name)) return(.all_attributes(.data, node_attribute,
                                                net_node_attributes(.data)))
  network::get.vertex.attribute(.data, attr_name)
}

#' @rdname measure_attributes_nodes
#' @examples
#' node_labels(ison_southern_women)
#' @export
node_labels <- function(.data){
  if(is_labelled(.data)){
    igraph::vertex_attr(as_igraph(.data), "name")
  } else {
    indices <- seq.int(net_nodes(.data))
    paste0("N", gsub("\\s", "0", format(indices, width=max(nchar(indices)))))
  }
}

#' @rdname measure_attributes_nodes
#' @export
node_names <- node_labels

#' @rdname measure_attributes_nodes
#' @examples 
#' node_is_mode(ison_southern_women)
#' @export
node_is_mode <- function(.data){
  if(is_twomode(.data)){
    out <- igraph::vertex_attr(as_igraph(.data), "type")
  } else{
    out <- rep(FALSE, net_nodes(.data))
  }
  # cannot use make_node_mark here because then eternal loop
  class(out) <- c("node_mark", class(out))
  if(is.null(names(out)) & is_labelled(.data))
    names(out) <- node_labels(.data)
  attr(out, "mode") <- out
  out
}

# Tie attributes ####

#' Describing attributes of ties in a network
#' @name measure_attributes_ties
#' @description 
#'   These functions extract certain attributes from network data:
#'   
#'   - `tie_attribute()` returns an attribute's values for the ties in a network.
#'   - `tie_weights()` returns the weights of the ties in a network.
#'   - `tie_signs()` returns the signs of the ties in a network.
#'   - `tie_is_twomode()` returns whether each tie in a network is a cross-mode tie. 
#'   - `tie_is_parallel()` returns whether each tie in a network runs parallel
#'   to another, i.e. whether another tie joins the same pair of nodes at the
#'   same moment.
#'   - `tie_is_backbone()` returns whether each tie in a network is retained by
#'   a backbone filter, i.e. whether it carries more weight, or holds more
#'   structure, than a null model local to its endpoints expects.
#'   
#'   These functions are also often used as helpers within other functions.
#'   `tie_*()` always return vectors the same length
#'   as the number of ties in the network, respectively.
#' @family measures
#' @template param_data
#' @template param_attr
NULL

#' @rdname measure_attributes_ties
#' @examples
#' tie_attribute(ison_algebra, "task_tie")
#' @export
tie_attribute <- function(.data, attr_name) UseMethod("tie_attribute")

#' @export
tie_attribute.default <- function(.data, attr_name){
  out <- igraph::edge_attr(as_igraph(.data), attr_name)
  if(is.numeric(out)) make_tie_measure(out, .data) else out
}

#' @export
tie_attribute.stocnet <- function(.data, attr_name){
  # `igraph::edge_attr()` returns every attribute where none is named,
  # so every other class returns every attribute there too.
  # 'from' and 'to' identify a tie rather than describe it, so they are dropped.
  if(missing(attr_name))
    return(as.list(.data$ties[setdiff(names(.data$ties), c("from", "to"))]))
  out <- .data$ties[[attr_name]]
  if(is.numeric(out)) make_tie_measure(out, .data) else out
}

#' @export
tie_attribute.network <- function(.data, attr_name){
  if(missing(attr_name)) return(.all_attributes(.data, tie_attribute,
                                                net_tie_attributes(.data)))
  network::get.edge.attribute(.data, attr_name)
}

# Collects every named attribute into a list, as `igraph::vertex_attr()` and
# `igraph::edge_attr()` do where the caller names no attribute.
.all_attributes <- function(.data, FUN, attr_names){
  stats::setNames(lapply(attr_names, function(x) FUN(.data, x)), attr_names)
}

#' @rdname measure_attributes_ties
#' @examples
#' tie_weights(to_mode1(ison_southern_women))
#' @export
tie_weights <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::edge_attr(.data, "weight")
  if(is.null(out)) out <- rep(1, net_ties(.data))
  make_tie_measure(out, .data)
}

#' @rdname measure_attributes_ties
#' @examples 
#' tie_signs(to_uniplex(fict_marvel,"relationship"))
#' @export
tie_signs <- function(.data){
  .data <- as_igraph(.data)
  out <- igraph::edge_attr(.data, "sign")
  # signs can also be held as negative weights, as they are in 'stocnet'
  # objects, in which case the sign is the sign of the weight
  if(is.null(out) && is_signed(.data))
    out <- sign(igraph::edge_attr(.data, "weight"))
  if(is.null(out)) out <- rep(1, net_ties(.data))
  make_tie_measure(out, .data)
}

#' @rdname measure_attributes_ties
#' @examples 
#' tie_is_twomode(fict_actually)
#' @export
tie_is_twomode <- function(.data){
  if(is_twomode(.data)){
    el <- igraph::as_edgelist(.data, names = FALSE)
    el[,1] <- node_is_mode(.data)[el[,1]]
    el[,2] <- node_is_mode(.data)[el[,2]]
    out <- el[,1] != el[,2]
  } else out <- rep(FALSE, net_ties(.data))
  make_tie_mark(out, .data)
}

#' @rdname measure_attributes_ties
#' @section Parallel ties:
#'   Parallel ties, also called multi-edges, are two or more ties that join
#'   the same pair of nodes at the same moment.
#'   Ties that join the same pair of nodes at different moments follow one
#'   another rather than run alongside one another,
#'   and so are not parallel.
#'   How a network records time therefore decides which ties coexist:
#'   
#'   - Where a network is a panel, ties are parallel where they share a wave.
#'   - Where a network records a stream of events, ties are parallel where
#'   they share a moment.
#'   - Where a network records the interval each tie lasts over,
#'   ties are parallel where those intervals overlap.
#'   Intervals that merely abut, one beginning as the other ends, do not.
#'   - Where a network records no time at all, any two ties on a pair of
#'   nodes are parallel.
#'   
#'   Ties of different types are likewise not parallel.
#'   Several types of tie between a pair of nodes is what `is_multiplex()`
#'   marks; `tie_is_parallel()` marks several ties of one type.
#'   
#'   Every tie in such a bundle is marked, and not just the repetitions,
#'   so `sum(tie_is_parallel(ison_koenigsberg))` counts four of the seven
#'   bridges and not two.
#'   
#'   Note that `as_matrix()` reports how many ties join each pair of nodes,
#'   so a network with parallel ties gives a matrix with cells greater than
#'   one even where it is neither weighted nor signed.
#' @examples
#' tie_is_parallel(ison_koenigsberg)
#' @export
tie_is_parallel <- function(.data){
  make_tie_mark(.parallel_ties(.data), .data)
}

#' @rdname measure_attributes_ties
#' @inheritParams to_backbone
#' @seealso [to_backbone()], which deletes the ties this does not mark,
#'   and which documents each filter and the works they come from.
#' @examples
#' tie_is_backbone(ison_networkers)
#' @export
tie_is_backbone <- function(.data, filter = NULL, threshold = NULL,
                            endpoints = c("either", "both")){
  spec <- .backbone_spec(.data, filter, threshold)
  make_tie_mark(.backbone_keep(.data, spec, endpoints), .data)
}

# Which ties run parallel to another tie. A pair of nodes joined twice at two
# different moments is joined once at either of them, so the moment each tie
# belongs to joins the pair of nodes it runs between in the key that decides
# which ties coexist. `.time_rule()` reports how a network records its
# moments, and intervals are handled apart since they overlap rather than
# match.
.parallel_ties <- function(.data){
  nt <- net_ties(.data)
  if(nt < 2) return(rep(FALSE, nt))
  dyad <- .dyad_keys(.data)
  if(identical(.time_rule(.data), "interval")){
    atts <- net_tie_attributes(.data)
    layer <- intersect(c("layer", "type"), atts)[1]
    if(!is.na(layer))
      dyad <- paste(dyad, as.character(tie_attribute(.data, layer)))
    return(.parallel_spells(.data, dyad))
  }
  key <- paste(dyad, .parallel_strata(.data))
  key %in% key[duplicated(key)]
}

# What separates one tie on a pair of nodes from another, as a string: the
# moment it belongs to, and the layer it belongs to. Ties of different types
# are what `is_multiplex()` marks, so counting them as parallel too would
# leave 'several ties of one type' with no mark of its own, which is the gap
# this fills.
.parallel_strata <- function(.data){
  atts <- net_tie_attributes(.data)
  strata <- character(0)
  stamp <- intersect(c("time", "wave", "panel"), atts)[1]
  if(!is.na(stamp))
    strata <- c(strata, list(as.character(.bare_time(tie_attribute(.data, stamp)))))
  layer <- intersect(c("layer", "type"), atts)[1]
  if(!is.na(layer))
    strata <- c(strata, list(as.character(tie_attribute(.data, layer))))
  if(!length(strata)) return(rep("", net_ties(.data)))
  do.call(paste, strata)
}

# The pair of nodes each tie runs between, as a string. An undirected tie runs
# between its nodes rather than from one to the other, so its ends are put in
# a fixed order and A-B and B-A name the same pair.
# A stocnet keeps its ties in its own table, and coercion may reciprocate its
# undirected layers, so its ends are read from that table rather than from a
# coerced copy. Otherwise the keys would not line up with the tie attributes
# that stratify them, which `tie_attribute()` reads from the same table.
.dyad_keys <- function(.data){
  if(inherits(.data, "stocnet")){
    from <- as.character(.data$ties$from)
    to <- as.character(.data$ties$to)
  } else {
    el <- igraph::as_edgelist(as_igraph(.data), names = FALSE)
    from <- as.character(el[,1])
    to <- as.character(el[,2])
  }
  if(is_directed(.data)) paste(from, to) else
    paste(pmin(from, to), pmax(from, to))
}

# Which ties overlap another tie on the same pair of nodes. Two intervals
# overlap where each begins before the other ends, so spells that abut, one
# beginning in the moment the other ends, are consecutive and not parallel.
.parallel_spells <- function(.data, dyad){
  atts <- net_tie_attributes(.data)
  begin <- .bare_time(tie_attribute(.data,
                                    intersect(c("begin", "beg", "start"), atts)[1]))
  end <- if("end" %in% atts) .bare_time(tie_attribute(.data, "end")) else
    rep(NA, length(dyad))
  # A tie that begins and never ends is right-censored, and so lasts at least
  # as long as the last moment the network records.
  if(anyNA(end)) end[is.na(end)] <- max(c(end, begin), na.rm = TRUE)
  out <- rep(FALSE, length(dyad))
  for(d in unique(dyad[duplicated(dyad)])){
    i <- which(dyad == d)
    for(a in utils::head(seq_along(i), -1)) for(b in (a+1):length(i)){
      if(begin[i[a]] < end[i[b]] && begin[i[b]] < end[i[a]]){
        out[i[a]] <- TRUE
        out[i[b]] <- TRUE
      }
    }
  }
  out
}

