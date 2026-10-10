# Joining parts ####

#' Joining parts of a network
#' @name modif_from_parts
#' @description
#'   These functions join a list of networks that are each a part of one
#'   network, holding some of its nodes and the ties among them,
#'   back into that one network.
#'   Each reverses one of the `to_*s()` functions that splits a network into
#'   parts.
#'
#'   - `from_subgraphs()` joins a list of subgraphs, as `to_subgraphs()`
#'   returns, into one network.
#'   - `from_egos()` joins a list of ego networks, as `to_egos()` returns,
#'   into one whole network.
#'
#'   A node or a tie that more than one part holds is held once in the result.
#'
#'   Every function here returns the class of the networks in the list:
#'   a 'stocnet' from 'stocnet's, a 'tbl_graph' from 'tbl_graph's,
#'   an 'igraph' from 'igraph's, a 'network' from 'network's,
#'   and a single matrix from matrices, since the parts are all of one network.
#'
#'   To stack networks that each hold the same nodes, one for each moment,
#'   layer, or reporter, see [from_times()] and the other functions there.
#' @param netlist A list of network, igraph, tidygraph, matrix, or edgelist
#'   objects.
#' @template fam_modif
NULL

#' @rdname modif_from_parts
#' @importFrom igraph graph_from_data_frame as_data_frame set_vertex_attr
#' @examples
#' ison_adolescents |>
#'   mutate(unicorn = sample(c("yes", "no"), 8, replace = TRUE)) |>
#'   to_subgraphs(attribute = "unicorn") |>
#'   from_subgraphs()
#' @export
from_subgraphs <- function(netlist) {
  if (!is.list(netlist[1])) {
    snet_abort("Please declare a list of subgraphs. ")
  }
  ann <- lapply(netlist, as_igraph)
  out <- ann[[1]]
  for (i in seq_along(ann)[-1]) {
    out <- join_nodes(out, ann[[i]])
  }
  for (i in seq_along(ann)[-1]) {
    out <- join_ties(out, ann[[i]])
  }
  orig <- object2 <- NULL
  out <- select_ties(out, -c(orig, object2))
  .as_class_of(as_tidygraph(out), netlist[[1]])
}

#' @rdname modif_from_parts
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents |>
#'   to_egos() |>
#'   from_egos()
#' @export
from_egos <- function(netlist) {
  if (!is.list(netlist[1])) {
    snet_abort("Please declare a list of egos.")
  }
  ann <- lapply(netlist, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  out <- as_tidygraph(igraph::graph_from_data_frame(dplyr::distinct(out)))
  .as_class_of(out, netlist[[1]])
}

# Joining along a third dimension ####

#' Joining networks along a third dimension
#' @name modif_from_ternary
#' @description
#'   These functions join a list of networks that each hold the same nodes,
#'   one network for each moment, layer, or reporter, into one network.
#'   Each tie then records the network it came from in one of its columns:
#'   `time` for a moment, `layer` for a layer, and `by` for a reporter.
#'   Each reverses one of the `to_*s()` functions that splits a network along
#'   such a dimension.
#'
#'   - `from_times()` joins a list of the network at each moment, as
#'   `to_times()` returns, back into one network that records time.
#'   This is where new work on rejoining a network over time belongs;
#'   `from_waves()` and `from_slices()` are the older, form-specific spellings.
#'   - `from_waves()` joins a list of network waves into a longitudinal network.
#'   - `from_slices()` joins a list of time slices of a network into
#'   a dynamic network.
#'   - `from_layers()` joins several networks over the same nodes into one
#'   multiplex network, keeping each as its own layer.
#'   `from_ties()` is an alias.
#'   Where `to_layers()` splits a multiplex network into its layers,
#'   `from_layers()` reassembles them.
#'   To combine the networks' tie values into a single value per dyad instead,
#'   use `to_aggregated()` on the result, or `join_ties()` for two networks.
#'   - `from_reporters()` joins a list of the networks that each reporter
#'   reported, named after the reporters, into one cognitive social
#'   structure (see [is_cognitive()]).
#'
#'   Every function here returns the class of the networks in the list:
#'   a 'stocnet' from 'stocnet's, a 'tbl_graph' from 'tbl_graph's,
#'   an 'igraph' from 'igraph's, a 'network' from 'network's.
#'   A list of matrices gives a three-dimensional array, with one slice for
#'   each matrix, named after the list, since a matrix has no column in which
#'   to record the moment, layer, or reporter of a tie.
#'   `as_stocnet()` reads such an array back, once told what its slices hold.
#'
#'   A tie that a network records as missing stays missing from that network
#'   alone, so a node that did not report at one moment, or in one layer, is
#'   recorded as not reporting there and nowhere else.
#'
#'   To join networks that are each a part of one network, holding some of its
#'   nodes, see [from_subgraphs()] and [from_egos()].
#' @param netlist A list of network, igraph, tidygraph, matrix, or edgelist
#'   objects, over the same nodes.
#' @template fam_modif
NULL

#' @rdname modif_from_ternary
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @examples
#' ison_adolescents |>
#'   mutate_ties(wave = sample(1:4, 10, replace = TRUE)) |>
#'   to_waves(attribute = "wave") |>
#'   from_waves()
#' @export
from_waves <- function(netlist) {
  if (!is.list(netlist[1])) {
    snet_abort("Please declare a list of waves.")
  }
  if (.all_matrices(netlist)) return(.stack_matrices(netlist))
  ann <- lapply(netlist, as_igraph)
  .as_class_of(.rebind_netlist(ann), netlist[[1]])
}

# Reassemble a list of igraphs into one network, keeping isolates and node
# attributes by binding the vertex tables as well as the edge tables.
# For nodes whose attributes differ between waves/slices (e.g. 'active'),
# the first appearance wins.
.rebind_netlist <- function(ann, distinct_ties = FALSE) {
  ties <- do.call(rbind, lapply(ann, igraph::as_data_frame, what = "edges"))
  if (isTRUE(distinct_ties)) ties <- dplyr::distinct(ties)
  if (all(vapply(ann, is_labelled, logical(1)))) {
    nodes <- do.call(rbind, lapply(ann, igraph::as_data_frame,
                                   what = "vertices"))
    nodes <- nodes[!duplicated(nodes$name), , drop = FALSE]
    as_tidygraph(igraph::graph_from_data_frame(ties, vertices = nodes))
  } else {
    # unlabelled networks offer no node identity across waves beyond ties
    as_tidygraph(igraph::graph_from_data_frame(ties))
  }
}

#' @rdname modif_from_ternary 
#' @param remove.duplicates Should duplicates be removed?
#' By default FALSE.
#' If TRUE, duplicated edges are removed.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents |>
#'   mutate_ties(time = 1:10, increment = 1) |> 
#'   add_ties(c(1,2), list(time = 3, increment = -1)) |> 
#'   to_slices(slice = c(5,7)) |>
#'   from_slices()
#' @export
from_slices <- function(netlist, remove.duplicates = FALSE) {
  if (is.list(netlist[1])) {
    if (.all_matrices(netlist)) return(.stack_matrices(netlist))
    ann <- lapply(netlist, as_igraph)
    .as_class_of(.rebind_netlist(ann, distinct_ties = remove.duplicates),
                 netlist[[1]])
  } else {
    message("Only one slice is available, cannot be joined.")
  }
}

#' @rdname modif_from_ternary
#' @details
#'   `from_times()` rejoins what [to_times()] returns, stamping each network's
#'   ties with the moment it names before binding them, and so inverts it for
#'   a network that stamps its moments. Where the networks carry the interval
#'   each tie lasts over, the moment is already in those ties, so they are
#'   bound and deduplicated rather than stamped again.
#' @examples
#' from_times(to_times(ison_tailorshop))
#' @export
from_times <- function(netlist) {
  if(!is.list(netlist) || !length(netlist))
    snet_abort("Please declare a list of networks, as {.fn to_times} returns.")
  if(.all_matrices(netlist)) return(.stack_matrices(netlist))
  intervals <- any(c("begin", "beg", "start") %in%
                     net_tie_attributes(netlist[[1]]))
  if(intervals) return(from_slices(netlist, remove.duplicates = TRUE))
  moments <- names(netlist)
  if(is.null(moments)) moments <- as.character(seq_along(netlist))
  # The names are the moments as characters, and a moment counted or dated is
  # not a character, so they are read back as what they were written from.
  moments <- utils::type.convert(moments, as.is = TRUE)
  if(all(vapply(netlist, function(x) inherits(x, "stocnet"), logical(1))))
    return(.join_stocnets(netlist, "time", moments))
  stamped <- Map(function(net, at) mutate_ties(net, time = at),
                 netlist, moments)
  .as_class_of(.rebind_netlist(lapply(stamped, as_igraph)), netlist[[1]])
}

#' @rdname modif_from_ternary
#' @param ... Two or more networks over the same nodes to be merged,
#'   or a single list of such networks, such as that returned by `to_layers()`.
#'   Nodes are matched by name where the networks are labelled,
#'   and by position where they are not and are the same size.
#' @param layer_names A character vector of names for the different network objects,
#'   if not already named within the list.
#' @examples
#' marriage <- to_uniplex(ison_florentine, "marriage")
#' business <- to_uniplex(ison_florentine, "business")
#' from_layers(marriage = marriage, business = business)
#' @export
from_layers <- function(..., layer_names){
  netlist <- .splice_netlist(...)
  if(length(netlist) == 0)
    snet_abort("Please give two or more networks to merge.")
  if(!missing(layer_names) && !is.null(layer_names))
    names(netlist) <- layer_names
  if(.all_matrices(netlist)) return(.stack_matrices(netlist))
  # a network split into layers may hold just one, and reassembling one
  # network is that network
  if(length(netlist) == 1) return(netlist[[1]])
  layer_names <- if(missing(layer_names)) NULL else layer_names
  if(inherits(netlist[[1]], "stocnet"))
    .stack_layers_stocnet(netlist, layer_names)
  else .as_class_of(.stack_layers_tbl(netlist, layer_names), netlist[[1]])
}

#' @rdname modif_from_ternary
#' @export
from_ties <- from_layers

#' @rdname modif_from_ternary
#' @details
#'   `from_reporters()` takes the reporter of each network from the names of
#'   the list, which may be the labels of the nodes or their positions.
#'   Where the list is not named, the networks are taken to be the reports of
#'   the first nodes, in order.
#' @examples
#' reports <- list(A = create_ring(4), B = create_star(4))
#' from_reporters(lapply(reports, add_node_attribute, "name", LETTERS[1:4]))
#' @export
from_reporters <- function(netlist) {
  if(!is.list(netlist) || !length(netlist) || is_manynet(netlist))
    snet_abort("Please declare a list of networks, one for each reporter.")
  if(.all_matrices(netlist)) return(.stack_matrices(netlist))
  .as_class_of(.join_third(netlist, "by"), netlist[[1]])
}

# Helpers for joining ####

# A stocnet whose nonresponse records are replaced by the missing ties they
# imply, listed in its missings, so that they can be joined with the missing
# ties of other networks and recompressed for the joined network.
.explicit_missing <- function(net){
  missing <- as_missinglist(net)
  net <- .clear_missing(net)
  net$missings <- if(!is.null(missing) && nrow(missing))
    .tidy_registry(missing) else NULL
  net
}

# The class a list of networks came in, which a join gives back.
.as_class_of <- function(out, template){
  cls <- setdiff(class(template), c("mnet", "tbl_df", "tbl"))[1]
  switch(cls,
         stocnet = as_stocnet(out),
         tbl_graph = as_tidygraph(out),
         igraph = as_igraph(out),
         network = as_network(out),
         matrix = as_matrix(out),
         # an array's third dimension is what was split or combined, so what
         # is left of it is a matrix
         array = as_matrix(out),
         data.frame = as_edgelist(out),
         out)
}

.all_matrices <- function(netlist){
  length(netlist) > 0 &&
    all(vapply(netlist, function(x) is.matrix(x) && !is.data.frame(x),
               logical(1)))
}

# A list of matrices stacked into an array, one slice for each, named after
# the list. Matrices of different nodes are first aligned on their names.
.stack_matrices <- function(netlist){
  mats <- lapply(netlist, as.matrix)
  first <- mats[[1]]
  same <- all(vapply(mats, function(m) identical(dim(m), dim(first)) &&
                       identical(dimnames(m), dimnames(first)), logical(1)))
  if(!same){
    named <- all(vapply(mats, function(m)
      !is.null(rownames(m)) && !is.null(colnames(m)), logical(1)))
    if(!named)
      snet_abort("Matrices without names can only be stacked where they are",
                 "all the same size.")
    rows <- unique(unlist(lapply(mats, rownames)))
    cols <- unique(unlist(lapply(mats, colnames)))
    square <- all(vapply(mats, function(m)
      identical(rownames(m), colnames(m)), logical(1)))
    if(square) rows <- cols <- unique(c(rows, cols))
    mats <- lapply(mats, function(m){
      out <- matrix(0, length(rows), length(cols),
                    dimnames = list(rows, cols))
      out[rownames(m), colnames(m)] <- m
      out
    })
    first <- mats[[1]]
  }
  slices <- names(netlist)
  dimnames <- if(is.null(dimnames(first)) && is.null(slices)) NULL else
    list(rownames(first), colnames(first), slices)
  array(unlist(mats), dim = c(nrow(first), ncol(first), length(mats)),
        dimnames = dimnames)
}

# Joins a list of stocnets over the same nodes into one, recording which of
# them each tie came from in the column `col`. The ties each network records
# as missing come along too, marked as such, so that `make_stocnet()` records
# a node that did not report in one of them for that one alone.
.join_stocnets <- function(netlist, col, values){
  first <- .clear_missing(netlist[[1]])
  # The networks record one relation, which is directed wherever any of them
  # is, so the ties of an undirected one are then held in both directions.
  each <- vapply(netlist, is_directed, logical(1))
  directed <- any(each)
  bothways <- function(tab){
    if(!nrow(tab)) return(tab)
    rev <- tab[tab$from != tab$to, , drop = FALSE]
    rev[c("from", "to")] <- rev[c("to", "from")]
    dplyr::bind_rows(tab, rev)
  }
  rows <- Map(function(net, value){
    ties <- net$ties
    if(is.null(ties)) ties <- dplyr::tibble(from = integer(0), to = integer(0))
    missing <- as_missinglist(net)
    if(!is.null(missing) && nrow(missing)){
      missing <- .tidy_registry(missing)
      missing$na <- TRUE
      ties <- dplyr::bind_rows(ties, missing)
    }
    if(directed && !is_directed(net) && !is_twomode(net)) ties <- bothways(ties)
    ties[[col]] <- rep(value, nrow(ties))
    ties
  }, netlist, values)
  ties <- dplyr::bind_rows(rows)
  # A join of networks without ties still records what joins them.
  if(!nrow(ties)){
    ties <- dplyr::tibble(from = integer(0), to = integer(0))
    ties[[col]] <- values[0]
  }
  info <- first$info
  info$transformations <- NULL
  # Where the networks agree, the first one's record of direction, which may
  # be one for each layer, stands for them all.
  if(!all(each == directed)) info$directed <- directed
  make_stocnet(info = info, nodes = first$nodes, ties = ties,
               changes = first$changes, globals = first$globals)
}

# Joins a list of networks, each the report of one reporter ('by') or each
# about one target ('about'), into one stocnet naming that node in `col`.
.join_third <- function(netlist, col){
  nets <- lapply(netlist, as_stocnet)
  labels <- nets[[1]]$nodes[["label"]]
  values <- .third_node_index(names(netlist), labels, length(nets))
  out <- .join_stocnets(nets, col, values)
  if(col == "by" && is.null(out$info$observation))
    out$info$observation <- "cognitive"
  out
}

# The node each network of a list names, from the names of the list: labels
# where the nodes have them and the names match, and positions otherwise.
.third_node_index <- function(names, labels, n){
  if(is.null(names)) return(seq_len(n))
  if(!is.null(labels)){
    idx <- match(names, labels)
    if(!anyNA(idx)) return(idx)
  }
  idx <- suppressWarnings(as.integer(names))
  if(anyNA(idx))
    snet_abort("The names {names[is.na(idx)]} do not name nodes of the",
               "networks.")
  idx
}

# Networks may be given one by one or as a single list, as `to_layers()` and
# the other splitting functions return one. A network is not itself spliced,
# even though some classes are lists.
.splice_netlist <- function(...){
  netlist <- list(...)
  if(length(netlist) == 1 && is.list(netlist[[1]]) &&
     !is_manynet(netlist[[1]])) netlist <- netlist[[1]]
  netlist
}

.stack_layers_tbl <- function(netlist, layer_names){
  netlist <- lapply(netlist, as_tidygraph)
  labelled <- vapply(netlist, is_labelled, logical(1))
  # layers are stacked by joining the networks' node tables, which needs a
  # name to join on. Unlabelled networks are named by position first, and
  # unlabelled again afterwards where none of them was labelled to begin with.
  if(!all(labelled)) netlist <- .name_by_position(netlist, labelled)
  if(is.null(names(netlist))){
    if(!is.null(layer_names)){
      names(netlist) <- layer_names
    } else snet_abort("Please name the layers of the networks to be merged,",
                            "either by naming the list elements or",
                            "by providing a vector of names to 'layer_names'.")
  }
  netlist <- lapply(seq_along(netlist), 
                    function(x) if(is_multiplex(netlist[[x]])){
                      netlist[[x]] } else { 
                        mutate_ties(netlist[[x]], type = names(netlist)[x])
                        })
  out <- suppressMessages(Reduce(tidygraph::graph_join, netlist))
  # `graph_join()` returns a directed graph whatever it is given, which would
  # make an undirected tie recorded as A-B differ from the same tie recorded
  # as B-A. "each" re-marks the graph without collapsing any of its ties.
  if(!any(vapply(netlist, is_directed, logical(1))))
    out <- as_tidygraph(igraph::as_undirected(as_igraph(out), mode = "each"))
  # record the layers as tie-type metadata; otherwise metadata inherited
  # from the (single-layer) inputs would shadow them in layer_names()
  if("type" %in% igraph::edge_attr_names(out))
    out <- igraph::set_graph_attr(out, "ties",
                                  unique(igraph::edge_attr(out, "type")))
  if(!any(labelled)) out <- to_unlabelled(out)
  as_tidygraph(out)
}

# Gives the unlabelled networks a name for each node so that the node tables
# can be joined. Where one of the networks is labelled, its names are used,
# since the nodes are the same nodes in the same order; where none is, the
# positions themselves are the names, and are removed again afterwards.
.name_by_position <- function(netlist, labelled){
  sizes <- vapply(netlist, function(x) igraph::vcount(as_igraph(x)), numeric(1))
  if(length(unique(sizes)) > 1)
    snet_abort(paste0("Layering matches the networks' nodes by name, or by ",
                      "position where they are unlabelled, so please label ",
                      "them first with {.fn to_labelled} or give networks of ",
                      "the same size."))
  labels <- if(any(labelled)) node_names(netlist[[which(labelled)[1]]]) else
    as.character(seq_len(sizes[1]))
  netlist[!labelled] <- lapply(netlist[!labelled], function(x)
    as_tidygraph(igraph::set_vertex_attr(as_igraph(x), "name", value = labels)))
  netlist
}

.stack_layers_stocnet <- function(netlist, layer_names){

  ## 1. Name the list elements (used as fallback layer names) -------------
  if(is.null(names(netlist)) || any(names(netlist) == "")){
    if(!is.null(layer_names)){
      names(netlist) <- layer_names
    } else {
      snet_abort("Please name the layers of the networks to be merged,",
                 "either by naming the list elements or",
                 "by providing a vector of names to 'layer_names'.")
    }
  }
  layer_names <- names(netlist)
  
  ## 2. Resolve layer names per network, only renaming on clashes ----------
  seen_layers <- character(0)
  for(i in seq_along(netlist)){
    # A node that did not report in one network did not report in that layer
    # alone, so its missing ties are listed before the node tables are merged.
    net <- .explicit_missing(netlist[[i]])
    layers_i <- .get_layers(net, layer_names[i])
    clash <- intersect(layers_i, seen_layers)
    if(length(clash)){
      rn <- stats::setNames(paste0(layer_names[i], ".", clash), clash)
      net <- .rename_layers(net, rn)
      layers_i[layers_i %in% clash] <- unname(rn[layers_i[layers_i %in% clash]])
      snet_warn(paste0("Layer name(s) ", phrase(clash),
                       " already used by another network; renamed to ",
                       phrase(unname(rn)), " for '", layer_names[i], "'."))
    }
    netlist[[i]] <- .ensure_layers(net, layers_i, layer_names[i])
    seen_layers <- c(seen_layers, layers_i)
  }
  
  ## 3. Merge labelled node tables ------------------------------------------
  labelled <- vapply(netlist, function(x) is_labelled(x), logical(1))
  
  merged_nodes <- .merge_node_tables(lapply(netlist[labelled], function(x) x$nodes))
  
  ## 4. Reindex labelled networks onto merged_nodes -------------------------
  netlist[labelled] <- lapply(netlist[labelled], function(net){
    new_idx <- match(net$nodes$label, merged_nodes$label)
    .apply_reindex(net, new_idx)
  })
  
  ## 5. Match/merge unlabelled networks, by block size (and mode) ----------
  for(i in which(!labelled)){
    net <- netlist[[i]]
    blocks <- .get_blocks(net)
    res <- .match_anon_blocks(blocks, merged_nodes)
    merged_nodes <- .carry_node_attributes(res$merged_nodes, net$nodes,
                                           res$new_idx)
    netlist[[i]] <- .apply_reindex(net, res$new_idx)
  }
  
  ## 6. Combine ties, changes, global ---------------------------------------
  bind_or_null <- function(lst){
    lst <- lst[!vapply(lst, is.null, logical(1))]
    if(length(lst) == 0) return(NULL)
    out <- dplyr::bind_rows(lst)
    if(nrow(out) == 0) NULL else out
  }
  merged_ties    <- bind_or_null(lapply(netlist, function(x) x$ties))
  merged_changes <- bind_or_null(lapply(netlist, function(x) x$changes))
  merged_globals <- bind_or_null(lapply(netlist, function(x) x$globals))
  merged_missings <- bind_or_null(lapply(netlist, function(x) x$missings))
  
  ## 7. Combine info ----------------------------------------------------------
  merged_info <- .merge_info(netlist, merged_nodes)
  if (!is.null(merged_nodes) &&
      "label" %in% names(merged_nodes) &&
      all(is.na(merged_nodes$label))) {
    merged_nodes$label <- NULL
  }
  
  ## 8. Assemble ----------------------------------------------------------------
  out <- make_stocnet(info = merged_info, nodes = merged_nodes,
               ties = merged_ties, changes = merged_changes,
               globals = merged_globals, missings = merged_missings)
  out
}

## --- Helpers ----------------------------------------------------------------

# Work out a network's layer name(s), preferring info$layers and any
# 'layer' column on ties; falling back to the list-element name.
.get_layers <- function(net, fallback_name){
  layers <- net$info$layers
  if(!is.null(net$ties) && "layer" %in% names(net$ties)){
    tl <- unique(net$ties$layer)
    layers <- union(layers, tl[!is.na(tl)])
  }
  if(is.null(layers) || length(layers) == 0) layers <- fallback_name
  layers
}

# Make sure info$layers and ties$layer are consistent with `layers`,
# and that 'directed'/'observation'/'update' are named by layer.
.ensure_layers <- function(net, layers, fallback_name){
  info <- net$info
  if(is.null(info)) info <- list()
  info$layers <- layers
  
  if(!is.null(net$ties)){
    if(!"layer" %in% names(net$ties)){
      net$ties$layer <- if(length(layers) == 1) layers else NA_character_
      if(length(layers) > 1)
        snet_warn(paste0("Network '", fallback_name, "' declares multiple ",
                         "layers but its ties have no 'layer' column."))
    } else if(any(is.na(net$ties$layer)) && length(layers) == 1){
      net$ties$layer[is.na(net$ties$layer)] <- layers
    }
  }
  # A tie missing from a network is missing from its layer, once that network
  # is one layer of several.
  if(!is.null(net$missings) && nrow(net$missings) && length(layers) == 1){
    if(!"layer" %in% names(net$missings)) net$missings$layer <- layers else
      net$missings$layer[is.na(net$missings$layer)] <- layers
  }
  
  if(length(layers) == 1){
    for(field in c("directed", "observation", "update")){
      v <- info[[field]]
      if(!is.null(v) && is.null(names(v))) info[[field]] <- stats::setNames(v, layers)
    }
  }
  
  net$info <- info
  net
}

# Rename layers (in ties$layer, info$layers, named info fields, info$focal,
# and nested per-layer info blocks) per a named map old -> new.
.rename_layers <- function(net, rename_map){
  if(!is.null(net$ties) && "layer" %in% names(net$ties)){
    idx <- net$ties$layer %in% names(rename_map)
    net$ties$layer[idx] <- unname(rename_map[net$ties$layer[idx]])
  }
  info <- net$info
  if(!is.null(info)){
    if(!is.null(info$layers)){
      idx <- info$layers %in% names(rename_map)
      info$layers[idx] <- unname(rename_map[info$layers[idx]])
    }
    for(field in c("directed", "observation", "update")){
      v <- info[[field]]
      if(!is.null(v) && !is.null(names(v))){
        nm <- names(v)
        idx <- nm %in% names(rename_map)
        nm[idx] <- unname(rename_map[nm[idx]])
        names(info[[field]]) <- nm
      }
    }
    if(!is.null(info$focal) && info$focal %in% names(rename_map))
      info$focal <- unname(rename_map[[info$focal]])
    for(old in names(rename_map)){
      new <- rename_map[[old]]
      if(old %in% names(info) && new != old){
        info[[new]] <- info[[old]]
        info[[old]] <- NULL
      }
    }
  }
  net$info <- info
  net
}

# Full-join node tables on 'label', coalescing overlapping attribute columns
# (including 'mode') and warning on genuine conflicts.
.merge_node_tables <- function(node_tbls){
  node_tbls <- node_tbls[!vapply(node_tbls, is.null, logical(1))]
  if(length(node_tbls) == 0) return(NULL)
  
  merged <- node_tbls[[1]]
  for(tb in node_tbls[-1]){
    common <- intersect(setdiff(names(merged), "label"),
                        setdiff(names(tb), "label"))
    merged <- dplyr::full_join(merged, tb, by = "label", suffix = c("", ".y"))
    for(col in common){
      ycol <- paste0(col, ".y")
      if(ycol %in% names(merged)){
        conflict <- !is.na(merged[[col]]) & !is.na(merged[[ycol]]) &
          merged[[col]] != merged[[ycol]]
        if(any(conflict))
          snet_warn(paste0("Conflicting values for node attribute '", col,
                           "' across networks; keeping the first network's ",
                           "values where they differ."))
        merged[[col]] <- dplyr::coalesce(merged[[col]], merged[[ycol]])
        merged[[ycol]] <- NULL
      }
    }
  }
  merged
}

# Apply a from-old-to-new node index map to ties$from/to and changes$node
.apply_reindex <- function(net, new_idx){
  if(!is.null(net$ties)) net$ties <- .remap_tie_nodes(net$ties, new_idx)
  if(!is.null(net$missings))
    net$missings <- .remap_tie_nodes(net$missings, new_idx)
  if(!is.null(net$changes)){
    net$changes$node <- new_idx[net$changes$node]
  }
  net
}

# Describe an unlabelled network's nodeset as one or more "blocks"
# (by mode, if a mode column exists), each with a size and a mode tag.
.get_blocks <- function(net){
  nodes <- net$nodes
  if(is.null(nodes)){
    n <- suppressWarnings(max(c(net$ties$from, net$ties$to,
                                net$changes$node), 0, na.rm = TRUE))
    return(list(list(mode = NA_character_, size = n)))
  }
  if("mode" %in% names(nodes)){
    keys <- ifelse(is.na(nodes$mode), "\u0001NA\u0001", nodes$mode)
    lapply(unique(keys), function(k){
      list(mode = if(k == "\u0001NA\u0001") NA_character_ else k,
           size = sum(keys == k))
    })
  } else {
    list(list(mode = NA_character_, size = nrow(nodes)))
  }
}

# Match each unlabelled block to an existing label==NA pool in merged_nodes,
# primarily by size; 'mode' is used only to disambiguate when there are
# multiple same-sized candidates, and to fill in unknown mode info.
# An unlabelled network is matched to the merged nodes by position, which
# gives each of its nodes a row there but not the attributes it holds. Those
# are carried to the rows they were matched to, where a row does not already
# hold a value for them.
.carry_node_attributes <- function(merged_nodes, nodes, idx){
  if(is.null(merged_nodes) || is.null(nodes)) return(merged_nodes)
  for(col in setdiff(names(nodes), c("label", "mode"))){
    value <- nodes[[col]]
    if(!col %in% names(merged_nodes))
      merged_nodes[[col]] <- value[rep(NA_integer_, nrow(merged_nodes))]
    held <- merged_nodes[[col]][idx]
    merged_nodes[[col]][idx] <- ifelse(is.na(held), value, held)
  }
  merged_nodes
}

.match_anon_blocks <- function(blocks, merged_nodes){
  sizes <- vapply(blocks, function(b) b$size, numeric(1))
  offsets <- cumsum(c(0, sizes))
  new_idx <- integer(sum(sizes))
  
  for(i in seq_along(blocks)){
    b <- blocks[[i]]
    rng <- (offsets[i] + 1):offsets[i + 1]
    if(b$size == 0) next
    
    candidates <- list()
    if(!is.null(merged_nodes)){
      na_label <- is.na(merged_nodes$label)
      mode_vals <- if("mode" %in% names(merged_nodes)) merged_nodes$mode
      else rep(NA_character_, nrow(merged_nodes))
      pool_idx <- which(na_label)
      if(length(pool_idx)){
        keys <- ifelse(is.na(mode_vals[pool_idx]), "\u0001NA\u0001", mode_vals[pool_idx])
        for(k in unique(keys)){
          idx <- pool_idx[keys == k]
          pm <- if(k == "\u0001NA\u0001") NA_character_ else k
          # match on size alone; mode is not a hard requirement
          if(length(idx) == b$size)
            candidates[[length(candidates) + 1]] <- list(idx = idx, mode = pm)
        }
      }
    }
    
    if(length(candidates) > 1 && !is.na(b$mode)){
      # if several same-sized pools exist, prefer one whose mode matches
      # (or is unknown) over one with a known, differing mode
      compatible <- vapply(candidates, function(cand)
        is.na(cand$mode) || cand$mode == b$mode, logical(1))
      if(any(compatible)) candidates <- candidates[compatible]
    }
    
    if(length(candidates) == 1){
      claim <- candidates[[1]]
      new_idx[rng] <- claim$idx
      if(is.na(claim$mode) && !is.na(b$mode)){
        if(!("mode" %in% names(merged_nodes))) merged_nodes$mode <- NA_character_
        merged_nodes$mode[claim$idx] <- b$mode
      }
    } else {
      if(length(candidates) > 1)
        snet_warn(paste0("Multiple possible matches found for an unlabelled ",
                         "block of ", b$size, " nodes; adding as new nodes ",
                         "rather than guessing."))
      add <- dplyr::tibble(label = rep(NA_character_, b$size))
      if(!is.na(b$mode)) add$mode <- b$mode
      merged_nodes <- if(is.null(merged_nodes)) add else 
        dplyr::bind_rows(merged_nodes, add)
      new_idx[rng] <- (nrow(merged_nodes) - b$size + 1):nrow(merged_nodes)
    }
  }
  list(new_idx = new_idx, merged_nodes = merged_nodes)
}

# Build the merged info list
.merge_info <- function(netlist, merged_nodes){
  all_layers <- unique(unlist(lapply(netlist, function(x) x$info$layers),
                              use.names = FALSE))
  
  all_modes <- character(0)
  if(!is.null(merged_nodes) && "mode" %in% names(merged_nodes))
    all_modes <- unique(stats::na.omit(merged_nodes$mode))
  for(net in netlist) all_modes <- union(all_modes, net$info$modes)
  
  directed_vec    <- stats::setNames(rep(NA, length(all_layers)), all_layers)
  observation_vec <- stats::setNames(rep(NA_character_, length(all_layers)), all_layers)
  update_vec      <- stats::setNames(rep(NA_character_, length(all_layers)), all_layers)
  
  for(net in netlist){
    for(ly in net$info$layers){
      d <- net$info$directed
      if(!is.null(d)){
        val <- if(!is.null(names(d)) && ly %in% names(d)) d[[ly]]
        else if(length(d) == 1) d[[1]] else NA
        if(!is.na(val)) directed_vec[ly] <- val
      }
      obs <- if(!is.null(net$info[[ly]]$observation)) net$info[[ly]]$observation
      else net$info$observation
      if(!is.null(obs)){
        if(!is.null(names(obs)) && ly %in% names(obs)) observation_vec[ly] <- obs[[ly]]
        else if(length(obs) == 1) observation_vec[ly] <- obs
      }
      upd <- if(!is.null(net$info[[ly]]$update)) net$info[[ly]]$update
      else net$info$update
      if(!is.null(upd)){
        if(!is.null(names(upd)) && ly %in% names(upd)) update_vec[ly] <- upd[[ly]]
        else if(length(upd) == 1) update_vec[ly] <- upd
      }
    }
  }
  if(all(is.na(directed_vec)))    directed_vec    <- NULL
  # A layer that declared no design is left undeclared, rather than given one
  # it did not state, while the layers that declared theirs keep them.
  observation_vec <- observation_vec[!is.na(observation_vec)]
  if(!length(observation_vec)) observation_vec <- NULL
  update_vec <- update_vec[!is.na(update_vec)]
  if(!length(update_vec)) update_vec <- NULL
  
  focals <- unlist(lapply(netlist, function(x) x$info$focal))
  focal <- if(length(focals) >= 1) focals[1] else NULL
  if(length(focals) > 1)
    snet_warn(paste0("Multiple networks declared a 'focal' layer; keeping ",
                     "the first ('", focal, "')."))
  
  merged_info <- list(
    name     = paste(all_layers, collapse = " + "),
    modes    = if(length(all_modes)) all_modes else NULL,
    layers   = all_layers,
    directed = directed_vec,
    observation = observation_vec,
    update   = update_vec,
    focal    = focal
  )
  
  # date / doi: keep if consistent; if conflicting, warn and take the earlier
  for(key in c("date", "doi")){
    vals <- lapply(netlist, function(x) x$info[[key]])
    vals <- vals[!vapply(vals, is.null, logical(1))]
    if(length(vals) == 1){
      merged_info[[key]] <- vals[[1]]
    } else if(length(vals) > 1){
      uvals <- unlist(vals)
      if(length(unique(uvals)) > 1)
        snet_warn(paste0("Networks specify different '", key,
                         "' values; using the ",
                         ifelse(key == "date", "earliest", "first"), "."))
      merged_info[[key]] <- if(key == "date") min(uvals) else uvals[[1]]
    }
  }
  
  # location / source: carry forward only if a single source specifies it
  for(key in c("location", "source")){
    vals <- lapply(netlist, function(x) x$info[[key]])
    vals <- vals[!vapply(vals, is.null, logical(1))]
    if(length(vals) == 1) merged_info[[key]] <- vals[[1]]
  }
  
  # per-layer metadata blocks (sender, receiver, method, etc.)
  layer_keys <- c("sender", "receiver", "update", "observation", "source",
                  "method", "date", "location", "doi")
  for(net in netlist){
    for(ly in net$info$layers){
      nested <- net$info[[ly]]
      if(is.null(nested)) nested <- list()
      toplevel <- net$info[intersect(names(net$info), layer_keys)]
      block <- c(nested, toplevel[setdiff(names(toplevel), names(nested))])
      block$observation <- NULL
      block$update <- NULL
      if(length(block) > 0) merged_info[[ly]] <- block
    }
  }
  
  merged_info[!vapply(merged_info, is.null, logical(1))]
}
