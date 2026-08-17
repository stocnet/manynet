#' Splitting networks into lists
#' @name modif_split
#' @description
#'   These functions offer tools for splitting manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects) into lists of networks.
#'   
#'   - `to_egos()` splits a network into ego (or focal) networks.
#'   - `to_subgraphs()` splits a network into subgraphs on some given node
#'   attribute.
#'   - `to_layers()` splits a multiplex network into its layers,
#'   i.e. a list of uniplex networks, one per tie type.
#'   Use `to_uniplex()`, or its alias `to_layer()`, to retain just one of them.
#'   - `to_components()` splits a network into its components,
#'   ordered from the largest to the smallest.
#'   Use `to_component()` to retain just one of them.
#'   - `to_waves()` splits a network with some discrete observations over time
#'   into a list of those observations.
#'   - `to_slices()` splits a network with some continuous time variable at some
#'   time slice(s).
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(components|subgraphs|egos|waves|slices|layers)"))
#'   ```
#' @template param_data
#' @template param_dir
#' @template param_connectivity
#' @template fam_modif
NULL

#' @rdname modif_split
#' @param max_dist The maximum breadth of the neighbourhood.
#'   By default 1.
#' @param min_dist The minimum breadth of the neighbourhood.
#'   By default 0. 
#'   Increasing this to 1 excludes the ego,
#'   and 2 excludes ego's direct alters.
#' @importFrom igraph make_ego_graph
#' @examples
#'   to_egos(ison_adolescents)
#'   # graphs(to_egos(ison_adolescents,2))
#' @export
to_egos <- function(.data, 
                    max_dist = 1, 
                    min_dist = 0,
                    direction = c("out","in")) UseMethod("to_egos")

#' @export
to_egos.default <- function(.data, max_dist = 1, min_dist = 0,
                            direction = c("out","in")){
  as_input(.data, to_egos, max_dist = max_dist, min_dist = min_dist,
           direction = direction)
}

#' @export
to_egos.igraph <- function(.data, 
                           max_dist = 1, 
                           min_dist = 0,
                           direction = c("out","in")){
  if(is_twomode(.data)) max_dist <- max_dist*2
  snet_progress_step("Obtaining neighbourhoods")
  out <- igraph::make_ego_graph(.data,
                                order = max_dist,
                                mindist = min_dist,
                                mode = match.arg(direction))
  if(is_labelled(.data)) 
    names(out) <- node_labels(.data)
  out
}

#' @export
to_egos.tbl_graph <- function(.data, 
                           max_dist = 1, 
                           min_dist = 0,
                           direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_egos.network <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0,
                            direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_network(x))
}

#' @export
to_egos.matrix <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0,
                           direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_egos.data.frame <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0,
                              direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_edgelist(x))
}

#' @rdname modif_split
#' @param attribute A character string indicating the categorical
#'   attribute in a network used to split into subgraphs.
#' @importFrom igraph induced_subgraph
#' @examples
#' ison_adolescents |>
#'   mutate(unicorn = sample(c("yes", "no"), 8,
#'                           replace = TRUE)) |>
#'   to_subgraphs(attribute = "unicorn")
#' @export
to_subgraphs <- function(.data, attribute) UseMethod("to_subgraphs")

#' @export
to_subgraphs.default <- function(.data, attribute){
  as_input(.data, to_subgraphs, attribute = attribute)
}

#' @export
to_subgraphs.igraph <- function(.data, attribute){
  types <- unique(node_attribute(.data, attribute))
  lapply(types, function(x) igraph::induced_subgraph(.data, 
                              node_attribute(.data, attribute) == x))
}

#' @export
to_subgraphs.tbl_graph <- function(.data, attribute){
  lapply(to_subgraphs(as_igraph(.data), attribute), as_tidygraph)
}

#' @export
to_subgraphs.network <- function(.data, attribute){
  lapply(to_subgraphs(as_igraph(.data), attribute), as_network)
}

#' @rdname modif_split
#' @section `to_layers()`:
#'   The layers of a multiplex network are held in a tie attribute,
#'   `type` in tidygraph/igraph objects and `layer` in 'stocnet' objects.
#'   Each layer is extracted by `to_uniplex()`, so that the layers returned
#'   here are the same networks as retrieving them one at a time,
#'   and the returned list is named by the tie types found in the network.
#'   Where a network holds no tie types it is already uniplex,
#'   and a list of length one is returned.
#' @examples
#' as_tidygraph(create_filled(5)) |>
#'   mutate_ties(type = sample(c("friend", "enemy"), 10, replace = TRUE)) |>
#'   to_layers()
#' @export
to_layers <- function(.data) UseMethod("to_layers")

#' @export
to_layers.default <- function(.data){
  as_input(.data, to_layers)
}

#' @export
to_layers.tbl_graph <- function(.data){
  # layer_names() falls back to the network's tie label where there are no
  # layers, so the tie attribute is what detects multiplexity here.
  layer_attr <- .layer_attribute(.data)
  if(is.na(layer_attr)){
    snet_info("This network holds no tie types, so is already uniplex.")
    # With no layers, layer_names() falls back to the network's tie label,
    # which is the right name for the single layer returned here.
    return(stats::setNames(list(.data), layer_names(.data)[1] %||% "ties"))
  }
  types <- unique(tie_attribute(.data, layer_attr))
  stats::setNames(lapply(types, function(x) to_uniplex(.data, x)), types)
}

#' @export
to_layers.igraph <- function(.data){
  lapply(to_layers(as_tidygraph(.data)), as_igraph)
}

#' @export
to_layers.network <- function(.data){
  lapply(to_layers(as_tidygraph(.data)), as_network)
}

#' @export
to_layers.data.frame <- function(.data){
  lapply(to_layers(as_tidygraph(.data)), as_edgelist)
}

#' @rdname modif_split
#' @examples
#'   to_components(to_uniplex(fict_marvel, "relationship"))
#'   # Strong decomposition of a directed network returns many small components,
#'   # ordered here from largest to smallest, so just the largest is shown:
#'   to_components(fict_starwars, connectivity = "strong")[[1]]
#' @export
to_components <- function(.data,
                          connectivity = c("weak", "strong")) UseMethod("to_components")

#' @export
to_components.default <- function(.data, connectivity = c("weak", "strong")){
  as_input(.data, to_components, connectivity = connectivity)
}

#' @importFrom igraph decompose vcount
#' @export
to_components.igraph <- function(.data, connectivity = c("weak", "strong")){
  out <- igraph::decompose(.data, mode = match.arg(connectivity))
  # igraph returns components in discovery order, not size order,
  # so that to_components(.data)[[n]] matches to_component(.data, n).
  out[order(vapply(out, igraph::vcount, numeric(1)), decreasing = TRUE)]
}

#' @export
to_components.tbl_graph <- function(.data, connectivity = c("weak", "strong")){
  out <- to_components.igraph(as_igraph(.data), connectivity)
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_components.network <- function(.data, connectivity = c("weak", "strong")){
  out <- to_components.igraph(as_igraph(.data), connectivity)
  lapply(out, function(x) as_network(x))
}

#' @export
to_components.matrix <- function(.data, connectivity = c("weak", "strong")){
  out <- to_components.igraph(as_igraph(.data), connectivity)
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_components.data.frame <- function(.data, connectivity = c("weak", "strong")){
  out <- to_components.igraph(as_igraph(.data), connectivity)
  lapply(out, function(x) as_edgelist(x))
}

#' @rdname modif_split
#' @param attribute Character string indicating the date
#'   attribute in a network used to split into subgraphs.
#' @param panels Would you like to select certain waves?
#'   NULL by default.
#'   That is, a list of networks for every available wave is returned.
#'   Users can also list specific waves they want to select.
#' @param cumulative Whether to make wave ties cumulative.
#'   FALSE by default. That is, each wave is treated isolated.
#' @examples
#' ison_adolescents |>
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) |>
#'   to_waves(attribute = "wave")
#' @export
to_waves <- function(.data, attribute = "wave", panels = NULL,
                     cumulative = FALSE) UseMethod("to_waves")

#' @export
to_waves.default <- function(.data, attribute = "wave", panels = NULL,
                             cumulative = FALSE){
  as_input(.data, to_waves, attribute = attribute, panels = panels,
           cumulative = cumulative)
}

#' @importFrom tidygraph to_subgraph as_tbl_graph
#' @export
to_waves.tbl_graph <- function(.data, attribute = "wave", panels = NULL,
                               cumulative = FALSE) {
  out <- NULL
  if(is_changing(.data) && is_longitudinal(.data)){
    cl <- as_changelist(.data)
    # Waves are defined by the tie attribute; the changes recorded up to each
    # wave are then applied to that wave's nodes.
    if(!attribute %in% net_tie_attributes(.data))
      attribute <- intersect(c("wave", "panel", "time"),
                             net_tie_attributes(.data))[1]
    times <- sort(unique(tie_attribute(.data, attribute)))
    if(!is.null(panels))
      times <- intersect(panels, times)
    waves <- lapply(times, function(t) {
      out <- .apply_changes_upto(.data, cl, t)
      filter_ties(out, !!as.name(attribute) == t)
    })
    names(waves) <- paste("Wave", times)
    out <- waves
  } else if(is_changing(.data)){
    cl <- as_changelist(.data)
    # Get all unique times in order
    times <- sort(unique(cl$time))
    if(!is.null(panels))
      times <- intersect(panels, times)
    waves <- lapply(times, function(t) .apply_changes_upto(.data, cl, t))
    names(waves) <- paste("Wave", times)
    out <- waves
  } else if(is_longitudinal(.data) ||
            attribute %in% net_tie_attributes(.data)){
    # An explicitly named tie attribute is honoured even if the network is not
    # marked longitudinal (i.e. the attribute is not called "wave" or "panel").
    wp <- sort(unique(tie_attribute(.data, attribute)))
    if(!is.null(panels))
      wp <- intersect(panels, wp)
    if(length(wp) > 1) {
      out <- lapply(wp, function(l){
        filter_ties(.data, !!as.name(attribute) == l)
      })
      names(out) <- wp
    } else {
      out <- filter_ties(.data, !!as.name(attribute) == wp)
    }
    if (isTRUE(cumulative) && is.list(out) && !is_manynet(out)) {
      out <- .cumulative_ties(out, attribute)
      # Waves keep the natural (sorted) order of the attribute values;
      # ordering by names(out) here would sort numeric waves
      # lexicographically ("1", "10", "11", ..., "2", ...).
      out <- out[order(match(names(out), as.character(wp)))]
    }
  }
  if(is.null(out)) .data else out
}

#' @export
to_waves.igraph <- function(.data, attribute = "wave", panels = NULL,
                            cumulative = FALSE) {
  out <- to_waves(as_tidygraph(.data), attribute, panels, cumulative)
  # A single network is returned as a single network, not iterated over;
  # note that length() of a network is its number of nodes, so a list of
  # waves must be identified by class rather than by length.
  if(is.list(out) && !is_manynet(out)) lapply(out, as_igraph) else as_igraph(out)
}

#' @export
to_waves.data.frame <- function(.data, attribute = "wave", panels = NULL,
                                cumulative = FALSE) {
  if(!attribute %in% names(.data)) return(.data)
  wp <- sort(unique(.data[[attribute]]))
  if(!is.null(panels)) wp <- intersect(panels, wp)
  if(length(wp) > 1) {
    # Cumulative waves gather the ties of all earlier waves too, relabelled
    # to the wave they are gathered into. Edgelists are accumulated directly
    # rather than via .cumulative_ties(), since coercing a three-column
    # edgelist to a network would read the wave column as a tie weight.
    out <- lapply(seq_along(wp), function(k){
      keep <- if(isTRUE(cumulative)) .data[[attribute]] %in% wp[seq_len(k)] else
        .data[[attribute]] == wp[k]
      rows <- .data[keep, , drop = FALSE]
      if(isTRUE(cumulative)) rows[[attribute]] <- wp[k]
      rows
    })
    names(out) <- wp
  } else out <- .data[.data[[attribute]] %in% wp, , drop = FALSE]
  out
}

#' @export
to_waves.diff_model <- function(.data, attribute = "t", panels = NULL,
                                cumulative = FALSE) {
  if (!is.null(panels)) .data <- .data[.data[[attribute]] %in% panels,]
  if (length(unique(.data[["n"]])) > 1)
    snet_abort("Please make sure diffusion has the same number of nodes for all time points.")
  net <- as_tidygraph(.data)
  diff <- .data
  out <- list()
  for (k in .data[[attribute]]) {
    out[[paste("Time:", formatC(k, width = max(nchar(.data[[attribute]])),
                                flag = 0))]] <- net |>
      tidygraph::mutate(Infected = .node_is_infected(diff, time = k),
                        Exposed = .node_is_latent(diff, time = k),
                        Recovered = .node_is_recovered(diff, time = k))
  }
  if (isTRUE(cumulative)) {
    out <- .cumulative_ties(out, attribute)
  }
  out
}

# Applies to a network the changes recorded in its changelist up to and
# including time `t`, and then drops the changelist. For each nodal variable
# the latest change per node wins; changelist values are stored as character
# (or as a list-column) where changed variables are of different types, so
# they are coerced back to the type of the attribute they update.
.apply_changes_upto <- function(.data, changes, t){
  out <- .data
  changes <- changes[changes$time <= t, , drop = FALSE]
  if(nrow(changes)){
    changes <- changes[order(changes$time), , drop = FALSE]
    if(is.character(changes$node))
      changes$node <- match(changes$node, node_labels(.data))
    for(v in unique(changes$var)){
      upd <- changes[changes$var == v, , drop = FALSE]
      # Where a node changes more than once by time t, the latest wins
      upd <- upd[!duplicated(upd$node, fromLast = TRUE), , drop = FALSE]
      old <- node_attribute(out, v)
      new <- if(is.null(old)) rep(NA, net_nodes(out)) else
        if(is.factor(old)) old else as.vector(old)
      new[upd$node] <- .match_attribute_type(upd$value, old)
      out <- add_node_attribute(out, v, new)
    }
  }
  delete_changes(out)
}

# Coerces changelist values to the type of the nodal attribute they update,
# so that e.g. an "active" attribute stays logical rather than becoming
# character (or failing to combine at all).
.match_attribute_type <- function(value, old){
  if(is.list(value) && all(lengths(value) == 1L))
    value <- unlist(value, use.names = FALSE)
  if(is.null(old)) value
  else if(is.logical(old)) as.logical(value)
  else if(is.numeric(old)) as.numeric(value)
  else if(is.factor(old)) factor(as.character(value), levels = levels(old))
  else as.character(value)
}

.cumulative_ties <- function(x, attribute) {
  edges <- to <- from <- NULL
  thisRequires("zoo")
  thisRequires("purrr")
  ties <- data.frame()
  x <- lapply(x, as_tidygraph)
  for (k in seq_along(names(x))) {
    a <- x[[k]] |>
      tidygraph::activate(edges) |>
      dplyr::as_tibble() |>
      dplyr::mutate(order = k) |>
      dplyr::select(to, from, dplyr::all_of(attribute), order)
    ties <- rbind(ties, a)
  }
  if (is.numeric(ties[[attribute]])) {
    ties <- ties[order(ties[[attribute]]),]
    a <- list()
    for (k in unique(ties[[attribute]])) {
      if (k != unique(ties[[attribute]][1])) {
        a[[as.character(k)]] <- subset(ties, ties[[attribute]] < k)[1:3]
        a[[as.character(k)]][attribute] <- k
      }
    }
  } else {
    snet_info("Cumulative ties were added based on order of appearance for attribute.")
    a <- list()
    for (k in unique(ties$order)) {
      if (k != 1) {
        a[[unique(ties[[attribute]][k])]] <- subset(ties, ties$order < k)[1:3]
        a[[unique(ties[[attribute]][k])]][attribute] <- k
      }
    }
  }
  for (k in names(a)) {
    x[[k]] <- igraph::add_edges(
      x[[k]], c(a[[k]]$to, a[[k]]$from)[order(c(ceiling(seq_along(a[[k]]$to)/1),
                                                seq_along(a[[k]]$from)))],
      attr = a[[k]][3])
  }
  lapply(x, as_tidygraph)
}

#' @rdname modif_split
#' @param attribute One or two attributes used to slice data.
#' @param slice Character string or character list indicating the date(s)
#'   or integer(s) range used to slice data (e.g slice = c(1:2, 3:4)).
#' @examples
#' ison_adolescents |>
#'   mutate_ties(time = 1:10, increment = 1) |> 
#'   add_ties(c(1,2), list(time = 3, increment = -1)) |> 
#'   to_slices(slice = 7)
#' @export
to_slices <- function(.data, attribute = "time", slice = NULL) UseMethod("to_slices")

#' @export
to_slices.default <- function(.data, attribute = "time", slice = NULL){
  as_input(.data, to_slices, attribute = attribute, slice = slice)
}

#' @export
to_slices.tbl_graph <- function(.data, attribute = "time", slice = NULL) {
  # Without the time attribute there is nothing to slice on, so the network
  # is returned unchanged rather than filtering on a non-existent variable.
  if(!attribute %in% net_tie_attributes(.data)) return(.data)
  incremented <- "increment" %in% net_tie_attributes(.data)
  updated <- "replace" %in% net_tie_attributes(.data)
  if(!is.null(slice))
    moments <- slice else
      moments <- unique(tie_attribute(.data, attr_name = attribute))
  # Summarising ties introduces a weight, but ties can only be dropped for
  # having summed to zero where such a weight exists. The question here is
  # whether the ties carry a value at all, and not whether that value varies,
  # so the attribute is asked for directly rather than through `is_weighted()`.
  drop_zeroes <- function(x)
    if("weight" %in% net_tie_attributes(x)) filter_ties(x, weight != 0) else x
  if(length(moments)>1){
    out <- lapply(moments, function(tm){
      snap <- filter_ties(.data, !!as.name(attribute) <= tm)
      if(incremented) snap <- summarise_ties(snap, sum(increment))
      if(updated) snap <- summarise_ties(snap, dplyr::last(replace))
      snap <- drop_zeroes(snap)
      snap
    })
    names(out) <- moments
  } else {
    out <- filter_ties(.data, !!as.name(attribute) <= moments)
    if(incremented) out <- summarise_ties(out, sum(increment))
    if(updated) out <- summarise_ties(out, dplyr::last(replace))
    out <- drop_zeroes(out)
  }
  out
}

#' @export
to_slices.igraph <- function(.data, attribute = "time", slice = NULL) {
  out <- to_slices(as_tidygraph(.data), attribute, slice)
  if(is.list(out) && !is_manynet(out))
    lapply(out, function(ea) as_igraph(ea)) else
      as_igraph(out)
}
