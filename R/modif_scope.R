# Scoping ####

#' Modifying networks scope
#' @name modif_scope
#' @description
#'   These functions offer tools for transforming manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects).
#'   Transforming means that the returned object may have different dimensions
#'   than the original object.
#' 
#'   - `to_ego()` scopes a network into the local neighbourhood of a given node.
#'   - `to_component()` scopes a network to a single one of its components,
#'   either the `component`th largest or the one containing a named node.
#'   It is the singular counterpart of `to_components()`,
#'   which returns a list of all of them, largest first.
#'   - `to_giant()` scopes a network into one including only the main component
#'   and no smaller components or isolates.
#'   It is a wrapper, such that `to_giant(.data)` is `to_component(.data, 1)`.
#'   - `to_time()` scopes a longitudinal network to the network as it stood at a given wave or time point.
#'   `to_wave()` is an alias, using the wave-based vocabulary of `net_waves()` and `to_waves()`.
#'   For interval (spell) networks with tie `begin`/`end` lifespans, `to_time()`
#'   returns the ties active at that moment, or -- when `time` is omitted -- a
#'   list of slices, one per change point (each tie beginning or end).
#'   - `to_no_isolates()` scopes a network into one excluding all nodes without ties.
#'   - `to_no_missing()` scopes a network to one retaining only complete cases,
#'   i.e. nodes with no missing values.
#'   - `to_subgraph()` scopes a network into a subgraph by filtering on some node-related logical statement.
#'   - `to_blocks()` reduces a network to ties between a given partition membership vector.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(no_|ego|component$|subgraph|blocks)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_scope
#' @export
to_no_missing <- function(.data) UseMethod("to_no_missing")

#' @export
to_no_missing.default <- function(.data){
  as_input(.data, to_no_missing)
}

#' @export
to_no_missing.tbl_graph <- function(.data){
  out <- .data
  nl <- as_nodelist(out)
  if(is.null(nl)) return(out)
  delete_nodes(.data, !stats::complete.cases(nl)) |> 
    add_info(name = paste(net_name(.data), "without nodes with missing data"))
}


#' @rdname modif_scope
#' @param node Name or index of node.
#' @param max_dist The maximum breadth of the neighbourhood.
#'   By default 1.
#' @param min_dist The minimum breadth of the neighbourhood.
#'   By default 0. 
#'   Increasing this to 1 excludes the ego,
#'   and 2 excludes ego's direct alters.
#' @template param_dir
#' @export
to_ego <- function(.data, node, max_dist = 1, min_dist = 0,
                   direction = c("out","in")) UseMethod("to_ego")

#' @export
to_ego.default <- function(.data, node, max_dist = 1, min_dist = 0,
                           direction = c("out","in")){
  as_input(.data, to_ego, node, max_dist = max_dist, min_dist = min_dist,
           direction = direction)
}

#' @export
to_ego.igraph <- function(.data, node, max_dist = 1, min_dist = 0,
                          direction = c("out","in")){
  as_igraph(.to_ego_subgraph(.data, node, max_dist, min_dist, direction))
}

#' @export
to_ego.tbl_graph <- function(.data, node, max_dist = 1, min_dist = 0,
                             direction = c("out","in")){
  existname <- net_name(.data, prefix = "from")
  out <- as_tidygraph(.to_ego_subgraph(.data, node, max_dist, min_dist,
                                       direction))
  add_info(out, name = paste("Ego network of", node, existname))
}

# Obtains the neighbourhood of just this node.
# Note that to_egos() would obtain the neighbourhood of every node in the
# network before discarding all but this one, which does not scale.
.to_ego_subgraph <- function(.data, node, max_dist, min_dist, direction){
  direction <- match.arg(direction, c("out","in"))
  if(is_twomode(.data)) max_dist <- max_dist*2
  out <- igraph::make_ego_graph(as_igraph(.data), order = max_dist,
                                nodes = node, mindist = min_dist,
                                mode = direction)[[1]]
  out
}

#' @rdname modif_scope
#' @param time A time point or wave at which to present the network.
#'   For an interval (spell) network that records tie `begin`/`end` lifespans,
#'   `time` may be omitted, in which case a list of slices is returned,
#'   one per change point (each moment at which some tie begins or ends).
#' @details
#'   For interval (spell) networks, whose ties carry `begin`/`end` lifespans
#'   (e.g. `irps_wwi`), `to_time()` scopes to the ties active at `time`, using
#'   the half-open convention (`begin <= time < end`) shared with
#'   `network::networkDynamic`. When `time` is omitted, one such slice is
#'   returned for each distinct change point (every moment at which some tie
#'   begins or ends), as a named list, so that the evolving network can be
#'   iterated over or animated (e.g. with `autograph::grapht()`).
#' @examples
#'   # A single snapshot of the ties active in a given year:
#'   to_time(irps_wwi, 1901)
#'   # Or one slice per change point (each tie beginning or end):
#'   length(to_time(irps_wwi))
#' @export
to_time <- function(.data, time = NULL) UseMethod("to_time")

#' @export
to_time.default <- function(.data, time = NULL){
  as_input(.data, to_time, time = time)
}

#' @export
to_time.igraph <- function(.data, time = NULL){
  out <- to_time(as_tidygraph(.data), time)
  if(is.list(out) && !is_graph(out))
    lapply(out, as_igraph) else as_igraph(out)
}

#' @export
to_time.tbl_graph <- function(.data, time = NULL){
  # Interval/spell networks (begin/end tie lifespans) are handled first: with
  # `time` given, scope to the ties active then; with `time` omitted, return
  # one slice per change point. This precedes the wave-count guard below, which
  # is meaningless for spell networks and would error on a missing `time`.
  if(is_dynamic(.data) &&
     all(c("begin", "end") %in% net_tie_attributes(.data))){
    return(.to_time_spell(.data, time))
  }
  if(is.null(time))
    snet_abort("Please supply a {.arg time} (wave or time point) to scope to.")
  if(time > .net_waves(.data)){
    snet_info("Sorry, there are not that many waves in this dataset.",
              "Reverting to the maximum wave:", .net_waves(.data))
    time <- .net_waves(.data)
  }
  if(is_dynamic(.data)){
    snet_unavailable()
  } else if(is_longitudinal(.data)){
    out <- .data
    if(is_changing(out)){
      if(any(time >= as_changelist(.data)$time)){
        out <- apply_changes(out, time)
      } else {
        igraph::graph_attr(out, "changes") <- NULL
      } 
      if("active" %in% net_node_attributes(out)){
        out <- out |> 
          filter_nodes(active) |> 
          select_nodes(-active)
      }
    }
    if("wave" %in% net_tie_attributes(out)){
      out |>
        # trim ties
        filter_ties(wave == time) |>
        select_ties(-wave)
    } else out
  } else {
    .data
  }
}

#' @rdname modif_scope
#' @export
to_wave <- to_time

# Scopes an interval (spell) network -- one whose ties carry `begin`/`end`
# lifespans -- to the ties active at a moment `t` (begin <= t < end, the
# half-open convention that `network::networkDynamic` uses, so a tie that ends
# and one that begins at the same instant do not overlap). A tie with a missing
# `end` is treated as right-censored (active from its `begin` onwards).
# With `time` supplied, the single such snapshot is returned; with `time` NULL,
# one snapshot per change point (each distinct tie `begin` or `end`) is returned
# as a named list, ordered in time.
.to_time_spell <- function(.data, time = NULL){
  begin <- end <- NULL # bound within filter_ties()' tie data mask
  active_at <- function(t)
    filter_ties(.data, begin <= t & (is.na(end) | end > t))
  if(!is.null(time)) return(active_at(time))
  moments <- sort(unique(stats::na.omit(c(tie_attribute(.data, "begin"),
                                          tie_attribute(.data, "end")))))
  out <- lapply(moments, active_at)
  names(out) <- as.character(moments)
  if(length(out) == 1) out[[1]] else out
}

#' @rdname modif_scope
#' @param component Which component to retain.
#'   By default 1, i.e. the largest (giant) component,
#'   with 2 the second largest, and so on.
#'   Alternatively, the name of a node,
#'   in which case the component containing that node is retained.
#' @template param_connectivity
#' @examples
#'   to_component(fict_greys, 2)
#'   to_component(fict_greys, "Miranda Bailey")
#' @export
to_component <- function(.data, component = 1,
                         connectivity = c("weak", "strong")) UseMethod("to_component")

#' @export
to_component.default <- function(.data, component = 1,
                                 connectivity = c("weak", "strong")){
  as_input(.data, to_component, component = component,
           connectivity = connectivity)
}

#' @export
to_component.igraph <- function(.data, component = 1,
                                connectivity = c("weak", "strong")) {
  igraph::delete_vertices(.data,
                          !.to_component_ids(.data, component, connectivity))
}

#' @export
to_component.network <- function(.data, component = 1,
                                 connectivity = c("weak", "strong")) {
  keep <- .to_component_ids(as_igraph(.data), component, connectivity)
  network::delete.vertices(.data, which(!keep))
}

#' @export
to_component.tbl_graph <- function(.data, component = 1,
                                   connectivity = c("weak", "strong")) {
  out <- as_tidygraph(to_component(as_igraph(.data), component, connectivity))
  qual <- .connectivity_word(.data, connectivity)
  noun <- if(qual == "") "Component" else
    paste0(toupper(substring(qual, 1, 1)), substring(qual, 2), " component")
  prefix <- if(is.character(component))
    paste0(noun, " containing ", component, " of") else
      paste0(noun, " ", component, " of")
  add_info(out, name = paste(net_name(.data, prefix = prefix)))
}

#' @export
to_component.data.frame <- function(.data, component = 1,
                                    connectivity = c("weak", "strong")) {
  as_edgelist(to_component(as_igraph(.data), component, connectivity))
}

#' @export
to_component.matrix <- function(.data, component = 1,
                                connectivity = c("weak", "strong")) {
  as_matrix(to_component(as_igraph(.data), component, connectivity))
}

# Names the sense in which a component is connected, for reporting.
# Only directed networks are qualified, since the two notions coincide
# for undirected networks, where naming either would be misleading.
.connectivity_word <- function(.data, connectivity) {
  if(!is_directed(.data)) return("")
  match.arg(connectivity, c("weak", "strong"))
}

# Identifies which nodes belong to the requested component,
# whether named by size rank or by a node it contains.
.to_component_ids <- function(.data, component, connectivity) {
  comps <- igraph::components(.data, mode = match.arg(connectivity,
                                                      c("weak", "strong")))
  if(is.character(component)) {
    if(!is_labelled(.data))
      snet_abort("{.arg component} can only name a node in a labelled network.")
    idx <- match(component, node_labels(.data))
    if(length(component) != 1 || is.na(idx))
      snet_abort("{.val {component}} is not the name of a node in this network.")
    comps$membership == comps$membership[idx]
  } else {
    if(length(component) != 1 || component < 1 || component > comps$no)
      snet_abort(paste("{.arg component} must be a single number between 1 and",
                       "{comps$no}, the number of components in this network."))
    comps$membership == order(comps$csize, decreasing = TRUE)[component]
  }
}

#' @rdname modif_scope
#' @examples
#'   to_giant(fict_greys)
#' @export
to_giant <- function(.data, connectivity = c("weak", "strong")) {
  out <- to_component(.data, component = 1, connectivity = connectivity)
  if(inherits(out, "tbl_graph")) {
    qual <- .connectivity_word(.data, connectivity)
    prefix <- paste0("Giant ", if(qual == "") "" else paste0(qual, " "),
                     "component of")
    out <- add_info(out, name = paste(net_name(.data, prefix = prefix)))
  }
  out
}

#' @rdname modif_scope
#' @importFrom tidygraph node_is_isolated
#' @importFrom dplyr filter
#' @examples
#' ison_adolescents |>
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) |>
#'   to_waves(attribute = "wave") |>
#'   to_no_isolates()
#' @export
to_no_isolates <- function(.data) UseMethod("to_no_isolates")

#' @export
to_no_isolates.default <- function(.data){
  as_input(.data, to_no_isolates)
}

#' @export
to_no_isolates.tbl_graph <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices
  .data |> tidygraph::activate(nodes) |> 
    dplyr::filter(!tidygraph::node_is_isolated()) |> 
    add_info(name = paste(net_name(.data), "without isolates"))
}

#' @export
to_no_isolates.list <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices in each list
  lapply(.data, function(x) {
    x |> tidygraph::activate(nodes) |> dplyr::filter(!tidygraph::node_is_isolated())
  })
}

#' @export
to_no_isolates.igraph <- function(.data) {
  as_igraph(to_no_isolates(as_tidygraph(.data)))
}

#' @export
to_no_isolates.matrix <- function(.data) {
  as_matrix(to_no_isolates(as_tidygraph(.data)))
}

#' @export
to_no_isolates.network <- function(.data) {
  as_network(to_no_isolates(as_tidygraph(.data)))
}

#' @export
to_no_isolates.data.frame <- function(.data) {
  as_edgelist(to_no_isolates(as_tidygraph(.data)))
}

#' @rdname modif_scope
#' @param ... Arguments passed on to dplyr::filter
#' @importFrom dplyr filter
#' @export
to_subgraph <- function(.data, ...) UseMethod("to_subgraph")

#' @export
to_subgraph.default <- function(.data, ...){
  as_input(.data, to_subgraph, ...)
}

#' @export
to_subgraph.tbl_graph <- function(.data, ...){
  dplyr::filter(.data = .data, ..., 
                .preserve = FALSE)
}

#' @export
to_subgraph.igraph <- function(.data, ...){
  as_igraph(to_subgraph(as_tidygraph(.data), ...))
}

#' @export
to_subgraph.network <- function(.data, ...){
  as_network(to_subgraph(as_tidygraph(.data), ...))
}

#' @export
to_subgraph.data.frame <- function(.data, ...){
  as_edgelist(to_subgraph(as_tidygraph(.data), ...))
}

#' @export
to_subgraph.matrix <- function(.data, ...){
  as_matrix(to_subgraph(as_tidygraph(.data), ...))
}

#' @rdname modif_scope
#' @section `to_blocks()`: 
#'   Reduced graphs provide summary representations of network structures 
#'   by collapsing groups of connected nodes into single nodes 
#'   while preserving the topology of the original structures.
#' @param membership A vector of partition memberships.
#' @param FUN A function for summarising block content.
#'   By default `mean`.
#'   Other recommended options include `median`, `sum`,
#'   `min` or `max`.
#' @export
to_blocks <- function(.data, membership, FUN = mean) UseMethod("to_blocks")

#' @export
to_blocks.default <- function(.data, membership, FUN = mean){
  as_input(.data, to_blocks, membership, FUN)
}

#' @export
to_blocks.matrix <- function(.data, membership, FUN = mean){
  if(is_twomode(.data)){
    mat <- to_onemode(.data)
    m1_membs <- membership[!node_is_mode(.data)]
    m2_membs <- membership[node_is_mode(.data)]
    x <- length(unique(m1_membs))
    y <- length(unique(m2_membs))
    out <- matrix(nrow = unique(m1_membs)[x],
                  ncol = unique(m2_membs)[y])
    membership <- as.numeric(as.factor(membership))
    for(i in unique(m1_membs)) for (j in unique(m2_membs))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(unique(m1_membs)[x]))
    colnames(out) <- paste("Block", seq_len(unique(m2_membs)[y]))
  } else {
    mat <- .data
    membership <- as.numeric(as.factor(membership))
    parts <- max(membership)
    out <- matrix(nrow = parts, 
                  ncol = parts)
    for(i in seq_len(parts)) for (j in seq_len(parts))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(parts))
    colnames(out) <- paste("Block", seq_len(parts))
  }
  out[is.na(out)] <- 0
  out
}

#' @export
to_blocks.igraph <- function(.data, membership, FUN = mean){
  as_igraph(to_blocks(as_matrix(.data), membership, FUN))
}

#' @export
to_blocks.network <- function(.data, membership, FUN = mean){
  as_network(to_blocks(as_matrix(.data), membership, FUN))
}

#' @export
to_blocks.data.frame <- function(.data, membership, FUN = mean){
  as_edgelist(to_blocks(as_matrix(.data), membership, FUN))
}

#' @export
to_blocks.tbl_graph <- function(.data, membership, FUN = mean){
  as_tidygraph(to_blocks(as_matrix(.data), membership, FUN))
}

