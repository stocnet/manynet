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
#'   - `to_time()` scopes a network to the network as it stood at a given
#'   moment, in whichever of the ways set out in the Time section it records
#'   time. `to_wave()` is an alias, using the wave-based vocabulary of
#'   `net_waves()` and `to_waves()`. For one network per moment, see
#'   [to_times()].
#'   - `to_subgraph()` scopes a network into a subgraph by filtering on some node-related logical statement.
#'   - `to_blockmodel()` reduces a network to the ties between the blocks of a
#'   given partition membership vector.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(ego|component$|subgraph|blockmodel)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

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
  add_info(out, name = paste("Ego network of", node, existname)) |>
    .record_exclusion(.data, paste("outside the ego network of", node),
                      "nodes")
}

#' @export
to_ego.stocnet <- function(.data, node, max_dist = 1, min_dist = 0,
                           direction = c("out","in")){
  existname <- net_name(.data, prefix = "from")
  keep_nodes(.data, .to_ego_ids(.data, node, max_dist, min_dist, direction)) |>
    add_info(name = paste("Ego network of", node, existname)) |>
    .record_exclusion(.data, paste("outside the ego network of", node),
                      "nodes")
}

# The indices of the nodes in the neighbourhood, for the classes that rebuild
# from a nodelist rather than from a graph.
.to_ego_ids <- function(.data, node, max_dist, min_dist, direction){
  direction <- match.arg(direction, c("out","in"))
  if(is_twomode(.data)) max_dist <- max_dist*2
  sort(as.integer(igraph::ego(as_igraph(.data), order = max_dist,
                              nodes = node, mindist = min_dist,
                              mode = direction)[[1]]))
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
#' @param time A moment at which to present the network.
#'   A moment beyond the last one the network records reverts to the last,
#'   except in an interval network, which is defined between the moments it
#'   records as well as at them.
#' @section Time:
#'   Two things about how a network records time are independent of each
#'   other, and the network is scoped to a moment by both of them.
#'
#'   How a moment is *represented* can be read from the ties. A network either
#'   stamps each tie with the point it was recorded at, in a 'time' column, or
#'   states the interval each tie lasts over, in 'begin' and 'end' columns.
#'
#'   How a moment *relates to the one before it* cannot be read from the ties,
#'   and the network declares it in `info$update`. Where this is "replace",
#'   which it is by default, each moment re-states the ties, so the network at
#'   a moment is the ties stamped with it. Where it is "increment", each row
#'   is a change to a tie's value, so the network at a moment is every row up
#'   to and including it, accumulated, and a tie that has accumulated to zero
#'   is no longer a tie. An interval tie carries its own lifespan, so nothing
#'   is declared about it: it is active at a moment where it began at or
#'   before that moment and has not yet ended (`begin <= time < end`, the
#'   half-open convention shared with `network::networkDynamic`, so a tie that
#'   ends and one that begins at the same instant do not overlap). A tie with
#'   no 'end' is right-censored, and active from its beginning onwards.
#'
#'   `info$observation` records a third thing, how densely the network is
#'   observed: a "panel" of a few complete re-observations, or a stream of
#'   many "event" records. This describes a network rather than scoping it.
#'   The two go together without entailing each other: a panel re-observes
#'   the whole network at each wave and so is usually "replace", and an event
#'   stream is often "increment", but an event that states a value afresh is
#'   "replace" too. `is_longitudinal()` marks the first, `is_dynamic()` the
#'   second, and no network is both.
#'
#'   In every case the nodal changes recorded up to the moment are applied,
#'   and a layer that states something holding throughout is carried into
#'   whichever moment is asked for, since such a layer is a constant covariate
#'   rather than an observation of that moment. A layer holds throughout where
#'   the network declares it "cross-sectional", or where it records that layer
#'   at a single moment while another layer spans several.
#' @seealso [to_times()] for one network per moment, and [net_times()] for how
#'   many moments there are to ask for.
#' @examples
#'   # The ties a panel observed in a given wave:
#'   to_time(ison_monks, 2)
#'   # The ties an interval network held in a given year:
#'   to_time(irps_wwi, 1901)
#'   # The state an event network had accumulated to by a given day:
#'   to_time(irps_nuclear, as.Date("2011-04-01"))
#' @export
to_time <- function(.data, time = NULL) UseMethod("to_time")

#' @export
to_time.default <- function(.data, time = NULL){
  as_input(.data, to_time, time = time)
}

#' @export
to_time.igraph <- function(.data, time = NULL){
  as_igraph(.to_time(as_tidygraph(.data), time))
}

#' @export
to_time.tbl_graph <- function(.data, time = NULL){
  .to_time(.data, time)
}

#' @export
to_time.stocnet <- function(.data, time = NULL){
  # A stocnet holds what it knows about itself alongside its ties, so it is
  # scoped in place rather than routed through another class and back, which
  # is what would otherwise drop its info, its changes, and its missings.
  out <- .to_time(.data, time)
  if(is.null(out$missings) || !"time" %in% names(out$missings)) return(out)
  # The missings list the moments they were missing from the same way the ties
  # list the moments they were observed at, so they are scoped the same way.
  out$missings <- out$missings[out$missings$time == time, , drop = FALSE]
  # A component that holds nothing is NULL rather than an empty table, as it
  # is everywhere else a stocnet is built.
  if(!nrow(out$missings)) out$missings <- NULL else out$missings$time <- NULL
  out
}

#' @rdname modif_scope
#' @export
to_wave <- to_time

# The network as it stood at a moment, whichever way it records time.
# The representation is tested first, since an interval tie carries its own
# lifespan and so says nothing about how it relates to the moment before it.
.to_time <- function(.data, time = NULL){
  if(is.null(time))
    snet_abort(paste("Please supply a {.arg time} to scope to,",
                     "or use {.fn to_times} for one network per moment."))
  rule <- .time_rule(.data)
  if(rule == "none") return(.data)
  out <- .apply_changes_at(.data, time)
  at <- .clamp_time(.data, time, rule)
  out <- switch(rule,
                interval = .active_at(out, at),
                increment = .slice_at(out, .stamp_of(out), at),
                replace = .stamped_at(out, at))
  # the nodes and the ties are dropped by two different criteria, so each
  # is recorded on its own rather than summed into one figure
  out |>
    .record_exclusion(.data, paste("not present at time", time), "nodes") |>
    .record_exclusion(.data, paste("not tied at time", time), "ties")
}

# The nodes as they stood at a moment: the changes recorded up to then applied,
# the changelist dropped, and the nodes that are not in the network at that
# moment taken out with the 'active' column that said so.
.apply_changes_at <- function(.data, time){
  if(!is_changing(.data)) return(.data)
  out <- .apply_changes_upto(.data, as_changelist(.data), time)
  if("active" %in% net_node_attributes(out))
    out <- out |> filter_nodes(active) |> select_nodes(-active)
  out
}

# A moment beyond the last one a network records is the last one it records,
# for the ways of recording time whose moments are the observations themselves.
# An interval network is defined between its change points as well as at them,
# so it is not clamped: a tie can be active at a moment none begins or ends at.
.clamp_time <- function(.data, time, rule){
  if(rule == "interval") return(time)
  moments <- .time_moments(.data, changes = FALSE)
  if(is.null(moments)) return(time)
  last <- moments[length(moments)]
  if(!isTRUE(time > last)) return(time)
  snet_info("Sorry, there are not that many moments in this dataset.",
            "Reverting to the last:", last)
  last
}

# The column a network stamps its moments in.
.stamp_of <- function(.data){
  intersect(c("time", "wave", "panel"), net_tie_attributes(.data))[1]
}

# A network that re-states its ties at each moment holds, at a moment, the ties
# stamped with it. The stamp goes with them, since a network scoped to one
# moment no longer varies in time. A layer that states something holding
# throughout is carried in whichever moment is asked for.
.stamped_at <- function(.data, at){
  stamp <- .stamp_of(.data)
  if(is.na(stamp)) return(.data)
  invariant <- .invariant_layers(.data)
  layer <- intersect(c("layer", "type"), net_tie_attributes(.data))[1]
  keep <- .bare_time(tie_attribute(.data, stamp)) == at
  if(length(invariant) && !is.na(layer))
    keep <- keep | as.character(tie_attribute(.data, layer)) %in% invariant
  .keep_ties_of(.data, which(keep)) |>
    select_ties(-dplyr::all_of(stamp))
}

# A network whose ties carry the interval each lasts over holds, at a moment,
# the ties active then: begun by then and not yet ended. This is the half-open
# convention (begin <= time < end) that `network::networkDynamic` uses, so a
# tie that ends and one that begins at the same instant do not overlap. A tie
# with no end is right-censored, and active from its beginning onwards.
.active_at <- function(.data, at){
  atts <- net_tie_attributes(.data)
  begin <- .bare_time(tie_attribute(.data,
                                    intersect(c("begin", "beg", "start"), atts)[1]))
  end <- if("end" %in% atts) .bare_time(tie_attribute(.data, "end")) else
    rep(NA, length(begin))
  .keep_ties_of(.data, which(begin <= at & (is.na(end) | end > at)))
}

# Ties are kept by position, so that the moment being matched on is never
# looked up inside `filter_ties()`, whose data mask a tie attribute of the
# same name would otherwise mask.
.keep_ties_of <- function(.data, kept){
  if(inherits(.data, "stocnet")) return(keep_ties(.data, kept))
  # The vector is built before the call, since `filter_ties()` evaluates its
  # condition in a data mask where '.data' is the rlang pronoun instead.
  keep <- seq_len(as.numeric(net_ties(.data))) %in% kept
  filter_ties(.data, keep)
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
  .name_component(out, .data, component, connectivity)
}

#' @export
to_component.stocnet <- function(.data, component = 1,
                                 connectivity = c("weak", "strong")) {
  keep <- .to_component_ids(as_igraph(.data), component, connectivity)
  .name_component(keep_nodes(.data, which(keep)), .data, component,
                  connectivity)
}

# Names the component retained and records what was left outside it. The
# criterion names the same component the name does, so that `to_giant()`,
# which delegates here for component 1, needs no entry of its own.
.name_component <- function(out, .data, component, connectivity){
  qual <- .connectivity_word(.data, connectivity)
  noun <- if(qual == "") "Component" else
    paste0(toupper(substring(qual, 1, 1)), substring(qual, 2), " component")
  prefix <- if(is.character(component))
    paste0(noun, " containing ", component, " of") else
      paste0(noun, " ", component, " of")
  criterion <- if(is.character(component))
    paste0("not in the ", tolower(noun), " containing ", component) else
      paste0("not in ", tolower(noun), " ", component)
  add_info(out, name = paste(net_name(.data, prefix = prefix))) |>
    .record_exclusion(.data, criterion, "nodes")
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
  # A matrix or an edgelist has nowhere to hold a name; every other class
  # names the component the giant one rather than the first one.
  if(inherits(out, c("tbl_graph", "stocnet", "network"))) {
    qual <- .connectivity_word(.data, connectivity)
    prefix <- paste0("Giant ", if(qual == "") "" else paste0(qual, " "),
                     "component of")
    out <- add_info(out, name = paste(net_name(.data, prefix = prefix)))
  }
  out
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
  out <- dplyr::filter(.data = .data, ..., 
                       .preserve = FALSE)
  # the conditions themselves are the exclusion criteria GRAND asks for, so
  # they are deparsed rather than summarised, and negated because they say
  # which nodes were kept where the record says which were excluded
  .record_exclusion(out, .data, paste("not", .deparse_conditions(...)),
                    "nodes")
}

#' @export
to_subgraph.stocnet <- function(.data, ...){
  if(is.null(.data$nodes) || nrow(.data$nodes) == 0) return(.data)
  with_active_context(.data, "nodes", {
    node_df <- dplyr::mutate(.data$nodes, .orig_id = dplyr::row_number())
    kept <- dplyr::filter(node_df, ...)$.orig_id
    keep_nodes(.data, kept) |>
      .record_exclusion(.data, paste("not", .deparse_conditions(...)), "nodes")
  })
}

# Renders the conditions given to a filtering function back into the text the
# user wrote, for recording which nodes or ties an exclusion kept.
.deparse_conditions <- function(...){
  conds <- vapply(as.list(substitute(list(...)))[-1],
                  function(x) paste(deparse(x), collapse = " "), character(1))
  if(length(conds) == 0) return("a filter")
  paste(conds, collapse = " & ")
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
#' @section `to_blockmodel()`: 
#'   Reduced graphs provide summary representations of network structures 
#'   by collapsing groups of connected nodes into single nodes
#'   while preserving the topology of the original structures.
#'   Like the other scoping functions, the reduced graph is returned in the
#'   same class as the input: a blockmodel matrix of summarised block content
#'   for matrix input, and the corresponding weighted network otherwise.
#'   Memberships are usually obtained from one of the `node_in_*()` functions
#'   in `{netrics}`.
#' @param membership A vector of partition memberships.
#'   For two-mode networks this is a single vector covering the nodes in
#'   both modes, and not one vector per mode;
#'   the blocks of each mode are established separately from it,
#'   so that the result has one row per block of the first mode and
#'   one column per block of the second.
#' @param FUN A function for summarising block content.
#'   By default `mean`.
#'   Other recommended options include `median`, `sum`,
#'   `min` or `max`.
#' @export
to_blockmodel <- function(.data, membership, FUN = mean) UseMethod("to_blockmodel")

#' @export
to_blockmodel.default <- function(.data, membership, FUN = mean){
  as_input(.data, to_blockmodel, membership, FUN)
}

#' @export
to_blockmodel.matrix <- function(.data, membership, FUN = mean){
  if(is_twomode(.data)){
    # The incidence matrix already has the first mode in its rows and the
    # second in its columns, so blocks are read off it directly.
    mat <- .data
    modes <- node_is_mode(.data)
    if(length(membership) != length(modes))
      cli::cli_abort(paste("{.arg membership} should be a single vector of",
                           "length {length(modes)}, giving the partition of",
                           "the nodes in both modes,",
                           "but it is of length {length(membership)}."))
    # Each mode is recoded separately so that shared labels across the modes,
    # or labels that are not 1...k, still index the block matrix correctly.
    m1_membs <- as.factor(membership[!modes])
    m2_membs <- as.factor(membership[modes])
    x <- length(levels(m1_membs))
    y <- length(levels(m2_membs))
    out <- matrix(nrow = x, ncol = y)
    for(i in seq_len(x)) for (j in seq_len(y))
      out[i, j] <- FUN(mat[as.integer(m1_membs) == i,
                           as.integer(m2_membs) == j, drop = FALSE],
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(x))
    colnames(out) <- paste("Block", seq_len(y))
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
to_blockmodel.igraph <- function(.data, membership, FUN = mean){
  as_igraph(.blockmodel_info(as_tidygraph(to_blockmodel(as_matrix(.data),
                                                        membership, FUN)),
                             .data, membership, FUN))
}

#' @export
to_blockmodel.network <- function(.data, membership, FUN = mean){
  as_network(.blockmodel_info(as_tidygraph(to_blockmodel(as_matrix(.data),
                                                         membership, FUN)),
                              .data, membership, FUN))
}

# The reduced graph is built from a matrix, which holds nothing about the
# network it came from, so what that network recorded about itself is carried
# over rather than lost, and the aggregation recorded on top of it as GRAND
# item 4.5 asks.
.blockmodel_info <- function(out, .data, membership, FUN){
  .carry_info(out, .data) |>
    .record_transformation("aggregation",
                           paste0("nodes into ", length(unique(membership)),
                                  " blocks (", .fun_name(FUN), ")"))
}

#' @export
to_blockmodel.data.frame <- function(.data, membership, FUN = mean){
  as_edgelist(to_blockmodel(as_matrix(.data), membership, FUN))
}

#' @export
to_blockmodel.tbl_graph <- function(.data, membership, FUN = mean){
  .blockmodel_info(as_tidygraph(to_blockmodel(as_matrix(.data), membership,
                                              FUN)),
                   .data, membership, FUN)
}

