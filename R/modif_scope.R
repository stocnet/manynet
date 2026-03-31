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
#'   - `to_giant()` scopes a network into one including only the main component and no smaller components or isolates.
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
#'   available_methods(collect_functions("to_.*(no_|ego|giant|subgraph|blocks)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_scope
#' @export
to_no_missing <- function(.data) UseMethod("to_no_missing")

#' @export
to_no_missing.tbl_graph <- function(.data){
  delete_nodes(.data, !stats::complete.cases(as_nodelist(.data))) %>% 
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
to_ego.igraph <- function(.data, node, max_dist = 1, min_dist = 0,
                          direction = c("out","in")){
  egos <- to_egos(.data, max_dist = max_dist, min_dist = min_dist,
                  direction = direction)
  as_igraph(egos[[node]])
}

#' @export
to_ego.tbl_graph <- function(.data, node, max_dist = 1, min_dist = 0,
                             direction = c("out","in")){
  egos <- to_egos(.data, max_dist = max_dist, min_dist = min_dist,
                  direction = direction)
  existname <- net_name(.data, prefix = "from")
  out <- as_tidygraph(egos[[node]])
  add_info(out, name = paste("Ego network of", node, existname))
}

#' @rdname modif_scope
#' @param time A time point or wave at which to present the network.
#' @export
to_time <- function(.data, time) UseMethod("to_time")

#' @export
to_time.tbl_graph <- function(.data, time){
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
        out <- out %>% 
          filter_nodes(active) %>% 
          select_nodes(-active)
      }
    }
    if("wave" %in% net_tie_attributes(out)){
      out %>% 
        # trim ties
        filter_ties(wave == time) %>% 
        select_ties(-wave)
    } else out
  } else {
    .data
  }
}

#' @rdname modif_scope
#' @export
to_giant <- function(.data) UseMethod("to_giant")

#' @export
to_giant.igraph <- function(.data) {
  comps <- igraph::components(.data)
  max.comp <- which.max(comps$csize)
  igraph::delete_vertices(.data, comps$membership != max.comp)
}

#' @export
to_giant.network <- function(.data) {
  comps <- igraph::components(as_igraph(.data))
  network::delete.vertices(.data, 
                           which(comps$membership != which.max(comps$csize)))
}

#' @export
to_giant.tbl_graph <- function(.data) {
  as_tidygraph(to_giant(as_igraph(.data))) %>% 
    add_info(name = paste(net_name(.data, prefix = "Giant component of")))
}

#' @export
to_giant.data.frame <- function(.data) {
  as_edgelist(to_giant(as_igraph(.data)))
}

#' @export
to_giant.matrix <- function(.data) {
  as_matrix(to_giant(as_igraph(.data)))
}

#' @rdname modif_scope
#' @importFrom tidygraph node_is_isolated
#' @importFrom dplyr filter
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   to_no_isolates()
#' @export
to_no_isolates <- function(.data) UseMethod("to_no_isolates")

#' @export
to_no_isolates.tbl_graph <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices
  .data %>% tidygraph::activate(nodes) %>% 
    dplyr::filter(!tidygraph::node_is_isolated()) %>% 
    add_info(name = paste(net_name(.data), "without isolates"))
}

#' @export
to_no_isolates.list <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices in each list
  lapply(.data, function(x) {
    x %>% tidygraph::activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
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

