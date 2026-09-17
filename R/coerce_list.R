#' Coercing into lists or matrices
#' @name coerce_list
#' @description
#'   These functions coerce objects into different objects by extracting
#'   and translating the information contained in the original object:
#'   - `as_edgelist()` coerces the object into an edgelist, as data frames or tibbles.
#'   - `as_nodelist()` coerces the object into a nodelist, as a data frame or tibble.
#'   - `as_changelist()` coerces the object into a changelist, as a data frame or tibble.
#'   - `as_globallist()` coerces the object into a globallist, as a data frame or tibble.
#'   - `as_missinglist()` coerces the object into a list of the ties it records
#'   as missing, as a tibble.
#'   - `as_infolist()` coerces the object into a list of network-level information, 
#'   such as the names of the nodes and ties, if not given in the nodelist or edgelist.
#'   - `as_matrix()` coerces the object into an adjacency (one-mode/unipartite) or incidence (two-mode/bipartite) matrix.
#'   If the network is a cognitive social structure or an egocentric network
#'   (i.e. the edgelist contains a 'by' column naming who reported each tie),
#'   or a gossip network (with an 'about' column naming whom each tie is about),
#'   `as_matrix()` returns a three-dimensional array instead,
#'   with dimensions for senders, receivers, and reporters (or targets).
#'   Every node takes a row, a column, and a slice, in the order of the nodes,
#'   and a tie recorded as missing holds `NA`.
#'   Where such a network has several layers, a list of one array for each
#'   layer is returned.
#'   Where a network holds parallel ties, i.e. where `tie_is_parallel()` is TRUE
#'   for any tie, the cells of the matrix report how many ties join each pair
#'   of nodes, and so may be greater than one even where the network is
#'   neither weighted nor signed.
#'
#'   These coercions are extractive in the sense that they will lose any information that cannot be contained in the target format.
#'   for example, `as_matrix()` will lose any information about edge attributes, such as edge types or weights.
#' @family coercions
#' @template param_data
#' @template param_two
#' @details
#' Edgelists are expected to be held in data.frame or tibble class objects.
#' The first two columns of such an object are expected to be the
#' senders and receivers of a tie, respectively, and are typically
#' named "from" and "to" (even in the case of an undirected network).
#' These columns can contain integers to identify nodes or character
#' strings/factors if the network is labelled.
#' If the sets of senders and receivers overlap, a one-mode network is inferred.
#' If the sets contain no overlap, a two-mode network is inferred.
#' If a third, numeric column is present, a weighted network will be created.
#'
#' Matrices can be either adjacency (one-mode) or incidence (two-mode) matrices.
#' Incidence matrices are typically inferred from unequal dimensions,
#' but since in rare cases a matrix with equal dimensions may still
#' be an incidence matrix, an additional argument `twomode` can be
#' specified to override this heuristic.
#'
#' @examples
#' test <- data.frame(from = c("A","B","B","C","C"), to = c("I","G","I","G","H"))
#' as_edgelist(test)
#' as_matrix(test)
#' as_igraph(test)
#' as_tidygraph(test)
#' as_network(test)
#' @return
#' The currently implemented coercions or translations are:
#'
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("as.*(list|matrix)"))
#'   ```
NULL

# Nodelists ####

#' @rdname coerce_list
#' @export
as_nodelist <- function(.data) UseMethod("as_nodelist")

# `data.frame()` on a 'tbl_graph' returns whichever part of the network is
# active, so the nodes are activated first. Otherwise a network left with its
# ties active by an earlier function returns a tielist here instead.
#' @export
as_nodelist.tbl_graph <- function(.data) {
  nodes <- NULL
  out <- data.frame(activate(.data, nodes))
  out <- dplyr::tibble(out)
  if(ncol(out)==0) NULL else out
}

#' @export
as_nodelist.igraph <- function(.data) {
  as_nodelist(as_tidygraph(.data))
}

#' @export
as_nodelist.stocnet <- function(.data) {
  .data$nodes
}

#' @export
as_nodelist.matrix <- function(.data) {
  as_nodelist(as_igraph(.data))
}

#' @export
as_nodelist.data.frame <- function(.data) {
  as_nodelist(as_igraph(.data))
}

#' @export
as_nodelist.network <- function(.data) {
  out <- .data
  out <- network::as.data.frame.network(out, unit = "vertices", 
                                        attrs_to_ignore = NULL)
  if("na" %in% names(out) && sum(out$na)==0) out$na <- NULL
  if(is_twomode(.data)) out$mode <- c(rep(FALSE, .data$gal$bipartite),
                                 rep(TRUE, .data$gal$n - .data$gal$bipartite))
  if(is_labelled(.data)) out$label <- network::network.vertex.names(.data)
  if("vertex.names" %in% names(out)) out$vertex.names <- NULL
  out <- dplyr::as_tibble(out) |> 
    dplyr::select(dplyr::any_of(c("id", "name", "label", "mode")), 
                  dplyr::everything())
  if(ncol(out)==0) NULL else out
}

# Changelists ####

#' @rdname coerce_list
#' @param time A moment to gather the changes in force at.
#'   By default `NULL`, in which case every change the network records is
#'   returned. Where a moment is given, only the changes recorded up to and
#'   including it are returned, one for each nodal variable a node changes,
#'   since a change states what a variable becomes from that moment on and is
#'   carried forward until another change states otherwise.
#' @export
as_changelist <- function(.data, time = NULL) UseMethod("as_changelist")

#' @export
as_changelist.tbl_graph <- function(.data, time = NULL) {
  .gathered_at(.graph_changes(as_igraph(.data)), time)
}

#' @export
as_changelist.igraph <- function(.data, time = NULL) {
  .gathered_at(.graph_changes(.data), time)
}

#' @export
as_changelist.stocnet <- function(.data, time = NULL) {
  .gathered_at(.data$changes, time)
}

#' @export
as_changelist.network <- function(.data, time = NULL) {
  out <- network::get.network.attribute(.data, "changes")
  out <- dplyr::tibble(data.frame(out))
  .gathered_at(if(ncol(out)==0) NULL else out, time)
}

# Matrices and edgelists have nowhere to hold a changelist
#' @export
as_changelist.matrix <- function(.data, time = NULL) NULL

#' @export
as_changelist.data.frame <- function(.data, time = NULL) NULL

# The parameter is not named '.data', because `dplyr::tibble()` evaluates its
# arguments in a data mask where that name is the rlang pronoun instead.
.graph_changes <- function(x){
  out <- igraph::graph_attr(x, "changes")
  out <- dplyr::tibble(data.frame(out))
  if(ncol(out)==0) NULL else out
}

# The changes in force at a moment: those recorded up to and including it,
# reduced to the last value each node takes for each variable, since a change
# is carried forward until another change states otherwise.
.gathered_at <- function(changes, time){
  if(is.null(time) || is.null(changes) || !nrow(changes)) return(changes)
  t <- time
  changes |>
    dplyr::filter(time <= t) |>
    dplyr::arrange(node, var, time) |>
    dplyr::group_by(node, var) |>
    dplyr::mutate(value = dplyr::last(value)) |>
    dplyr::distinct(node, var, value) |>
    dplyr::ungroup()
}

# Edgelists ####

#' @rdname coerce_list
#' @importFrom igraph as_data_frame
#' @importFrom dplyr as_tibble arrange
#' @importFrom network get.edge.attribute as.edgelist
#' @export
as_edgelist <- function(.data, twomode = FALSE) UseMethod("as_edgelist")

#' @export
as_edgelist.igraph <- function(.data, twomode = FALSE) {
  out <- igraph::as_data_frame(.data, what = "edges") |>
    dplyr::as_tibble()
  if(ncol(out)==0) NULL else out
}

#' @export
as_edgelist.tbl_graph <- function(.data, twomode = FALSE) {
  out <- igraph::as_data_frame(.data, what = "edges") |>
    dplyr::as_tibble()
  if(ncol(out)==0) NULL else out
}

#' @export
as_edgelist.network <- function(.data, twomode = FALSE) {
  out <- network::as.edgelist(.data)
  edges <- as.data.frame(out)
  if (is_twomode(.data)) {
    edges <- edges[((nrow(edges)/2) + 1):nrow(edges),]
  }
  from <- to <- NULL
  # Handle node names
  if (is_labelled(.data)) {
    names <- attr(out, "vnames")
    edges[,1] <- names[edges[,1]]
    edges[,2] <- names[edges[,2]]
  }
  # Handle edge weights, which where a network is signed may hold only signs
  if ("weight" %in% network::list.edge.attributes(.data)) {
    edges[,3] <- network::get.edge.attribute(.data, "weight")
    names(edges) <- c("from", "to", "weight")
    # Remove weight column if only unity weights.
    if (all(edges$weight == 1)) edges <- edges[, -3]
  } else names(edges) <- c("from", "to")
  out <- dplyr::arrange(dplyr::as_tibble(edges), from, to)
  if(ncol(out)==0) NULL else out
}

#' @export
as_edgelist.matrix <- function(.data, twomode = FALSE) {
  out <- as_edgelist(as_igraph(.data, twomode = twomode))
  if(ncol(out)==0) NULL else out
}

#' @export
as_edgelist.data.frame <- function(.data, twomode = FALSE) {
  if (ncol(.data) == 2 && any(names(.data) != c("from", "to"))) {
    names(.data) <- c("from", "to")
    .data
  } else if(ncol(.data) == 3 &&
            (any(names(.data) != c("from", "to", "weight")) |
            any(names(.data) != c("from", "to", "sign"))) &&
            # a third column naming the reporter or target of each tie is not
            # a weight
            !(identical(names(.data)[1:2], c("from", "to")) &&
              names(.data)[3] %in% c("by", "about"))) {
    names(.data) <- c("from", "to", "weight")
    .data
  } else .data
}

#' @export
as_edgelist.network.goldfish <- function(.data, twomode = FALSE) {
  out <- as_matrix(as_igraph(.data, twomode = twomode))
  if(ncol(out)==0) NULL else out
}

#' @export
as_edgelist.siena <- function(.data, twomode = NULL) {
  out <- as_edgelist(as_igraph(.data, twomode = twomode))
  if(ncol(out)==0) NULL else out
}

#' @export
as_edgelist.stocnet <- function(.data, twomode = NULL) {
  # An edgelist lists arcs where the network is directed, so any layer held
  # once per dyad is reciprocated here. This also serves `as_igraph.stocnet()`
  # and, through it, every other class coerced to via igraph.
  out <- .reciprocate_layers(.data)
  # The reporter and the target of a tie are nodes too, so they are named
  # the way its ends are.
  if(is_labelled(.data)) out <- .label_tie_nodes(out, .data$nodes$label)
  if(ncol(out)==0) NULL else out
}

# Infolists ####

#' @rdname coerce_list
#' @importFrom utils modifyList
#' @export
as_infolist <- function(.data) UseMethod("as_infolist")

#' @export
as_infolist.igraph <- function(.data){
  out <- igraph::graph_attr(.data)
  # A 'grand' attribute holds the network's metadata as a list of its own,
  # which is merged with the other attributes rather than replacing them,
  # since those are network metadata too.
  if("grand" %in% names(out)){
    grand <- out$grand
    out <- out[setdiff(names(out), "grand")]
    if(is.list(grand)){
      if("mode" %in% names(grand)) grand$mode <- NULL
      out <- utils::modifyList(out, grand)
    }
  }
  if("changes" %in% names(out)) out$changes <- NULL
  if("missings" %in% names(out)) out$missings <- NULL
  if("globals" %in% names(out)) out$globals <- NULL
  if("global" %in% names(out)) out$global <- NULL
  out
}

#' @export
as_infolist.tbl_graph <- function(.data){
  out <- igraph::graph_attr(.data)
  # A 'grand' attribute holds the network's metadata as a list of its own,
  # which is merged with the other attributes rather than replacing them,
  # since those are network metadata too.
  if("grand" %in% names(out)){
    grand <- out$grand
    out <- out[setdiff(names(out), "grand")]
    if(is.list(grand)){
      if("mode" %in% names(grand)) grand$mode <- NULL
      out <- utils::modifyList(out, grand)
    }
  }
  if("changes" %in% names(out)) out$changes <- NULL
  if("missings" %in% names(out)) out$missings <- NULL
  if("globals" %in% names(out)) out$globals <- NULL
  if("global" %in% names(out)) out$global <- NULL
  out
}

#' @export
as_infolist.stocnet <- function(.data) {
  .data$info
}

#' @export
as_infolist.network <- function(.data) {
  .data$gal
}

# Matrices and edgelists have nowhere to hold network-level information
#' @export
as_infolist.matrix <- function(.data) NULL

#' @export
as_infolist.data.frame <- function(.data) NULL

# Globallists ####

#' @rdname coerce_list
#' @export
as_globallist <- function(.data) UseMethod("as_globallist")

#' @export
as_globallist.stocnet <- function(.data) {
  .data$globals
}

# The attribute was named 'global' before the component was renamed 'globals',
# so objects saved by an earlier version are still read here.
#' @export
as_globallist.igraph <- function(.data) {
  out <- igraph::graph_attr(.data, "globals") %||%
    igraph::graph_attr(.data, "global")
  if(is.null(out) || ncol(out)==0) NULL else out
}

#' @export
as_globallist.network <- function(.data) {
  out <- network::get.network.attribute(.data, "globals") %||%
    network::get.network.attribute(.data, "global")
  if(is.null(out) || ncol(out)==0) NULL else out
}

# Matrices and edgelists have nowhere to hold a globallist
#' @export
as_globallist.matrix <- function(.data) NULL

#' @export
as_globallist.data.frame <- function(.data) NULL

# Missinglists ####

#' @rdname coerce_list
#' @section Missing ties:
#'   A missing tie is one that could have been observed and was not,
#'   which is neither a tie nor the absence of one.
#'   `as_missinglist()` returns them as a tibble of 'from' and 'to',
#'   with 'layer' and 'time' where the network records them.
#'   
#'   Compared to an **observed** tie, in which a tie is observed to be present
#'   or absent, with only present ties appearing as rows in the ties component, 
#'   a **missing** tie is one that could have been observed but was not.
#'   There are four different reasons a tie could be missing, 
#'   and they are treated differently depending on the context.
#'
#'   First, there are the nodal reasons a tie could be missing, 
#'   which are recorded in the nodes and changes components.
#'   1. **Non-availability**. A node was not in the network and so cannot
#'   send or receive ties.
#'   This is recorded in the 'active' column of the nodes component,
#'   and can change over time through the changes component.
#'   Note that this renders all outgoing and incoming ties missing.
#'   2. **Non-response**. A node was in the network but chose not to respond
#'   or report its ties. 
#'   This is recorded in the 'na' column of the nodes component,
#'   and can also change over time through the changes component.
#'   Note that this renders only outgoing ties missing for a directed layer,
#'   and both directions for an undirected layer.
#'   
#'   Second, a tie might be missing or missing some information,
#'   even though both nodes were in the network. This is recorded in the ties.
#'   1. **Unobserved tie**. One or more specific ties could have been reported
#'   and were not, such as one name a respondent skipped.
#'   These are held in the missings component,
#'   as a tibble of 'from' and 'to', with 'layer' and 'time' where available.
#'   2. **Unobserved weight**. One or more ties are reported as existing,
#'   but the strength of the tie is not known. 
#'   This is recorded as an `NA` in the 'weight' column of the ties component.
#'
#'   Note that missing ties are not ties. 
#'   `net_ties()` does not count them,
#'   `as_edgelist()` does not return them, 
#'   and they are not drawn or measured unless a function asks for them by name.
#'   Where necessary, `as_missinglist()` returns a list of all missing ties together.
#'   `net_tie_missing()` reports how many there are,
#'   and `impute_ties()` imputes them.
#'
#'   Each class holds them differently.
#'   A stocnet object records which nodes did not report,
#'   from which `as_missinglist()` derives the ties;
#'   see `make_stocnet()` for how those records are held.
#'   So, unlike the other `as_*list()` functions,
#'   this one does not return a component verbatim:
#'   it returns the missings component together with the ties that the
#'   non-responsive nodes imply, which is nearly always the larger part.
#'   An 'igraph' or 'tbl_graph' object carries the list in a graph attribute,
#'   since igraph has no way to mark an edge as missing.
#'   A 'network' object holds each as an edge marked in the reserved 'na'
#'   attribute, which is that package's own format and the one `{ergm}` expects.
#'   A matrix holds each as a missing cell.
#' @examples
#' as_missinglist(ison_classmates)
#' @export
as_missinglist <- function(.data) UseMethod("as_missinglist")

#' @export
as_missinglist.stocnet <- function(.data) {
  out <- .expand_missing(.data)
  if(!nrow(out)) NULL else out
}

#' @export
as_missinglist.igraph <- function(.data) {
  out <- igraph::graph_attr(.data, "missings")
  if(is.null(out) || !nrow(out)) NULL else dplyr::as_tibble(out)
}

#' @export
as_missinglist.tbl_graph <- function(.data) {
  as_missinglist(as_igraph(.data))
}

#' @export
as_missinglist.network <- function(.data) {
  if(!"na" %in% network::list.edge.attributes(.data)) return(NULL)
  edf <- network::as.data.frame.network(.data, unit = "edges", na.rm = FALSE)
  na <- unlist(network::get.edge.attribute(.data, "na", null.na = FALSE))
  if(!any(na)) return(NULL)
  out <- edf[na, , drop = FALSE]
  vnames <- as.character(network::network.vertex.names(.data))
  out$from <- match(as.character(out$.tail), vnames)
  out$to <- match(as.character(out$.head), vnames)
  out[c(".tail", ".head", "na")] <- NULL
  dplyr::as_tibble(out) |>
    dplyr::select("from", "to", dplyr::everything())
}

#' @export
as_missinglist.matrix <- function(.data) {
  idx <- which(is.na(.data), arr.ind = TRUE)
  if(!nrow(idx)) return(NULL)
  to <- idx[, 2]
  # A two-mode matrix numbers its columns within the second mode, whereas a
  # nodelist numbers every node across both.
  if(is_twomode(.data)) to <- to + nrow(.data)
  dplyr::tibble(from = as.integer(idx[, 1]), to = as.integer(to))
}

#' @export
as_missinglist.data.frame <- function(.data) NULL

#' @export
as_missinglist.default <- function(.data) {
  as_missinglist(as_igraph(.data))
}

# Matrices ####

#' @rdname coerce_list
#' @importFrom dplyr arrange
#' @importFrom igraph edge_attr_names as_adjacency_matrix as_biadjacency_matrix
#' @importFrom network is.bipartite list.edge.attributes as.matrix.network
#' @export
as_matrix <- function(.data,
                      twomode = NULL) UseMethod("as_matrix")

# Which tie column, if any, a network names a third node in: 'by' for the
# reporter of each tie, whether a reporter or an ego, and 'about' for its
# target. A matrix gains a third dimension for it.
.third_node_col <- function(.data){
  held <- c(by = is_cognitive(.data) || is_egocentric(.data),
            about = is_gossip(.data))
  if(all(held))
    snet_abort("This network names both the reporter and the target of its",
               "ties, which a three-dimensional array cannot hold together.",
               "Please drop one of the 'by' and 'about' columns first.")
  names(held)[held][1]
}

# A network whose ties name a third node, as a three-dimensional array of
# senders, receivers, and that node. Every node takes its row, column, and
# slice in the order of the nodelist, so that isolates, and reporters who
# reported nothing, keep theirs. A tie recorded as missing holds NA.
# Each layer is a separate array, since one array holds one relation, and a
# layer whose ties name no third node gives an ordinary matrix.
.third_node_array <- function(.data, col = "by"){
  net <- as_stocnet(.data)
  ties <- net$ties
  if(!.holds_node(ties[[col]]))
    snet_abort("Expected a network naming a node in a '{col}' column of its ties.")
  layers <- if(!is.null(ties[["layer"]])) unique(as.character(ties$layer)) else NULL
  if(length(layers) > 1){
    out <- lapply(layers, function(layer){
      sub <- to_uniplex(net, layer)
      if(.holds_node(sub$ties[[col]])) .third_node_array(sub, col) else
        as_matrix(sub)
    })
    return(stats::setNames(out, layers))
  }
  n <- nrow(net$nodes) %||%
    max(c(ties$from, ties$to, ties[[col]]), na.rm = TRUE)
  labels <- net$nodes[["label"]]
  if(is_twomode(net)){
    modes <- as.character(net$nodes$mode)
    rows <- which(modes == modes[1])
    cols <- setdiff(seq_len(n), rows)
  } else rows <- cols <- seq_len(n)
  out <- array(0, dim = c(length(rows), length(cols), n),
               dimnames = if(is.null(labels)) NULL else
                 list(labels[rows], labels[cols], labels))
  undirected <- !is_directed(net) && !is_twomode(net)
  cells <- function(tab){
    idx <- cbind(match(tab$from, rows), match(tab$to, cols), tab[[col]])
    if(undirected) idx <- rbind(idx, idx[idx[, 1] != idx[, 2], c(2, 1, 3),
                                         drop = FALSE])
    idx[stats::complete.cases(idx), , drop = FALSE]
  }
  ties <- ties[!is.na(ties[[col]]), , drop = FALSE]
  if(nrow(ties)){
    ties$value <- if("weight" %in% names(ties)) ties$weight else 1
    if(undirected){
      rev <- ties[ties$from != ties$to, , drop = FALSE]
      rev[c("from", "to")] <- rev[c("to", "from")]
      ties <- rbind(ties, rev)
    }
    idx <- cbind(match(ties$from, rows), match(ties$to, cols), ties[[col]])
    keep <- stats::complete.cases(idx)
    idx <- idx[keep, , drop = FALSE]
    # Parallel records of one tie are counted, as `as_matrix()` counts them in
    # a matrix of two dimensions.
    key <- paste(idx[, 1], idx[, 2], idx[, 3])
    sums <- rowsum(ties$value[keep], key, reorder = FALSE)
    out[idx[!duplicated(key), , drop = FALSE]] <- sums[, 1]
  }
  missing <- as_missinglist(net)
  if(!is.null(missing) && !is.null(missing[[col]])){
    missing <- missing[!is.na(missing[[col]]), , drop = FALSE]
    if(nrow(missing)) out[cells(missing)] <- NA
  }
  out
}

#' @export
as_matrix.data.frame <- function(.data,
                                 twomode = NULL) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  if ("tbl_df" %in% class(.data)) .data <- as.data.frame(.data)
  # A third column of nothing but ones and zeroes is not a weight, but where
  # any of its values are missing it still has to be read, since a tie recorded
  # as missing cannot be recovered from a count of the ties.
  valued <- ncol(.data) >= 3 &&
    (is_weighted(.data) | is_signed(.data) | anyNA(.data[, 3]))
  if (!valued) {
    .data <- data.frame(.data) # in case it's a tibble
    .data <- as.data.frame(table(c(.data[,1]), c(.data[,2])))
    names(.data) <- c("from","to","weight")
  }
  if (ncol(.data) == 3) {
    # Adds a third (weight) column to a two-column edgelist
    # .data <- .data[order(.data[,1], .data[,2]),]
    nodes1 <- as.character(unique(.data[,1]))
    nodes1 <- sort(nodes1)
    nodes2 <- as.character(unique(.data[,2]))
    nodes2 <- sort(nodes2)
    if(length(intersect(nodes1, nodes2)) > 0 &
       !setequal(nodes1, nodes2))
      nodes1 <- nodes2 <- sort(unique(c(nodes1,nodes2)))
    if (nrow(.data) != length(nodes1)*length(nodes2)) {
      allcombs <- expand.grid(nodes1, nodes2, stringsAsFactors = FALSE)
      allcombs <- subset(allcombs, !duplicated(allcombs))
      names(allcombs) <- c("from","to")
      .data <- merge(allcombs, .data, all.x = TRUE)
      .data <- .data[order(.data[,2], .data[,1]),]
      .data[is.na(.data)] <- 0
    }
    .data <- dplyr::arrange(.data,
                             as.character(.data$to),
                             as.character(.data$from))
    .data <- structure(as.numeric(.data[,3]),
                     dim = c(as.integer(length(nodes1)),
                             as.integer(length(nodes2))),
                     dimnames = list(nodes1, nodes2))
  }
  if(!is_twomode(.data) && all(rownames(.data) == as.character(seq_nodes(.data)))) attr(.data, "dimnames") <- NULL
  if(!is_twomode(.data) && sum(.data[lower.tri(.data)])==0) .data <- .data + t(.data)
  .data
}

#' @export
as_matrix.matrix <- function(.data,
                             twomode = NULL) {
  .data
}

# A matrix can hold just one value per tie, so where a network is weighted or
# signed the cells take whichever of these attributes the network actually has,
# preferring the weights where it has both.
.tie_value_attribute <- function(.data){
  attrs <- igraph::edge_attr_names(.data)
  if("weight" %in% attrs) "weight" else
    if("sign" %in% attrs) "sign" else NULL
}

# A network holds a tie recorded as missing as a tie of missing value,
# so the cells of such ties must be filled from that attribute and not left
# to a count of the ties, which would report them as present.
.holds_missing_ties <- function(.data){
  val <- .tie_value_attribute(.data)
  !is.null(val) && anyNA(igraph::edge_attr(.data, val))
}

# In a multiplex network, a layer that records no values at all leaves its
# ties without one, which igraph reports as a missing value just as it does a
# tie recorded as missing. The two are told apart by their layer: a layer
# holding some values but not others has ties that are genuinely missing,
# while a layer holding none simply records which of its ties are present.
.unvalued_layer_ties <- function(.data){
  attrs <- igraph::edge_attr_names(.data)
  layer <- if("layer" %in% attrs) "layer" else if("type" %in% attrs) "type" else NULL
  val <- .tie_value_attribute(.data)
  # Without layers, or without values, no tie is left without a value it might
  # otherwise have held.
  if(is.null(layer) || is.null(val)) return(integer(0))
  vals <- igraph::edge_attr(.data, val)
  lyr <- as.character(igraph::edge_attr(.data, layer))
  valued <- vapply(split(!is.na(vals), lyr), any, logical(1))
  which(lyr %in% names(valued)[!valued])
}

# A matrix marks a missing tie by holding nothing in that cell, which is the
# only thing it can say and all that `net_tie_missing()` needs from it.
.blank_missing <- function(mat, .data){
  missing <- as_missinglist(.data)
  if(is.null(missing) || !nrow(missing)) return(mat)
  from <- missing$from; to <- missing$to
  if(!is.numeric(from)){
    from <- match(as.character(from), rownames(mat))
    to <- match(as.character(to), colnames(mat))
  } else if(!is.null(dim(mat)) && nrow(mat) != ncol(mat)) to <- to - nrow(mat)
  idx <- cbind(from, to)
  idx <- idx[!is.na(idx[, 1]) & !is.na(idx[, 2]) &
               idx[, 1] <= nrow(mat) & idx[, 2] <= ncol(mat), , drop = FALSE]
  if(nrow(idx)) mat[idx] <- NA
  if(!is_directed(.data) && nrow(idx) && nrow(mat) == ncol(mat))
    mat[idx[, c(2, 1), drop = FALSE]] <- NA
  mat
}

#' @export
as_matrix.igraph <- function(.data,
                             twomode = NULL) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  if ((!is.null(twomode) && twomode) |
      (is.null(twomode) & is_twomode(.data) & !is_multiplex(.data))) {
    if (is_weighted(.data) | is_signed(.data) | .holds_missing_ties(.data)) {
      mat <- igraph::as_biadjacency_matrix(.data, sparse = FALSE,
                                           attr = .tie_value_attribute(.data))
    } else {
      mat <- igraph::as_biadjacency_matrix(.data, sparse = FALSE,
                                           attr = NULL)
    }
  } else {
    if (is_weighted(.data) | is_signed(.data) | .holds_missing_ties(.data)) {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = .tie_value_attribute(.data))
      if(anyNA(mat) && is_multiplex(.data)){
        el <- igraph::as_edgelist(.data, names = FALSE)[.unvalued_layer_ties(.data), ,
                                                        drop = FALSE]
        if(nrow(el)){
          mat[el] <- 1
          if(!igraph::is_directed(.data)) mat[el[, c(2, 1), drop = FALSE]] <- 1
        }
      }
    } else {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = NULL)
    }
  }
  mat <- .blank_missing(mat, .data)
  if(!is_labelled(.data)) attr(mat, "dimnames") <- NULL
  mat
}

#' @export
as_matrix.tbl_graph <- function(.data,
                                twomode = NULL) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  as_matrix(as_igraph(.data), twomode = twomode)
}

#' @export
as_matrix.network <- function(.data,
                              twomode = NULL) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  if (network::is.bipartite(.data)) {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      out <- network::as.matrix.network(.data,
                                 attrname = "weight",
                                 expand.bipartite = FALSE)
      # Note: if expand.bipartite is true it returns the adjacency matrix. If
      # false it returns the incidence matrix that we want. Use
      # to_multilevel(mat) on the resulting matrix to do the conversion if needed.
    } else {
      out <- network::as.matrix.network(.data,
                                 expand.bipartite = FALSE)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      out <- network::as.matrix.network(.data, attrname = "weight")
    } else {
      out <- network::as.matrix.network(.data)
    }
  }
  # because network can have vertex names that are integers (i.e. just node IDs), 
  # we remove them since they are really anonymous.
  if(is.integer(network::network.vertex.names(.data))){
    attr(out, "dimnames") <- NULL
  }
  out
}

#' @export
as_matrix.network.goldfish <- function(.data,
                                       twomode = FALSE) {
  as_matrix(as_igraph(.data, twomode = twomode))
}

#' @export
as_matrix.siena <- function(.data,
                            twomode = NULL) {
  # Get the dependent network(s) first
  # Identify all dyadic depvars
  dvs <- lapply(.data$depvars, function(x) is.matrix(x[,,1]) )
  ddvs <- names(which(dvs))
  # Add in first wave of first DV network
  out <- .data$depvars[[ddvs[1]]][,,1]
  # Add remaining waves
  for(d in 2:dim(.data$depvars[[ddvs[1]]])[3]) {
    out <- .data$depvars[[ddvs[1]]][,,d] + out
  }
  # Add other dyadic depvars
  if (length(ddvs) > 1) {
    for (l in 2:length(ddvs)) {
      for (d in seq_len(dim(.data$depvars[[ddvs[l]]])[3])) {
        out <- .data$depvars[[ddvs[l]]][,,d] + out
      }
    }
  }
  # Add dycCovars
  for (k in seq_along(.data$dycCovars)) {
    out <- .data$dycCovars[[ddvs[k]]] + out
  }
  # Add dyvCovars
  for (k in seq_along(.data$dyvCovars)) {
    for (d in seq_len(dim(.data$dyvCovars[[k]])[3])) {
      out <- .data$dyvCovars[[k]][,,d] + out
    }
  }
  out
}

#' @export
as_matrix.diff_model <- function(.data,
                                 twomode = FALSE) {
  as_matrix(as_igraph(.data, twomode = twomode))
}

#' @export
as_matrix.stocnet <- function(.data,
                                 twomode = FALSE) {
  third <- .third_node_col(.data)
  if (!is.na(third)) return(.third_node_array(.data, third))
  as_matrix(as_igraph(.data, twomode = twomode))
}

