#' Coercing into graph objects
#' @name coerce_graph
#' @description
#'   The `as_` functions in `{manynet}` coerce objects of any of the following common classes
#'   of social network objects in R into the declared class:
#'   - `as_igraph()` coerces the object into an `{igraph}` `graph` object.
#'   - `as_tidygraph()` coerces the object into a `{tidygraph}` `tbl_graph` object.
#'   - `as_network()` coerces the object into a `{network}` `network` object.
#'   - `as_graphAM()` coerces the object into a graph adjacency matrix.
#'   - `as_diffnet()` coerces a `diffusionModel` (see [as_diffusion()])
#'   into a `{netdiffuseR}` `diffnet` object.
#'
#'   An effort is made for all of these coercion routines to be as lossless
#'   as possible, though some object classes are better at retaining certain
#'   kinds of information than others.
#'   Note also that there are some reserved column names in one or more
#'   object classes, which could otherwise lead to some unexpected results.
#' @family coercions
#' @template param_data
#' @template param_two
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
#'   available_methods(collect_functions("^as_(igraph|tidygraph|network|graphAM|diffnet|hypergraph)$"))
#'   ```
NULL

# igraph ####

#' @rdname coerce_graph
#' @importFrom network list.edge.attributes as.sociomatrix
#' @export
as_igraph <- function(.data,
                      twomode = FALSE) UseMethod("as_igraph")

#' @importFrom igraph graph_from_data_frame 
#' @export
as_igraph.data.frame <- function(.data,
                                 twomode = FALSE) {
  if (inherits(.data, "tbl_df")) .data <- as.data.frame(.data)
  # Warn if no column named weight and weight set to true
  if ((is_weighted(.data) | is_signed(.data)) & !("weight" %in% names(.data))) {
    # a column of time or of layers is not a weight
    if(!names(.data)[3] %in% c("begin", "beg", "start", "end", "time", "wave",
                               "panel", "sign", "date", "layer", "type",
                               .not_weights))
      names(.data)[3] <- "weight"
    # snet_abort("Please rename the weight column of your dataframe to 'weight'")
  }
  if (!is_labelled(.data)) {
    graph <- igraph::graph_from_data_frame(.data,
                                           vertices = data.frame(name = 1:max(c(.data$from, .data$to))))
  } else graph <- igraph::graph_from_data_frame(.data)
  # An edgelist carries no directedness flag of its own, so directedness is
  # inferred from reciprocity, matching `is_directed.data.frame()`. That mark
  # cannot be called here because it routes back through `as_igraph()`, so
  # reciprocity is read off the graph just constructed. Without this the
  # coercion always returns a directed graph, and marks such as `is_acyclic()`
  # then disagree with every other representation of the same network.
  recip <- igraph::reciprocity(graph, mode = "default")
  if (isTRUE(recip == 0)) {
    # no mutual dyads, so no edges to merge: `each` keeps tie attributes
    graph <- igraph::as_undirected(graph, mode = "each")
  } else if (isTRUE(recip == 1)) {
    # every dyad is listed in both directions, so collapse the duplicates
    graph <- igraph::as_undirected(graph, mode = "collapse",
                                   edge.attr.comb = "first")
  }
  if (!is_labelled(.data)) {
    graph <- igraph::delete_vertex_attr(graph, "name")
  }
  # length(intersect(c(.data[,1]), c(.data[,2]))) == 0 && length(.data[,1])>1
  if (is_twomode(.data) || twomode) {
    igraph::V(graph)$type <- igraph::V(graph)$name %in% .data[,2]
  }
  graph
}

#' @importFrom igraph graph_from_biadjacency_matrix graph_from_adjacency_matrix
#' @export
as_igraph.matrix <- function(.data,
                             twomode = FALSE) {
  # A missing cell records a tie observed as missing, which igraph will not
  # accept in a matrix it builds a graph from. Such a cell is filled with one,
  # so that the tie is made, and its weight is set missing again afterwards.
  na_cells <- is.na(.data)
  missing_ties <- any(na_cells)
  if (missing_ties) .data[na_cells] <- 1
  twomode <- nrow(.data) != ncol(.data) | twomode
  valued <- missing_ties || !all(.data %in% c(0, 1))
  if (twomode) {
    graph <- igraph::graph_from_biadjacency_matrix(.data,
                                                   weighted = if(valued) TRUE else NULL,
                                                   directed = FALSE)
  } else {
    graph <- igraph::graph_from_adjacency_matrix(.data,
                                                 mode = ifelse(all(.data == t(.data)),
                                                               "max", "directed"),
                                                 weighted = if(valued) TRUE else NULL)
  }
  if (missing_ties) {
    el <- igraph::as_edgelist(graph, names = FALSE)
    # The nodes of a two-mode graph are numbered across both modes, whereas the
    # columns of the matrix they came from are numbered within the second.
    if (twomode) el[, 2] <- el[, 2] - nrow(.data)
    igraph::E(graph)$weight[na_cells[el]] <- NA
  }
  graph
}

#' @export
as_igraph.igraph <- function(.data,
                             twomode = FALSE) {
  class(.data) <- "igraph"
  .data
}

#' @export
as_igraph.tbl_graph <- function(.data,
                                twomode = FALSE) {
  class(.data) <- "igraph"
  .data
}

#' @export
as_igraph.network <- function(.data,
                              twomode = FALSE) {
  # A hyperedge has no two ends for an 'igraph' to tie, so a 'network' object
  # that holds hyperedges is read as a 'stocnet' and coerced from there.
  if (network::is.hyper(.data)) return(as_igraph(as_stocnet(.data)))
  # A 'network' object holds the ties it records as missing among its edges,
  # which a sociomatrix reports as missing cells and igraph refuses to build a
  # graph from. Those take the richer path, where they become a graph
  # attribute rather than edges, as igraph can mark no edge as missing.
  if (!is.null(as_missinglist(.data)))
    return(as_igraph(as_stocnet(.data), twomode = twomode))
  # A biadjacency matrix cannot carry direction, so a directed two-mode
  # network is read from its ties too.
  if (network::is.bipartite(.data) && isTRUE(.data$gal$directed))
    return(as_igraph(as_stocnet(.data), twomode = twomode))
  # Extract node attributes
  attr <- names(.data[[3]][[1]])
  # Convert to igraph
  if (network::is.bipartite(.data)) {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      graph <- network::as.sociomatrix(.data, attrname = "weight")
      graph <- igraph::graph_from_biadjacency_matrix(graph, weighted = TRUE)
    } else {
      graph <- network::as.sociomatrix(.data)
      graph <- igraph::graph_from_biadjacency_matrix(graph)
    }
    graph <- igraph::set_vertex_attr(graph, name = "type",
                                     value = c(rep(FALSE, .data$gal$bipartite),
                                rep(TRUE, .data$gal$n - .data$gal$bipartite)))
  } else {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      graph <- network::as.sociomatrix(.data, attrname = "weight")
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   weighted = TRUE,
                                                   mode = ifelse(.data$gal$directed,
                                                                 "directed",
                                                                 "max"))
    } else if (length(network::list.edge.attributes(.data)) > 1) {
      .data$gal$multiple <- FALSE
      graph <- network::as.sociomatrix(.data, attrname = network::list.edge.attributes(.data)[1])
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   weighted = TRUE,
                                                   mode = ifelse(.data$gal$directed,
                                                                 "directed",
                                                                 "max"))
    } else {
      graph <- network::as.sociomatrix(.data)
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   mode = ifelse(.data$gal$directed,
                                                                 "directed",
                                                                 "max"))
    }
  }
  # Add remaining node level attributes.
  # 'na' and 'vertex.names' are network's internal bookkeeping:
  # the latter is already carried by the sociomatrix's dimnames as 'name',
  # so copying it again would shadow those labels on the way back.
  for (a in setdiff(attr, c("na", "vertex.names"))) {
    graph <- igraph::set_vertex_attr(graph, name = a,
                                     value = sapply(.data[[3]], "[[", a))
  }
  # because network can have vertex names that are integers (i.e. just node IDs), 
  # we remove them since they are really anonymous.
  if(is.integer(network::network.vertex.names(.data))) 
    graph <- igraph::delete_vertex_attr(graph, "name")
  graph
}

# Undirected layers ####

# A stocnet holds each undirected tie once, on the dyad rather than on the arc.
# Other classes are directed or undirected as a whole, though, so a network
# with both directed and undirected layers must be coerced to a directed one.
# A single row would then read as a one-way arc, halving the degree of every
# node in that layer. These two helpers therefore reciprocate the undirected
# layers on the way out and collapse them again on the way back, so that a
# round trip returns the network it started from.

.reciprocate_layers <- function(.data){
  ties <- .data$ties
  directed <- .data$info$directed
  if(is.null(ties) || nrow(ties) == 0 || !"layer" %in% names(ties)) return(ties)
  if(is.null(directed) || is.null(names(directed))) return(ties)
  # Where the network is coerced as undirected anyway, one row per dyad is
  # already what the target class expects.
  if(!is_directed(.data)) return(ties)
  undirected <- names(directed)[!directed]
  add <- ties[ties$layer %in% undirected & ties$from != ties$to, , drop = FALSE]
  if(nrow(add) == 0) return(ties)
  add[c("from", "to")] <- add[c("to", "from")]
  # A layer marked undirected should not already hold both directions, but if
  # it does, the reverse tie is kept only once.
  add <- add[!.dyad_key(add) %in% .dyad_key(ties), , drop = FALSE]
  dplyr::bind_rows(ties, add)
}

# The ties a network records as missing, labelled where the network is, so that
# they can be written into whichever form the target class holds them in.
.missing_out <- function(.data){
  out <- as_missinglist(.data)
  # A structural zero is not missing, but it travels in the same table, marked
  # as fixed, so that it comes back as one.
  zeros <- if(inherits(.data, "stocnet")) .fixed_registry(.data) else NULL
  if(!is.null(zeros)){
    zeros$fixed <- TRUE
    if(!is.null(out) && nrow(out)) out$fixed <- FALSE
    out <- dplyr::bind_rows(out, zeros)
  }
  if(is.null(out) || !nrow(out)) return(NULL)
  if(all(is.na(out$layer))) out$layer <- NULL
  if(all(is.na(out$time))) out$time <- NULL
  for(col in c("by", "about"))
    if(!is.null(out[[col]]) && all(is.na(out[[col]]))) out[[col]] <- NULL
  if(is_labelled(.data)) out <- .label_tie_nodes(out, .data$nodes$label)
  out
}

# The reverse, for coercion back into a stocnet: the labels are matched to node
# indices again, so that the list can be compressed against the nodes.
.missing_in <- function(missing, nodes){
  if(is.null(missing) || !nrow(missing)) return(NULL)
  missing <- dplyr::as_tibble(missing)
  missing <- .index_tie_nodes(missing, nodes[["label"]])
  missing[!is.na(missing$from) & !is.na(missing$to), , drop = FALSE]
}

.collapse_layers <- function(ties, directed, directed_object){
  # Without a record of which layer is which, there is nothing to collapse and
  # the caller falls back to describing the network as directed or not.
  if(is.null(directed) || is.null(names(directed))) return(NULL)
  if(is.null(ties) || nrow(ties) == 0 || !"layer" %in% names(ties))
    return(list(ties = ties, directed = directed))
  # An undirected object holds one row per dyad in every layer already, so no
  # layer of it can be directed and none needs collapsing.
  if(!directed_object){
    directed[] <- FALSE
    return(list(ties = ties, directed = directed))
  }
  for(layer in names(directed)[!directed]){
    rows <- which(ties$layer %in% layer)
    if(length(rows) == 0) next
    keys <- .dyad_key(ties[rows, , drop = FALSE], ordered = FALSE)
    sizes <- table(keys)
    loops <- ties$from[rows] == ties$to[rows]
    # Only a fully reciprocated layer can be collapsed without losing a tie.
    # Where it is not, the layer is directed after all, whatever info claims.
    if(!all(sizes[keys[!loops]] == 2)){
      directed[layer] <- TRUE
      next
    }
    ties <- ties[-rows[duplicated(keys)], , drop = FALSE]
  }
  list(ties = ties, directed = directed)
}

# Identify a tie by its dyad, and by any layer or time that distinguishes it
# from another tie on the same dyad. `ordered = FALSE` ignores the direction.
.dyad_key <- function(ties, ordered = TRUE){
  from <- ties$from
  to <- ties$to
  if(!ordered){
    from <- pmin(ties$from, ties$to)
    to <- pmax(ties$from, ties$to)
  }
  # Two reports of one tie are two ties, as are two pieces of gossip
  # about different nodes, so 'by' and 'about' distinguish ties too.
  extra <- lapply(intersect(c("layer", "time", "by", "about"), names(ties)),
                  function(col) as.character(ties[[col]]))
  do.call(paste, c(list(from, to), extra, list(sep = "\r")))
}

#' @export
as_igraph.stocnet <- function(.data, twomode = FALSE) {
  # `.data$info$directed` is authoritative here, so use it rather than letting
  # graph_from_data_frame() default every stocnet to a directed graph.
  # An 'igraph' has no hyperedges, so each is held as a node of a further
  # mode that its members are tied to, and the graph is marked as holding a
  # hypergraph so that it can be told from a two-mode network and read back.
  if(is_hypergraph(.data)){
    out <- as_igraph(.hyper_to_twomode(.data), twomode = twomode)
    return(igraph::set_graph_attr(out, "hyper", TRUE))
  }
  directed <- is_directed(.data)
  # Note that the nodes are counted by their rows and not by their columns,
  # since a table of rows without columns is how a stocnet records the size of
  # a network whose nodes have no attributes, isolates included.
  if(is.null(.data$nodes) || nrow(.data$nodes) == 0){
    out <- igraph::graph_from_data_frame(as_edgelist(.data), directed = directed)
    out <- to_unlabelled(out)
  } else {
    vertices <- .data$nodes
    # A stocnet labels its nodes in 'label', but 'name' counts as a label too
    # (see `is_labelled()`), as where `mutate_nodes(name = )` named them.
    if("label" %in% names(vertices)){
      vertices <- vertices |> dplyr::mutate(name = label) |>
        dplyr::select(name, dplyr::everything(), -label)
    } else if("name" %in% names(vertices))
      vertices <- dplyr::select(vertices, name, dplyr::everything())
    # igraph records two modes in a logical 'type' attribute, which its
    # bipartite functions require, and three or more in the 'lvl' attribute
    # that `to_multilevel()` writes and `as_stocnet()` maps back to 'mode'.
    # A bare 'mode' column would survive coercion as an ordinary attribute
    # that no reader looks for, so it is translated either way.
    if(is_twomode(.data))
      vertices <- vertices |> dplyr::mutate(type = mode == unique(mode)[2]) |>
        dplyr::select(dplyr::any_of("name"), dplyr::everything(), -mode)
    else if(net_modes(.data) > 2){
      # `mode_names()` is read here rather than inside `mutate()`, where dplyr
      # masks '.data' with its own pronoun. `as_stocnet()` sorts the levels it
      # finds and reads their names back out of info, so the level of a mode is
      # its position in that same order.
      lvls <- mode_names(.data) %||% unique(vertices$mode)
      vertices <- vertices |>
        dplyr::mutate(lvl = match(mode, lvls)) |>
        dplyr::select(dplyr::any_of("name"), dplyr::everything(), -mode)
    }
    # Where only some of the nodes are labelled, as where `from_layers()` adds
    # unlabelled nodes to labelled ones, the labels cannot tell the nodes
    # apart, so the network is built by position and labelled afterwards.
    partial <- is_labelled(.data) && anyNA(vertices$name)
    if(is_labelled(.data) && !partial){
      out <- igraph::graph_from_data_frame(as_edgelist(.data),
                                           directed = directed,
                                           vertices = vertices)
    } else {
      # The nodelist is a table of nodal attributes rather than a network,
      # so it is passed to graph_from_data_frame() as vertices under
      # temporary index names; bind_node_attributes() would instead coerce
      # it as if it were an edgelist. The temporary names keep the nodes in
      # nodelist order, rather than in order of appearance in the ties, and
      # are dropped again once the attributes are attached.
      vertices <- dplyr::mutate(vertices,
                                name = as.character(seq_len(nrow(vertices))))
      vertices <- dplyr::select(vertices, name, dplyr::everything())
      labels <- if(partial) .data$nodes$label %||% .data$nodes$name
      ties <- if(partial) .reciprocate_layers(.data) else as_edgelist(.data)
      out <- igraph::graph_from_data_frame(ties, directed = directed,
                                           vertices = vertices)
      out <- to_unlabelled(out)
      if(partial) out <- igraph::set_vertex_attr(out, "name", value = labels)
    }
    
  }
  # A two-mode network holds one tie per dyad, unless it also ties within a
  # level, in which case those ties can be directed and each arc is its own
  # row. Collapsing then points both arcs of a dyad at the same pair of nodes.
  if(is_twomode(.data) && !is_directed(.data))
    out <- to_undirected(out)
  if(!is.null(as_infolist(.data)) && length(as_infolist(.data)) > 0)
    igraph::graph_attr(out) <- as_infolist(.data)
  if(!is.null(as_changelist(.data)) && length(as_changelist(.data)) > 0)
    igraph::graph_attr(out, "changes") <- as_changelist(.data)
  if(!is.null(as_globallist(.data)) && length(as_globallist(.data)) > 0)
    igraph::graph_attr(out, "globals") <- as_globallist(.data)
  # igraph has no way to mark an edge as missing, so the ties recorded as
  # missing travel as a graph attribute rather than as edges. This keeps them
  # out of every count and every tie vector, where they are not ties.
  missing <- .missing_out(.data)
  if(!is.null(missing)) igraph::graph_attr(out, "missings") <- missing else
    if("missings" %in% igraph::graph_attr_names(out))
      out <- igraph::delete_graph_attr(out, "missings")
  out
}

#' @export
as_igraph.sienadata <- function(.data, twomode = FALSE) {
  as_igraph(as_stocnet.sienadata(.data), twomode = twomode)
}

# nocov start
#' @export
as_igraph.diff_model <- function(.data,
                                 twomode = FALSE) {
  as_igraph(attr(.data, "network"))
}

#' @export
as_igraph.diffnet <- function(.data,
                              twomode = FALSE) {
  graph <- .data[, , 1:.data$meta$nper]
  static.attrs <- colnames(graph$vertex.static.attrs)
  dynamic.attrs <- colnames(graph$vertex.dyn.attrs[[1]])
  out <- vector("list", graph$meta$nper)
  names(out) <- dimnames(graph)[[3]]
  for (p in seq_along(out)) {
    tempgraph <- graph$graph[[p]]
    dimnames(tempgraph) <- with(graph$meta, list(ids, ids))
    tempgraph <- igraph::graph_from_adjacency_matrix(adjmatrix = tempgraph, 
                                                     mode = ifelse(graph$meta$undirected, "undirected", 
                                                                   "directed"), weighted = TRUE, diag = graph$meta$self)
    for (k in static.attrs) tempgraph <- igraph::set_vertex_attr(graph = tempgraph, 
                                                                 name = k, value = graph[[k]])
    for (k in dynamic.attrs) tempgraph <- igraph::set_vertex_attr(graph = tempgraph, 
                                                                  name = k, value = graph[[k]][[p]])
    tempgraph <- igraph::set_vertex_attr(graph = tempgraph, name = "toa", 
                                         value = graph$toa)
    tempgraph <- igraph::set_graph_attr(tempgraph, "name", 
                                        graph$meta$name)
    tempgraph <- igraph::set_graph_attr(tempgraph, "behavior", 
                                        graph$meta$behavior)
    out[[p]] <- tempgraph
  }
  out
}

#' @export
as_igraph.network.goldfish <- function(.data,
                                       twomode = FALSE) {
  
  # orig <- deparse(substitute(.data))
  # y <- ls(envir = .GlobalEnv)
  # envir  <- .GlobalEnv
  #
  # classesToKeep <- c("nodes.goldfish", "network.goldfish")
  # checkClasses <- function(.data, classes) vapply(classes, 
  #                                                  function(x) methods::is(.data, x), logical(1))
  # ClassFilter <- function(x) any(checkClasses(get(x), classes = classesToKeep))
  # gfobjs <- Filter(ClassFilter, y)
  # classes <- vapply(gfobjs, FUN = function(x) checkClasses(get(x), 
  #                                                          classes = classesToKeep), 
  #                   FUN.VALUE = logical(length(classesToKeep)))
  
  if(sum(.data)==0){
    out <- igraph::graph_from_data_frame(d = get(attr(.data, "events"))[,2:4],
                                         directed = attr(.data, "directed"),
                                         vertices = get(attr(.data, "nodes")))
  } else snet_abort("Non-empty starts are not yet supported by this function.")
  out
}

#' @export
as_igraph.networkDynamic <- function(.data, twomode = FALSE) {
  
  # edges
  out <- do.call(rbind, lapply(.data$mel, function(x) 
    data.frame(x$outl, x$inl, x$atl$active)))
  names(out) <- c("from","to","begin","end")
  out <- as.data.frame(out)
  
  # nodes
  nodes <- do.call(rbind, lapply(.data$val, 
                                 function(x) x[!names(x) %in% c("na","active")]))
  nodes <- as.data.frame(nodes)
  names(nodes) <- gsub("vertex.names", "name", names(nodes))
  
  out <- igraph::graph_from_data_frame(out, vertices = nodes)
  
  # changes
  changes <- do.call(rbind, lapply(seq_len(.data$val), 
                                   function(x) data.frame(node = x, 
                                                          if(is.null(.data$val[[x]]$active)) 
                                                            matrix(c(NA, NA), ncol = 2) else 
                                                              .data$val[[x]]$active
                                   )))
  names(changes) <- c("node","begin","end")
  changes <- stats::na.omit(changes)
  
  as_igraph(bind_changes(out, changes))
}

#' @export
as_igraph.siena <- function(.data, twomode = NULL) {
  edges <- orig <- .get_rem_time_periods <- .get_all_time_periods <- NULL
  ## Helper functions for as_igraph.siena
  .get_rem_time_periods <- function(g, x, name = NULL) {
    for(d in 2:dim(g)[3]){
      x <- join_ties(x, as_igraph(g[,,d]), 
                     attr_name = paste0(name, "_", "t", d))
    }
    x
  }
  .get_all_time_periods <- function(g, x, name = NULL) {
    # g is a matrix but x is igraph obj
    for(d in seq_len(dim(g)[3])){
      y <- g[,,d]
      if (isTRUE(is_twomode(y))) {
        # add names for new network
        rownames(y) <- as.character(seq_len(nrow(y)))
        colnames(y) <- as.character(paste0("N", seq_len(ncol(y))))
        # join ties
        if (isTRUE(is_twomode(x))) { # x and y are twomode
          x <- join_ties(x, as_igraph(y),
                         attr_name = paste0(name, "_", "t", d))
        } else { # x is onemode but y is twomode
          y <- as_edgelist(y)
          y <- y |>
            dplyr::mutate(weight = 1)
          x <- dplyr::bind_rows(y, as_edgelist(x)) |>
            as_igraph()
          x <- igraph::set_edge_attr(x, name = paste0(name, "_", "t", d),
                                     value = igraph::edge_attr(as_igraph(x),
                                                               "weight")) |>
            igraph::delete_edge_attr("weight")
        }
      } else {
        # add names for one-mode y
        y <- igraph::set_vertex_attr(y, name = "name",
                                 value = as.character(seq_len(igraph::vcount(as_igraph(y)))))
        # join ties
        if (isTRUE(is_twomode(x))) { # x is twomode but y is onemode
          y <- as_edgelist(y)
          y <- y |>
            dplyr::mutate(weight = 1)
          x <- dplyr::bind_rows(y, as_edgelist(x)) |>
            as_igraph()
          x <- igraph::set_edge_attr(x, name = paste0(name, "_", "t", d),
                                     value = igraph::edge_attr(as_igraph(x),
                                                               "weight")) |>
            igraph::delete_edge_attr("weight")
        } else { # x and y are onemode
          x <- join_ties(x, as_igraph(y), 
                         attr_name = paste0(name, "_", "t", d))
        }
      }
    }
    x
  }
  .get_attributes <- function(ndy, x, name = NULL) {
    for(d in seq_len(dim(ndy)[2])) {
      x <- igraph::set_vertex_attr(x, name = paste0(name, "_", "t", d),
                               value = as.vector(ndy[,d]))
    }
    x
  }
  # We always get the dependent network(s) first
  # Identify all dyadic and non-dyadic depvars
  dvs <- lapply(.data$depvars, function(x) is.matrix(x[,,1]) )
  ddvs <- names(which(dvs))
  # Add in first network as base and add names
  out <- .data$depvars[[ddvs[1]]][,,1] # first wave
  if (!is_twomode(out)) {
    out <- igraph::set_vertex_attr(out, name = "name",
                               value = as.character(seq_len(igraph::vcount(as_igraph(out)))))
  } else {
    rownames(out) <- as.character(seq_len(nrow(out)))
    colnames(out) <- as.character(paste0("N", seq_len(ncol(out))))
  }
  # Add ties from rest of time periods
  out <- .get_rem_time_periods(.data$depvars[[ddvs[1]]], out,
                               name = ddvs[1])
  out <- igraph::set_edge_attr(out, name = paste0(ddvs[1], "_", "t1"),
                               value = igraph::edge_attr(as_igraph(out),
                                                         "orig")) |>
    igraph::delete_edge_attr("orig")
  # Add rest of the dyadic depvars
  if (length(ddvs) > 1) {
    for (l in 2:length(ddvs)) {
      out <- .get_all_time_periods(.data$depvars[[ddvs[l]]], out,
                                   name = ddvs[l])
    }
  }
  # Add dycCovar
  for (k in seq_along(.data$dycCovars)) {
    out <- join_ties(out, as_igraph(.data$dycCovars[k]),
                     attr_name = paste0(names(.data$dycCovars)[k]))
  }
  # Add dyvCovars
  for (k in seq_along(.data$dyvCovars)) {
    out <- .get_all_time_periods(.data$dyvCovars[[k]], out,
                                 name = paste0(names(.data$dyvCovars)[k]))
  }
  # Add any behavioral depvars
  if(length(which(!dvs)) > 0) {
    bdvs <- names(which(!dvs))
    for (b in seq_along(bdvs)) {
      out <- .get_attributes(.data$depvars[[bdvs[b]]], out,
                             name = bdvs[b])
    }
  }
  # Add composition change
  for (k in seq_along(.data$compositionChange)) {
    out <- igraph::set_vertex_attr(out, name =  paste0(names(.data$compositionChange)[k]),
                               value = as.vector(.data$compositionChange[[k]]))
  }
  # Add cCovar
  for (k in seq_along(.data$cCovars)) {
    out <- igraph::set_vertex_attr(out, name = paste0(names(.data$cCovars)[k]),
                               value = as.vector(.data$cCovars[[k]]))
  }
  # Add vCovar
  for (k in seq_along(.data$vCovars)) {
    out <- .get_attributes(.data$vCovars[[k]], out,
                           name = paste0(names(.data$vCovars)[k]))
  }
  out
}
# nocov end

# tidygraph ####

#' @rdname coerce_graph
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_data_frame
#' @export
as_tidygraph <- function(.data, twomode = FALSE) UseMethod("as_tidygraph")

#' @export
as_tidygraph.data.frame <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(as_igraph(.data))
  out
}

#' @importFrom tidygraph tbl_graph
#' @export
as_tidygraph.list <- function(.data, twomode = FALSE) {
  if (!is.null(names(.data))){
    if ("nodes" %in% names(.data) & "ties" %in% names(.data)) {
      out <- tidygraph::tbl_graph(nodes = .data[["nodes"]],
                                  edges = .data[["ties"]])
    } else if ("nodes" %in% names(.data) & "edges" %in% names(.data)) {
      out <- tidygraph::tbl_graph(nodes = .data[["nodes"]],
                                  edges = .data[["edges"]])
    } else snet_abort("Please name the list elements 'nodes' and 'ties'.")
  } else snet_abort("Please name the list elements 'nodes' and 'ties'.")
  out
}

#' @export
as_tidygraph.matrix <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(as_igraph(.data, twomode = twomode))
  out
}

#' @export
as_tidygraph.igraph <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(.data)
  out
}

#' @export
as_tidygraph.tbl_graph <- function(.data, twomode = FALSE) {
  # A network saved by an earlier version may still carry the 'mnet' class.
  class(.data) <- setdiff(class(.data), "mnet")
  .data
}

#' @export
as_tidygraph.network <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(as_igraph(.data))
  out
}

#' @export
as_tidygraph.stocnet <- function(.data, twomode = FALSE) {
  as_tidygraph(as_igraph.stocnet(.data, twomode = twomode))
}

#' @export
as_tidygraph.sienadata <- function(.data, twomode = FALSE) {
  as_tidygraph(as_stocnet.sienadata(.data), twomode = twomode)
}

# nocov start
#' @export
as_tidygraph.network.goldfish <- function(.data,
                                          twomode = FALSE) {
  
  # orig <- deparse(substitute(.data))
  # y <- ls(envir = .GlobalEnv)
  # envir  <- .GlobalEnv
  #
  # classesToKeep <- c("nodes.goldfish", "network.goldfish")
  # checkClasses <- function(.data, classes) vapply(classes, 
  #                               function(x) methods::is(.data, x), logical(1))
  # ClassFilter <- function(x) any(checkClasses(get(x), classes = classesToKeep))
  # gfobjs <- Filter(ClassFilter, y)
  # classes <- vapply(gfobjs, FUN = function(x) checkClasses(get(x),
  #                                classes = classesToKeep),
  #                   FUN.VALUE = logical(length(classesToKeep)))
  
  if (sum(.data)==0) {
    out <- igraph::graph_from_data_frame(d = get(attr(.data, "events"))[,2:4],
                                         directed = attr(.data, "directed"),
                                         vertices = get(attr(.data, "nodes")))
    out <- as_tidygraph(out)
  } else snet_abort("Non-empty starts are not yet supported by this function.")
  
  # if(rowSums(classes)['network.goldfish']>1){
  #   nets <- colnames(classes)[classes['network.goldfish', ]==TRUE]
  #   nets <- nets[nets != orig]
  #   for(edges in nets){
  #     eventlist <- get(attr(get(edges), "events"))
  #     eventlist <- eventlist[,2:4]
  #     eventlist <- eventlist[!duplicated(eventlist),] # currently not carrying multiple ties across
  #     other <- as_tidygraph(eventlist)
  #     out <- join_edges(out, other, edges)
  #   }
  # }
  out
}

#' @export
as_tidygraph.siena <- function(.data, twomode = FALSE) {
  out <- as_tidygraph(as_igraph.siena(.data, twomode = FALSE))
  out
}

#' @export
as_tidygraph.diff_model <- function(.data, twomode = FALSE) {
  out <- as_tidygraph(attr(.data, "network"))
  attr(out, "diff_model") <- .data
  # if (!"name" %in% names(node_attribute(out))) {
  #   out <- add_node_attribute(out, "name",
  #                             as.character(seq_len(igraph::vcount(out))))
  # }
  out
}

#' @export
as_tidygraph.diffnet <- function(.data, twomode = FALSE) {
  out <- as_igraph(.data)
  lapply(out, as_tidygraph)
}

#' @export
as_tidygraph.networkDynamic <- function(.data, twomode = FALSE) {
  as_tidygraph(as_igraph.networkDynamic(.data, twomode = twomode))
}
# nocov end

# Network ####

#' @rdname coerce_graph
#' @importFrom network as.network set.vertex.attribute
#' @importFrom igraph vertex_attr
#' @export
as_network <- function(.data,
                       twomode = FALSE) UseMethod("as_network")

#' @export
as_network.network <- function(.data,
                               twomode = FALSE) {
  .data
}

#' @export
as_network.matrix <- function(.data,
                              twomode = FALSE) {
  # Convert to adjacency matrix if not square already
  if (is_twomode(.data)) {
    out <- to_multilevel(.data)
  } else out <- .data
  valued <- is_weighted(.data) | is_signed(.data) | anyNA(.data)
  network::as.network(out,
                      directed = is_directed(.data),
                      bipartite   = ifelse(is_twomode(.data),
                                           nrow(.data),
                                           FALSE),
                      loops = ifelse(sum(diag(out), na.rm = TRUE) > 0, TRUE, FALSE),
                      # a signed matrix's values are carried as weights too,
                      # so that the signs survive the coercion, as are the
                      # missing values of a matrix holding ties recorded as missing
                      ignore.eval = ifelse(valued, FALSE, TRUE),
                      names.eval  = ifelse(valued, "weight", NULL))
}

# A sociomatrix holds one value for each dyad, so it cannot carry the
# direction of a two-mode network, nor the reporter or target of each tie.
# Those networks are built from their ties instead, as a stocnet is.
.network_via_ties <- function(.data){
  (is_twomode(.data) && igraph::is_directed(.data)) ||
    any(c("by", "about") %in% igraph::edge_attr_names(.data))
}

#' @export
as_network.igraph <- function(.data,
                              twomode = FALSE) {
  if(.network_via_ties(.data)) return(as_network.stocnet(as_stocnet(.data)))
  name <- type <- NULL
  attr <- as.data.frame(igraph::vertex_attr(.data))
  if ("name" %in% colnames(attr)) attr <- subset(attr, select = c(-name))
  if ("type" %in% colnames(attr)) attr <- subset(attr, select = c(-type))
  out <- as_network(as_matrix(.data))
  out <- set_network_node_attributes(out, attr)
  out <- set_network_tie_attributes(out, .data)
  out <- set_network_info(out, igraph::graph_attr(as_igraph(.data)))
  out
}

#' @export
as_network.tbl_graph <- function(.data,
                                 twomode = FALSE) {
  if(.network_via_ties(.data)) return(as_network.stocnet(as_stocnet(.data)))
  nodes <- name <- type <- NULL
  attr <- as.data.frame(activate(.data, nodes))[-1]
  if ("name" %in% colnames(attr)) attr <- subset(attr, select = c(-name))
  if ("type" %in% colnames(attr)) attr <- subset(attr, select = c(-type))
  out <- as_network(as_matrix(.data))
  out <- set_network_node_attributes(out, attr)
  out <- set_network_tie_attributes(out, as_igraph(.data))
  out <- set_network_info(out, igraph::graph_attr(as_igraph(.data)))
  out
}

# `network::set.vertex.attribute()` interprets a single-column data frame as
# one value (the whole column) to be assigned to every vertex,
# so attributes are set one at a time.
set_network_node_attributes <- function(out, attr) {
  for (a in names(attr))
    out <- network::set.vertex.attribute(out, a, attr[[a]])
  out
}

# Since networks are constructed from a sociomatrix, which only carries the
# weight, any other tie attributes are copied across dyad by dyad.
# What a network knows about itself as a whole, carried onto a 'network'
# object. 'network' requires its own attributes of these names to be single
# values describing the whole network, so an info entry of the same name is
# dropped rather than merged: a per-layer 'directed' vector, for example, would
# otherwise replace the scalar and break every 'network' method that reads it.
# That vector is carried in an attribute of its own instead, which
# `as_stocnet.network()` reads back as 'directed'. Without it the round trip
# could not tell an undirected layer from a directed one whose ties happen to
# be reciprocated.
set_network_info <- function(out, info) {
  if (is.null(info) || length(info) == 0) return(out)
  if (!is.null(info$directed) && !is.null(names(info$directed)))
    out <- network::set.network.attribute(out, "layer_directed", info$directed)
  info <- info[setdiff(names(info), c("n", "mnext", "directed", "hyper",
                                      "loops", "multiple", "bipartite"))]
  if (length(info) > 0) out$gal <- utils::modifyList(out$gal, info)
  out
}

# The id of the one tie of `out` between the nodes of each row of `el`, or NA
# where there is none or more than one, as `network::get.edgeIDs()` would find
# it: leading from the first node to the second in a directed network, either
# way in an undirected one, and passing over ties recorded as missing. Every
# row is matched at once against the ties' endpoints.
.network_edge_ids <- function(out, el) {
  directed <- network::is.directed(out)
  held <- which(!vapply(out$mel, is.null, logical(1)))
  ties <- out$mel[held]
  tail <- vapply(ties, function(e) as.numeric(e$outl[1]), numeric(1))
  head <- vapply(ties, function(e) as.numeric(e$inl[1]), numeric(1))
  observed <- !vapply(ties, function(e) isTRUE(e$atl$na), logical(1))
  key <- function(a, b) if (directed) paste(a, b) else
    paste(pmin(a, b), pmax(a, b))
  have <- key(tail, head)[observed]
  held <- held[observed]
  ids <- held[match(key(el[, 1], el[, 2]), have)]
  ids[key(el[, 1], el[, 2]) %in% have[duplicated(have)]] <- NA_integer_
  as.integer(ids)
}

set_network_tie_attributes <- function(out, .data) {
  attrs <- setdiff(igraph::edge_attr_names(.data), c("weight", "na"))
  if (length(attrs) == 0) return(out)
  el <- igraph::as_edgelist(.data, names = FALSE)
  eids <- .network_edge_ids(out, el)
  for (a in attrs) {
    vals <- igraph::edge_attr(.data, a)
    keep <- !is.na(eids)
    # A list is passed since `network` rejects classed vectors (e.g. dates).
    if (any(keep))
      out <- network::set.edge.attribute(out, a, as.list(vals[keep]),
                                         e = eids[keep])
  }
  out
}

#' @export
as_network.data.frame <- function(.data,
                                  twomode = NULL) {
  if (inherits(.data, "tbl_df")) .data <- as.data.frame(.data)
  network::as.network.data.frame(.data,
                                 directed = ifelse(is_twomode(.data),
                                                   FALSE,
                                                   is_directed(.data)),
                                 bipartite = is_twomode(.data)
  )
}

# A 'network' object holds a hyperedge as an edge with several nodes at its
# tail and at its head. Where the hyperedge has no sides, its members are at
# both, which is how `{network}` reads an undirected hyperedge itself.
.hyper_to_network <- function(.data){
  ties <- .data$ties
  nodes <- .data$nodes
  hyper <- .hyper_rows(ties)
  sided <- .hyper_directed(.data)
  tails <- as.list(ties$from)
  heads <- as.list(ties$to)
  for(i in which(hyper)){
    members <- ties$with[[i]]
    if(sided){
      # the 'from' side is listed first, as the tail is read back before the
      # head, and whatever each member holds is kept in step with it
      side <- order(ties$with_direction[[i]] != "from")
      for(col in .with_cols(ties)) ties[[col]][[i]] <- ties[[col]][[i]][side]
      members <- ties$with[[i]] <- members[side]
      tails[[i]] <- members[ties$with_direction[[i]] == "from"]
      heads[[i]] <- members[ties$with_direction[[i]] == "to"]
    } else tails[[i]] <- heads[[i]] <- members
  }
  # `{network}` refuses an edge with nothing at its tail or its head, so a
  # hyperedge with no members, or with none on one side, is carried beside
  # the edges and put back among them on the way back.
  held <- lengths(tails) == 0 | lengths(heads) == 0
  out <- network::network.initialize(net_nodes(.data),
                                     directed = is_directed(.data),
                                     hyper = TRUE, loops = TRUE,
                                     multiple = TRUE)
  if(any(!held)){
    out <- network::add.edges(out, tail = tails[!held], head = heads[!held])
    cols <- setdiff(names(ties), c("from", "to", "with",
                                   if(sided) "with_direction"))
    for(col in cols)
      out <- network::set.edge.attribute(out, col, as.list(ties[[col]][!held]))
  }
  for(col in names(nodes)){
    out <- network::set.vertex.attribute(
      out, if(col == "label") "vertex.names" else col,
      if(col == "label") as.character(nodes[[col]]) else nodes[[col]])
  }
  out <- set_network_info(out, as_infolist(.data))
  if(any(held)){
    snet_warn("{sum(held)} hyperedge{?s} {?has/have} no members, or none on",
              "one side, which a {.cls network} object cannot hold as an edge.",
              "{?It is/They are} kept as a network attribute instead.")
    out <- network::set.network.attribute(
      out, "hyperedges_held", cbind(.row = which(held), ties[held, ]))
  }
  if(!all(hyper))
    out <- network::set.network.attribute(out, "layer_hyper",
                                          unique(ties$layer[hyper]))
  if(!is.null(as_changelist(.data)) && length(as_changelist(.data)) > 0)
    out <- network::set.network.attribute(out, "changes", as_changelist(.data))
  if(!is.null(as_globallist(.data)) && length(as_globallist(.data)) > 0)
    out <- network::set.network.attribute(out, "globals", as_globallist(.data))
  out
}

# The reverse: the edges of a 'network' object that holds hyperedges, as ties.
.network_to_hyper <- function(.data){
  edges <- Filter(Negate(is.null), .data$mel)
  in_layer <- .data$gal$layer_hyper
  layers <- vapply(edges, function(e) as.character(e$atl$layer %||% NA),
                   character(1))
  hyper <- if(is.null(in_layer)) rep(TRUE, length(edges)) else
    layers %in% in_layer
  # the hyperedges have sides where the layers they are in are directed
  sided <- .data$gal$layer_directed
  sided <- if(is.null(sided)) is_directed(.data) else
    isTRUE(any(sided[unique(layers[hyper])]))
  ties <- dplyr::tibble(from = rep(NA_integer_, length(edges)),
                        to = NA_integer_,
                        with = vector("list", length(edges)))
  sides <- vector("list", length(edges))
  for(i in seq_along(edges)){
    tail <- as.integer(edges[[i]]$outl)
    head <- as.integer(edges[[i]]$inl)
    if(!hyper[i]){
      ties$from[i] <- tail[1]
      ties$to[i] <- head[1]
    } else if(sided){
      ties$with[[i]] <- c(tail, head)
      sides[[i]] <- rep(c("from", "to"), c(length(tail), length(head)))
    } else ties$with[[i]] <- tail
  }
  attrs <- setdiff(unique(unlist(lapply(edges, function(e) names(e$atl)))),
                   "na")
  for(col in attrs){
    values <- lapply(edges, function(e) e$atl[[col]])
    # what each member holds stays a list, and anything else is one value
    if(!grepl("^with_", col)){
      values[lengths(values) == 0] <- NA
      if(all(lengths(values) == 1)) values <- do.call(c, values)
    }
    ties[[col]] <- values
  }
  if(sided && any(hyper)) ties$with_direction <- sides
  held <- .data$gal$hyperedges_held
  if(!is.null(held)){
    rows <- held$.row
    held <- dplyr::as_tibble(held[setdiff(names(held), ".row")])
    ties <- dplyr::bind_rows(ties, held)
    ties <- ties[order(c(setdiff(seq_len(nrow(ties)), rows), rows)), ]
    ties <- ties[names(held)]
  }
  ties
}

#' @export
as_network.stocnet <- function(.data, twomode = FALSE) {
  # 'network' is the one other class with hyperedges of its own
  if(is_hypergraph(.data)) return(.hyper_to_network(.data))
  # 'network' counts the first 'bipartite' nodes as the first mode, but a
  # stocnet need not list its modes in that order: `add_nodes()` puts a node
  # of the first mode after the second, for example. So the nodes are sorted
  # by mode first, which renumbers their ties, missing ties, and changes too.
  second <- if (is_twomode(.data)) as.logical(node_is_mode(.data)) else NULL
  if (is.unsorted(second)) .data <- arrange_nodes(.data, !!second)
  # Networks are constructed directly from the node and tie tables so that
  # multiple edges (multiplex/multi-wave) and their tie attributes (e.g. layer,
  # time, weight) are retained rather than collapsed into a sociomatrix.
  nodes <- .data$nodes
  # As for igraph, a layer held once per dyad is reciprocated here, since a
  # 'network' object is directed or undirected as a whole.
  ties <- .reciprocate_layers(.data)
  # A 'network' object is the one class that can mark an edge as missing, in
  # its reserved 'na' attribute, which is the format `{ergm}` and the rest of
  # statnet expect. So the ties recorded as missing are added as edges here,
  # marked as such, and `network`'s own functions omit them as they should.
  missing <- as_missinglist(.data)
  if(!is.null(missing) && nrow(missing)){
    missing <- missing[intersect(names(ties), names(missing))]
    missing$na <- TRUE
    ties <- dplyr::bind_rows(dplyr::mutate(ties, na = FALSE), missing)
  }
  # For two-mode networks the 'bipartite' count is the number of first-mode
  # nodes, which the sorting above has put first, with ties running from the
  # first mode ('from') to the second ('to'), matching network's convention.
  bipartite <- FALSE
  skip_cols <- "na"
  if (is_twomode(.data)) {
    bipartite <- sum(!second)
    # 'mode' is implied by the bipartite structure, so it is not stored as a
    # vertex attribute (it is reconstructed on the way back).
    skip_cols <- c(skip_cols, "mode")
  }
  # Multiple edges are needed for multiplex networks and for any repeated dyad
  # (e.g. the same tie observed across several waves).
  has_multiedges <- is_multiplex(.data) ||
    (!is.null(ties) && nrow(ties) > 0 &&
       anyDuplicated(ties[c("from", "to")]) > 0)
  out <- network::network.initialize(net_nodes(.data),
                                     directed = is_directed(.data),
                                     bipartite = bipartite,
                                     loops = !is.null(ties) &&
                                       any(ties$from == ties$to),
                                     multiple = has_multiedges)
  if (!is.null(ties) && nrow(ties) > 0) {
    out <- network::add.edges(out, tail = as.integer(ties$from),
                              head = as.integer(ties$to))
    for (col in setdiff(names(ties), c("from", "to"))) {
      out <- network::set.edge.attribute(out, col, as.vector(ties[[col]]))
    }
  }
  if (!is.null(nodes)) {
    for (col in setdiff(names(nodes), skip_cols)) {
      if (col == "label") {
        out <- network::set.vertex.attribute(out, "vertex.names",
                                              as.character(nodes$label))
      } else {
        out <- network::set.vertex.attribute(out, col, nodes[[col]])
      }
    }
  }
  # Merge the info list into (rather than replace) the network attribute list,
  # so that core network attributes such as n, directed, and bipartite survive.
  # 'network' requires these to be single values that describe the whole
  # network, so an info entry of the same name is dropped rather than merged.
  # A per-layer 'directed' vector, for example, would otherwise replace the
  # scalar set above and break every 'network' method that reads it.
  out <- set_network_info(out, as_infolist(.data))
  if(!is.null(as_changelist(.data)) && length(as_changelist(.data)) > 0)
    out <- network::set.network.attribute(out, "changes", as_changelist(.data))
  if(!is.null(as_globallist(.data)) && length(as_globallist(.data)) > 0)
    out <- network::set.network.attribute(out, "globals", as_globallist(.data))
  out
}

#' @export
as_network.sienadata <- function(.data, twomode = FALSE) {
  as_network(as_stocnet.sienadata(.data))
}

# nocov start
#' @export
as_network.network.goldfish <- function(.data,
                                        twomode = FALSE) {
  as_network(as_igraph(.data, twomode = twomode))
}

#' @export
as_network.diffnet <- function(.data,
                               twomode = FALSE) {
  graph <- .data[, , 1:.data$meta$nper]
  n <- graph$meta$n
  structure(Map(function(g, a, time) {
    dimnames(g) <- list(rownames(graph), rownames(graph))
    ans <- network::network(x = as.matrix(g), 
                            vertex.attr = c(list(toa = graph$toa),
                                            unclass(a), unclass(graph$vertex.static.attrs)), 
                            loops = graph$meta$self)
    network::set.network.attribute(ans, "name", graph$meta$name)
    network::set.network.attribute(ans, "behavior", graph$meta$behavior)
    ans
  }, g = graph$graph, a = graph$vertex.dyn.attrs), names = dimnames(graph)[[3]])
}

#' @export
as_network.siena <- function(.data, twomode = FALSE) {
  as_network(as_igraph.siena(.data, twomode = FALSE))
}

#' @export
as_network.networkDynamic <- function(.data, twomode = FALSE) {
  out <- .data
  class(out) <- setdiff(class(out), "networkDynamic")
  out
}
# nocov end

# hypergraph ####

#' @rdname coerce_graph
#' @section Hypergraphs:
#'   `as_hypergraph()` returns a 'hypergraph' object of the `{HyperG}` package,
#'   which is a sparse incidence matrix of hyperedges by nodes,
#'   and `as_stocnet()` reads one.
#'   A network that is not a hypergraph is reformatted as one first,
#'   as by [to_hypergraph()].
#'   A 'hypergraph' object holds which nodes each hyperedge has and their
#'   names, and nothing else, so weights, attributes, directions,
#'   and times are not carried.
#'   Neither function needs `{HyperG}` to be installed.
#'
#'   Note that [is_hypergraph()] marks a network of any class that holds
#'   hyperedges, of which a 'stocnet' object holds the most
#'   (see the Hypergraphs section of [make_stocnet()]).
#' @examples
#' as_hypergraph(create_ring(4))
#' @export
as_hypergraph <- function(.data) UseMethod("as_hypergraph")

#' @export
as_hypergraph.default <- function(.data) {
  net <- .dyads_to_hyper(to_hypergraph(as_stocnet(.data)))
  ties <- net$ties
  # A 'hypergraph' object holds which nodes each hyperedge has, and no more.
  lost <- c(if(.hyper_directed(net)) "directions",
            if(is_weighted(net) || "with_weight" %in% names(ties)) "weights",
            if(length(setdiff(names(ties), c("from", "to", "with", "label",
                                             "weight", "with_weight",
                                             "with_direction"))) ||
               length(setdiff(names(net$nodes), "label"))) "attributes")
  if(length(lost))
    snet_warn("A {.cls hypergraph} object of {.pkg HyperG} holds no {lost},",
              "so those of this network were not carried.")
  n <- net_nodes(net)
  m <- nrow(ties)
  # The memberships alone, as the ties of hyperedges to nodes. The hyperedges
  # are the first mode, so that they are the rows of the matrix.
  members <- lapply(ties$with, unique)
  graph <- igraph::make_bipartite_graph(
    rep(c(FALSE, TRUE), c(m, n)),
    rbind(rep(seq_len(m), lengths(members)), m + unlist(members)))
  M <- igraph::as_biadjacency_matrix(graph, sparse = TRUE)
  # `{HyperG}` makes its matrix column-compressed, where igraph returns triplets
  M <- tryCatch(methods::as(M, "CsparseMatrix"), error = function(e) M)
  dimnames(M) <- list(ties[["label"]],
                      net$nodes[["label"]] %||% as.character(seq_len(n)))
  structure(list(M = M), class = "hypergraph")
}

#' @export
as_hypergraph.hypergraph <- function(.data) .data


# graphAM ####

#' @rdname coerce_graph
#' @export
as_graphAM <- function(.data, twomode = NULL) UseMethod("as_graphAM")

setClass("attrData", representation(data="list",
                                    defaults="list"))

setClass("renderInfo", 
         representation(nodes="list", # information on nodes
                        edges="list", # information on edges
                        graph="list",
                        pars="list")) # passed on to graph.par before rendering

setClass("graphBase")

setClass("graph", representation(## edgemode="character",
  edgeData="attrData",
  nodeData="attrData",
  renderInfo="renderInfo",
  ## nodeInfo="list",
  ## edgeInfo="list",
  graphData="list",
  "VIRTUAL"),
  contains = "graphBase")

setClass("graphAM", contains="graph",
         slots = c(adjMat="matrix", edgemode="character"))

#' @export
as_graphAM.default <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.matrix <- function(.data, twomode = NULL) {
  thisRequires("methods")
  methods::new("graphAM", adjMat = to_onemode(.data), 
               edgemode = ifelse(is_directed(.data), "directed", "undirected"))
}

# nocov start
# Diffnet ####

#' @rdname coerce_graph
#' @export
as_diffnet <- function(.data,
                       twomode = FALSE) UseMethod("as_diffnet")

#' @export
as_diffnet.diff_model <- function(.data,
                                  twomode = FALSE) {
  out <- summary(.data) |> dplyr::filter(event == "I") |> 
    dplyr::distinct(nodes, .keep_all = TRUE) |> 
    dplyr::select(nodes,t)
  if(!is_labelled(as_igraph(.data)))
    out <- dplyr::arrange(out, nodes) else if (is.numeric(out$nodes))
      out$nodes <- node_labels(as_igraph(.data))[out$nodes]
    toa <- stats::setNames(out$t, out$nodes)
    if(is_dynamic(.data)){
      snet_unavailable("Coercing a dynamic network to a diffnet is not yet",
                       "available.")
      # netdiffuseR::igraph_to_diffnet(graph.list = to_waves(.data))
    } else {
      graph <- as_tidygraph(.data) |> mutate(toa = as.numeric(toa)) |> as_igraph()
      # suppressWarnings(netdiffuseR::igraph_to_diffnet(graph = graph,
      #                               toavar = "toa"))
      structure(list(graph = graph, toa = toa#, 
                     # adopt = adopt, 
                     # cumadopt = cumadopt, vertex.static.attrs = vertex.static.attrs, 
                     # vertex.dyn.attrs = vertex.dyn.attrs, graph.attrs = graph.attrs, 
                     # meta = meta
      ), class = "diffnet")
    }
    
}
# nocov end

