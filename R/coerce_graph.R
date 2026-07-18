#' Coercing into graph objects
#' @name coerce_graph
#' @description
#'   The `as_` functions in `{manynet}` coerce objects of any of the following common classes
#'   of social network objects in R into the declared class:
#'   - `as_igraph()` coerces the object into an `{igraph}` `graph` object.
#'   - `as_tidygraph()` coerces the object into a `{tidygraph}` `tbl_graph` object.
#'   - `as_network()` coerces the object into a `{network}` `network` object.
#'   - `as_siena()` coerces the (igraph/tidygraph) object into a SIENA dependent variable.
#'   - `as_graphAM()` coerces the object into a graph adjacency matrix.
#'   - `as_diffusion()` coerces a table of diffusion events into
#'   a `diff_model` object similar to the output of `play_diffusion()`.
#'   - `as_diffnet()` coerces a `diff_model` object into a `{netdiffuseR}` `diffnet` object.
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
#'   available_methods(collect_functions("as.*(graph|net|diff|siena|goldfish)"))
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
  if (is_weighted(.data) & !("weight" %in% names(.data))) {
    if(!names(.data)[3] %in% c("begin","sign","date"))
      names(.data)[3] <- "weight"
    # snet_abort("Please rename the weight column of your dataframe to 'weight'")
  }
  if (!is_labelled(.data)) {
    graph <- igraph::graph_from_data_frame(.data,
                                           vertices = data.frame(name = 1:max(c(.data$from, .data$to))))
  } else graph <- igraph::graph_from_data_frame(.data)
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
  if (nrow(.data) != ncol(.data) | twomode) {
    if (!(all(.data %in% c(0, 1)))) {
      graph <- igraph::graph_from_biadjacency_matrix(.data,
                                                     weighted = TRUE,
                                                     directed = FALSE)
    } else {
      graph <- igraph::graph_from_biadjacency_matrix(.data,
                                                     directed = FALSE)
    }
  } else {
    if (!(all(.data %in% c(0, 1)))) {
      graph <- igraph::graph_from_adjacency_matrix(.data, 
                                                   mode = ifelse(all(.data == t(.data)),
                                                                 "max", "directed"),
                                                   weighted = TRUE)
    } else {
      graph <- igraph::graph_from_adjacency_matrix(.data,
                                                   mode = ifelse(all(.data == t(.data)),
                                                                 "max", "directed"))
    }
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
  # Add remaining node level attributes
  if (length(attr) > 2) {
    for (a in attr[2:length(attr)]) {
      graph <- igraph::set_vertex_attr(graph, name = a,
                                       value = sapply(.data[[3]], "[[", a))
    }
  }
  # because network can have vertex names that are integers (i.e. just node IDs), 
  # we remove them since they are really anonymous.
  if(is.integer(network::network.vertex.names(.data))) 
    graph <- igraph::delete_vertex_attr(graph, "name")
  graph
}

#' @export
as_igraph.stocnet <- function(.data, twomode = FALSE) {
  if(is.null(as_nodelist(.data)) || length(as_nodelist(.data)) == 0){
    out <- igraph::graph_from_data_frame(as_edgelist(.data))
    out <- to_unlabelled(out)
  } else {
    vertices <- as_nodelist(.data)
    if(is_labelled(.data))
      vertices <- vertices |> dplyr::mutate(name = label) |>
        dplyr::select(name, dplyr::everything(), -label)
    if(is_twomode(.data))
      vertices <- vertices |> dplyr::mutate(type = mode == unique(mode)[2]) |>
        dplyr::select(dplyr::any_of("name"), dplyr::everything(), -mode)
    if(is_labelled(.data)){
      out <- igraph::graph_from_data_frame(as_edgelist(.data), 
                                           vertices = vertices)
    } else {
      out <- igraph::graph_from_data_frame(as_edgelist(.data)) |>
        bind_node_attributes(vertices)
    }
    
  }
  if(is_twomode(.data))
    out <- to_undirected(out)
  if(!is.null(as_infolist(.data)) && length(as_infolist(.data)) > 0)
    igraph::graph_attr(out) <- as_infolist(.data)
  if(!is.null(as_changelist(.data)) && length(as_changelist(.data)) > 0)
    igraph::graph_attr(out, "changes") <- as_changelist(.data)
  if(!is.null(as_globallist(.data)) && length(as_globallist(.data)) > 0)
    igraph::graph_attr(out, "global") <- as_globallist(.data)
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
  make_mnet(out)
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
  make_mnet(out)
}

#' @export
as_tidygraph.matrix <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(as_igraph(.data, twomode = twomode))
  make_mnet(out)
}

#' @export
as_tidygraph.igraph <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(.data)
  make_mnet(out)
}

#' @export
as_tidygraph.tbl_graph <- function(.data, twomode = FALSE) {
  out <- .data
  make_mnet(out)
}

#' @export
as_tidygraph.network <- function(.data, twomode = FALSE) {
  out <- tidygraph::as_tbl_graph(as_igraph(.data))
  make_mnet(out)
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
  make_mnet(out)
}

#' @export
as_tidygraph.siena <- function(.data, twomode = FALSE) {
  out <- as_tidygraph(as_igraph.siena(.data, twomode = FALSE))
  make_mnet(out)
}

#' @export
as_tidygraph.diff_model <- function(.data, twomode = FALSE) {
  out <- as_tidygraph(attr(.data, "network"))
  attr(out, "diff_model") <- .data
  # if (!"name" %in% names(node_attribute(out))) {
  #   out <- add_node_attribute(out, "name",
  #                             as.character(seq_len(igraph::vcount(out))))
  # }
  make_mnet(out)
}

#' @export
as_tidygraph.diffnet <- function(.data, twomode = FALSE) {
  out <- as_igraph(.data)
  lapply(out, as_tidygraph)
}

make_mnet <- function(out){
  class(out) <- unique(c("mnet", class(out)))
  out
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
  network::as.network(out,
                      directed = is_directed(.data),
                      bipartite   = ifelse(is_twomode(.data),
                                           nrow(.data),
                                           FALSE),
                      loops = ifelse(sum(diag(out)) > 0, TRUE, FALSE),
                      ignore.eval = ifelse(is_weighted(.data),
                                           FALSE, TRUE),
                      names.eval  = ifelse(is_weighted(.data),
                                           "weight", NULL))
}

#' @export
as_network.igraph <- function(.data,
                              twomode = FALSE) {
  name <- type <- NULL
  attr <- as.data.frame(igraph::vertex_attr(.data))
  if ("name" %in% colnames(attr)) attr <- subset(attr, select = c(-name))
  if ("type" %in% colnames(attr)) attr <- subset(attr, select = c(-type))
  out <- as_network(as_matrix(.data))
  if (length(attr) > 0) {
    out <- network::set.vertex.attribute(out, names(attr), attr)
  }
  out
}

#' @export
as_network.tbl_graph <- function(.data,
                                 twomode = FALSE) {
  nodes <- name <- type <- NULL
  attr <- as.data.frame(activate(.data, nodes))[-1]
  if ("name" %in% colnames(attr)) attr <- subset(attr, select = c(-name))
  if ("type" %in% colnames(attr)) attr <- subset(attr, select = c(-type))
  out <- as_network(as_matrix(.data))
  if (length(attr) > 0) {
    out <- network::set.vertex.attribute(out, names(attr), attr)
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

#' @export
as_network.stocnet <- function(.data, twomode = FALSE) {
  # Networks are constructed directly from the node and tie tables so that
  # multiple edges (multiplex/multi-wave) and their tie attributes (e.g. layer,
  # time, weight) are retained rather than collapsed into a sociomatrix.
  nodes <- .data$nodes
  ties <- .data$ties
  # For two-mode networks the 'bipartite' count is the number of first-mode
  # nodes. manynet orders nodes first-mode-first, with ties running from the
  # first mode ('from') to the second ('to'), matching network's convention.
  bipartite <- FALSE
  skip_cols <- "na"
  if (is_twomode(.data)) {
    bipartite <- sum(nodes$mode == unique(nodes$mode)[1])
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
  info <- as_infolist(.data)
  if (!is.null(info) && length(info) > 0)
    out$gal <- utils::modifyList(out$gal, info)
  if(!is.null(as_changelist(.data)) && length(as_changelist(.data)) > 0)
    out <- network::set.network.attribute(out, "changes", as_changelist(.data))
  if(!is.null(as_globallist(.data)) && length(as_globallist(.data)) > 0)
    out <- network::set.network.attribute(out, "global", as_globallist(.data))
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

# stocnet ####

#' @rdname coerce_graph
#' @export
as_stocnet <- function(.data,
                    twomode = FALSE) UseMethod("as_stocnet")

#' @export
as_stocnet.stocnet <- function(.data, twomode = FALSE) {
  .data
}

#' @export
as_stocnet.data.frame <- function(.data, twomode = FALSE) {
  out <- .data
  # make sure that the data frame has the right columns, rename them if necessary,
  # and then reorder them if necessary
   if (!all(c("from", "to") %in% colnames(out))) {
     if (all(c("source", "target") %in% colnames(out))) {
       snet_minor_info("Renaming 'source' and 'target' columns to 'from' and 'to'.")
       out <- out |> dplyr::rename(from = source, to = target)
     } else if (all(c("sender", "receiver") %in% colnames(out))) {
       snet_minor_info("Renaming 'sender' and 'receiver' columns to 'from' and 'to'.")
       out <- out |> dplyr::rename(from = sender, to = receiver)
     } else if (all(c("ego", "alter") %in% colnames(out))) {
       snet_minor_info("Renaming 'ego' and 'alter' columns to 'from' and 'to'.")
       out <- out |> dplyr::rename(from = ego, to = alter)
     } else snet_abort("Edgelist must have columns named 'from' and 'to'.")
   }
  if (!"weight" %in% colnames(out)) {
    if ("replace" %in% colnames(out)) {
       snet_minor_info("Renaming 'replace' column to 'weight'.")
      out <- out |> dplyr::rename(weight = replace)
    } else if ("increment" %in% colnames(out)) {
      snet_minor_info("Renaming 'increment' column to 'weight'.")
      out <- out |> dplyr::rename(weight = increment)
    } else if ("value" %in% colnames(out)) {
       snet_minor_info("Renaming 'value' column to 'weight'.")
      out <- out |> dplyr::rename(weight = value)
    }
  }
  out <- out |> dplyr::select(from, to, dplyr::everything())
  if(!is.numeric(out$from) || !is.numeric(out$to)){
   nodes <- unique(c(out$from, out$to))
   out <- out |> dplyr::mutate(from = match(from, nodes),
                              to = match(to, nodes))
   out <- make_stocnet(ties = out, nodes = data.frame(label = nodes))
  } else out <- make_stocnet(ties = out)
  if("increment" %in% colnames(.data)) out <- out |> 
    mutate_info(update = "increment")
  if("replace" %in% colnames(.data)) out <- out |> 
    mutate_info(update = "replace")
  out
}

#' @export
as_stocnet.igraph <- function(.data, twomode = FALSE) {
  info <- as_infolist(.data)
  nodes <- as_nodelist(.data)
  changes <- as_changelist(.data)
  ties <- as_edgelist(.data)
  global <- as_globallist(.data)
  
  if(is_labelled(.data)){
    ties$from <- match(ties$from, nodes$name)
    ties$to <- match(ties$to, nodes$name)
    nodes$label <- nodes$name
    nodes$name <- NULL
  } else {
    ties$from <- as.integer(ties$from)
    ties$to <- as.integer(ties$to)
  }
  if(is_twomode(.data)){
    if(!is.null(info$nodes) && length(info$nodes) == 2){
      nodes$mode <- info$nodes[(nodes$type*1+1)]
    } else {
      nodes$mode <- as.character(nodes$type)
    }
    nodes$type <- NULL
  }
  # is_multiplex() is also TRUE for networks with any non-reserved tie
  # attribute, which have no 'type' column to rename to 'layer'
  if(is_multiplex(.data) && "type" %in% names(ties)){
    ties$layer <- ties$type
    ties$type <- NULL
    if(is.null(info$ties)){
      info$ties <- unique(ties$layer)
    }
  }
  if(!is.null(info$changes)) info$changes <- NULL
  info$directed <- is_directed(.data)
  
  if(!is.null(nodes))
    nodes <- dplyr::select(nodes, 
                           dplyr::any_of(c("label", "mode")), 
                           dplyr::everything())
  
  out <- list(info = info, nodes = nodes, ties = ties, 
              changes = changes, global = global)
  class(out) <- c("stocnet", class(out))
  out <- rename_nodes(out)
  out <- rename_ties(out)
  out
}

#' @export
as_stocnet.matrix <- function(.data,
                           twomode = FALSE) {
 as_stocnet(as_tidygraph(.data, twomode = twomode)) 
}
  
#' @export
as_stocnet.network <- function(.data,
                           twomode = FALSE) {
  # Read edges together with all their attributes in a single, aligned pass so
  # that tie attributes (e.g. layer, time, weight) stay matched to their edges.
  edf <- network::as.data.frame.network(.data, unit = "edges", na.rm = FALSE)
  if (nrow(edf) == 0) {
    ties <- dplyr::tibble(from = integer(0), to = integer(0))
  } else {
    # `.tail`/`.head` are reported as vertex names; convert them back to the
    # integer node indices that stocnet ties tables use.
    vnames <- as.character(network::network.vertex.names(.data))
    edf$from <- match(as.character(edf$.tail), vnames)
    edf$to <- match(as.character(edf$.head), vnames)
    edf[c(".tail", ".head", "na")] <- NULL
    # Drop the weight column when all weights are unity (an unweighted network).
    if ("weight" %in% names(edf) && all(edf$weight == 1)) edf[["weight"]] <- NULL
    ties <- dplyr::as_tibble(edf) |>
      dplyr::select("from", "to", dplyr::everything())
  }
  out <- list(info = list(directed = is_directed(.data)),
              nodes = as_nodelist(.data),
              changes = as_changelist(.data),
              ties = ties)
  class(out) <- c("stocnet", class(out))
  # Integer vertex names are anonymous node ids, not real labels, so drop them.
  if(!is.null(out$nodes) &&
     inherits(network::network.vertex.names(.data), "integer")){
    out$nodes$label <- NULL
    if(ncol(out$nodes) == 0) out$nodes <- NULL
  }
  out
}

# RSiena ####

#' @rdname coerce_graph
#' @export
as_siena <- function(.data,
                     twomode = FALSE) UseMethod("as_siena")

#' @export
as_siena.default <- function(.data, twomode = FALSE) {
  # Any object that can be coerced to a 'stocnet' (igraph, tidygraph/mnet,
  # matrix, network, edgelist, ...) reaches SIENA through that richer path,
  # so that multiplex layers, waves, covariates, etc. are carried across.
  as_siena(as_stocnet(.data, twomode = twomode), twomode = twomode)
}

#' @export
as_siena.stocnet <- function(.data, twomode = FALSE) {
  thisRequires("RSiena")
  x <- .data
  # The longitudinal column may arrive as 'wave' (mnet convention) or 'time'.
  x$ties <- .siena_wave_to_time(x$ties)
  x$changes <- .siena_wave_to_time(x$changes)
  info <- x$info %||% list()
  smeta <- info$siena %||% list()
  svars <- smeta$vars %||% list()
  focal <- info$focal %||% character(0)
  centered <- info$centered %||% logical(0)
  nodes <- x$nodes %||% dplyr::tibble(label = character(0))
  # Node sets (modes) ----
  if ("mode" %in% names(nodes)) {
    set_names <- unique(as.character(nodes[["mode"]]))
    sizes <- vapply(set_names, function(m) sum(nodes[["mode"]] == m), integer(1))
  } else {
    set_names <- (info$modes %||% "Actors")[1]
    sizes <- stats::setNames(nrow(nodes), set_names)
  }
  offsets <- stats::setNames(cumsum(c(0, sizes))[seq_along(sizes)], set_names)
  labelled <- isTRUE(smeta$labelled)
  labs_of <- function(m) if (labelled)
    as.character(nodes$label[.siena_mode_rows(nodes, m, set_names)]) else NULL
  nodeSetObjs <- lapply(set_names, function(m)
    RSiena::sienaNodeSet(unname(sizes[m]), nodeSetName = m, names = labs_of(m)))
  W <- smeta$observations %||% .siena_infer_waves(x)
  cent_of <- function(nm) if (nm %in% names(centered)) unname(centered[nm]) else TRUE
  ns_of <- function(nm, default) svars[[nm]]$nodeSet %||% default
  objs <- list()
  # Tie layers: dependent networks, or dyadic covariates ----
  if (!is.null(x$ties) && nrow(x$ties)) {
    has_layer <- "layer" %in% names(x$ties)
    tie_layers <- if (has_layer) unique(as.character(x$ties$layer)) else "network"
    # With no declared dependent, treat the first tie layer as the dependent net.
    if (!length(focal)) focal <- tie_layers[1]
    for (L in tie_layers) {
      sub <- if (has_layer) x$ties[as.character(x$ties$layer) == L, , drop = FALSE] else x$ties
      meta <- svars[[L]]
      ns <- ns_of(L, set_names[1])
      off_from <- unname(offsets[ns[1]]); off_to <- unname(offsets[ns[length(ns)]])
      sz_from <- unname(sizes[ns[1]]);   sz_to <- unname(sizes[ns[length(ns)]])
      if (L %in% focal) { # dependent network
        if (W < 2)
          snet_abort(c("A SIENA dependent network requires at least two waves,",
                       "i" = "but the '{L}' layer has only one.",
                       "i" = paste("Add a 'time'/'wave' column to the ties, or",
                                   "mark the longitudinal layer(s) via info$focal.")))
        type <- meta$type %||% if (length(ns) > 1 || twomode) "bipartite" else "oneMode"
        arr <- .siena_ties_to_array(sub, W, sz_from, sz_to, off_from, off_to,
                                    onemode = identical(type, "oneMode"))
        if (!all(arr %in% c(0, 1, 10, 11) | is.na(arr)))
          snet_abort(c("A SIENA dependent network must have binary ties,",
                       "x" = "but the '{L}' layer is valued or signed.",
                       "i" = "Dichotomise it first with `to_unweighted()`."))
        if (labelled) dimnames(arr) <- list(labs_of(ns[1]), labs_of(ns[length(ns)]), NULL)
        objs[[L]] <- RSiena::sienaDependent(arr, type = type, nodeSet = ns,
                                            allowOnly = meta$allowOnly %||% TRUE)
      } else { # dyadic covariate
        has_time <- "time" %in% names(sub) && any(!is.na(sub$time))
        if (has_time) {
          arr <- .siena_ties_to_array(sub, max(1, W - 1), sz_from, sz_to,
                                      off_from, off_to, onemode = FALSE)
          objs[[L]] <- RSiena::varDyadCovar(arr, nodeSets = ns,
                                            centered = cent_of(L))
        } else {
          m <- .siena_ties_to_array(sub, 1, sz_from, sz_to, off_from, off_to,
                                    onemode = FALSE)[, , 1]
          objs[[L]] <- RSiena::coDyadCovar(m, nodeSets = ns, centered = cent_of(L))
        }
      }
    }
  }
  # Change variables: behavioural dependents, or varying covariates ----
  chvars <- setdiff(unique(as.character(x$changes$var)), "active")
  for (V in chvars) {
    ns <- ns_of(V, set_names[1])
    rows <- .siena_mode_rows(nodes, ns[1], set_names)
    off <- unname(offsets[ns[1]]); sz <- unname(sizes[ns[1]])
    if (V %in% focal) { # behavioural dependent [n, 1, W]
      mat <- .siena_changes_to_matrix(x$changes, V, sz, off, W)
      arr <- array(mat, dim = c(sz, 1, W))
      objs[[V]] <- RSiena::sienaDependent(arr, type = "behavior", nodeSet = ns[1],
                                          allowOnly = svars[[V]]$allowOnly %||% TRUE)
    } else { # varying covariate [n, W-1]
      mat <- .siena_changes_to_matrix(x$changes, V, sz, off, NULL)
      objs[[V]] <- RSiena::varCovar(mat, nodeSet = ns[1], centered = cent_of(V))
    }
  }
  # Constant covariates from (non-reserved) nodal columns ----
  reserved <- c("label", "mode", "active", "na", "type", "name")
  for (C in setdiff(names(nodes), reserved)) {
    ns <- ns_of(C, set_names[1])
    rows <- .siena_mode_rows(nodes, ns[1], set_names)
    vals <- nodes[[C]][rows]
    # SIENA covariates must be plain numeric vectors, so encode any categorical
    # attribute as numeric codes and strip stray attributes/classes.
    vals <- if (is.numeric(vals)) as.vector(vals) else as.numeric(as.factor(vals))
    objs[[C]] <- RSiena::coCovar(vals, nodeSet = ns[1], centered = cent_of(C))
  }
  # Composition change (from active-variable changes) ----
  comp <- .siena_build_composition(x, sizes, offsets, set_names, W, smeta)
  args <- objs
  if (!is.null(comp)) args <- c(args, list(comp))
  args <- c(args, list(nodeSets = nodeSetObjs))
  do.call(RSiena::sienaDataCreate, args)
}

# stocnet from sienadata ####

#' @export
as_stocnet.sienadata <- function(.data, twomode = FALSE) {
  thisRequires("RSiena")
  sd <- .data
  W <- sd$observations
  nsets <- sd$nodeSets
  set_names <- names(nsets)
  set_sizes <- stats::setNames(vapply(nsets, length, integer(1)), set_names)
  offsets <- stats::setNames(cumsum(c(0, set_sizes))[seq_along(set_sizes)],
                             set_names)
  labs <- .siena_node_labels(sd, set_names, set_sizes)
  labelled <- any(vapply(seq_along(set_names), function(i)
    !identical(labs[[i]], as.character(seq_len(set_sizes[i]))), logical(1)))
  multimode <- length(set_names) > 1
  nodes <- dplyr::tibble(label = unlist(labs, use.names = FALSE))
  if (multimode) nodes$mode <- rep(set_names, set_sizes)
  ties <- list(); changes <- list()
  focal <- character(0); centered <- logical(0)
  directed <- logical(0); svars <- list()
  # Dependent variables ----
  for (nm in names(sd$depvars)) {
    dv <- sd$depvars[[nm]]
    type <- attr(dv, "type"); ns <- attr(dv, "nodeSet")
    focal <- c(focal, nm)
    svars[[nm]] <- .siena_prune(list(role = "dependent", type = type, nodeSet = ns,
                                     allowOnly = attr(dv, "allowOnly"),
                                     uponly = attr(dv, "uponly"),
                                     downonly = attr(dv, "downonly"),
                                     sparse = attr(dv, "sparse")))
    if (identical(type, "behavior")) {
      changes[[nm]] <- .siena_behavior_to_changes(dv, nm, offsets[ns[1]], W)
    } else {
      directed[nm] <- !isTRUE(attr(dv, "symmetric"))
      ties[[nm]] <- .siena_array_to_ties(dv, nm, ns, offsets, W,
                                         onemode = identical(type, "oneMode"))
    }
  }
  # Constant covariates -> nodal columns ----
  for (nm in names(sd$cCovars)) {
    cc <- sd$cCovars[[nm]]; ns <- attr(cc, "nodeSet")
    col <- rep(NA_real_, nrow(nodes))
    col[.siena_mode_rows(nodes, ns, set_names)] <- .siena_uncenter(cc)
    nodes[[nm]] <- col
    centered[nm] <- isTRUE(attr(cc, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "coCovar", nodeSet = ns))
  }
  # Varying covariates -> change rows ----
  for (nm in names(sd$vCovars)) {
    vc <- sd$vCovars[[nm]]; ns <- attr(vc, "nodeSet")
    changes[[paste0(".vc_", nm)]] <- .siena_matrix_to_changes(
      matrix(.siena_uncenter(vc), nrow = dim(vc)[1]), nm, offsets[ns[1]])
    centered[nm] <- isTRUE(attr(vc, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "varCovar", nodeSet = ns))
  }
  # Constant dyadic covariates -> tie layer (no time) ----
  for (nm in names(sd$dycCovars)) {
    dc <- sd$dycCovars[[nm]]; ns <- attr(dc, "nodeSet")
    ties[[nm]] <- .siena_matrix_to_ties(dc, nm, ns, offsets)
    centered[nm] <- isTRUE(attr(dc, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "coDyadCovar",
                                     nodeSet = ns, type = attr(dc, "type"),
                                     sparse = attr(dc, "sparse")))
  }
  # Varying dyadic covariates -> tie layer (with time) ----
  for (nm in names(sd$dyvCovars)) {
    vd <- sd$dyvCovars[[nm]]; ns <- attr(vd, "nodeSet")
    ties[[nm]] <- .siena_array_to_ties(vd, nm, ns, offsets, dim(vd)[3],
                                       onemode = FALSE)
    centered[nm] <- isTRUE(attr(vd, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "varDyadCovar",
                                     nodeSet = ns, type = attr(vd, "type"),
                                     sparse = attr(vd, "sparse")))
  }
  # Composition change -> active change rows ----
  siena_meta <- list(version = attr(sd, "version"), observations = W,
                     labelled = labelled, vars = svars,
                     nodeSetRelations = .siena_prune(
                       list(higher = attr(sd, "higher"),
                            disjoint = attr(sd, "disjoint"),
                            atLeastOne = attr(sd, "atLeastOne"))))
  if (length(sd$compositionChange)) {
    ccobj <- sd$compositionChange[[1]]
    ns <- attr(ccobj, "nodeSet") %||% set_names[1]
    changes[["active"]] <- .siena_composition_to_changes(ccobj, ns, offsets, W)
    siena_meta$ccOption <- attr(sd$compositionChange, "ccOption")
    siena_meta$compositionNodeSet <- ns
  }
  # Assemble info ----
  info <- list(modes = set_names)
  tie_layers <- names(ties)
  if (length(tie_layers)) info$layers <- tie_layers
  if (length(directed)) info$directed <- directed
  if (length(focal)) info$focal <- focal
  if (length(centered)) info$centered <- centered
  info$siena <- siena_meta
  ties_tbl <- if (length(ties)) dplyr::bind_rows(ties) else NULL
  changes_tbl <- if (length(changes)) dplyr::bind_rows(changes) else NULL
  make_stocnet(info = info, nodes = nodes, ties = ties_tbl,
               changes = changes_tbl)
}

# siena coercion helpers ####

# Rows of the (global) node table belonging to a given mode/nodeSet.
.siena_mode_rows <- function(nodes, mode, set_names) {
  if ("mode" %in% names(nodes)) which(as.character(nodes[["mode"]]) == mode) else
    seq_len(nrow(nodes))
}

# Drop NULL and empty elements from a metadata list.
.siena_prune <- function(x) x[!vapply(x, function(e) is.null(e) || length(e) == 0,
                                      logical(1))]

# Recover the raw (uncentered) values of a nodal siena covariate. RSiena stores
# nodal covariates mean-centered; adding the stored mean back recovers the input
# so that re-centering on the way out reproduces the original exactly.
.siena_uncenter <- function(cov) {
  vals <- as.vector(cov)
  m <- attr(cov, "mean")
  if (isTRUE(attr(cov, "centered")) && !is.null(m)) vals <- vals + m
  vals
}

# Treat a 'wave' column as the canonical 'time' column.
.siena_wave_to_time <- function(tbl) {
  if (is.null(tbl)) return(tbl)
  if ("wave" %in% names(tbl) && !"time" %in% names(tbl))
    tbl <- dplyr::rename(tbl, time = "wave")
  tbl
}

# Infer the number of observations (waves) from the ties/changes of a stocnet.
.siena_infer_waves <- function(x) {
  tt <- if (!is.null(x$ties) && "time" %in% names(x$ties)) x$ties$time else NULL
  ct <- if (!is.null(x$changes) && "time" %in% names(x$changes)) x$changes$time else NULL
  suppressWarnings(max(c(1, as.numeric(tt), as.numeric(ct)), na.rm = TRUE))
}

# Collect node labels per node set from depvar/covar dimnames.
.siena_node_labels <- function(sd, set_names, set_sizes) {
  labs <- stats::setNames(lapply(set_names, function(m)
    as.character(seq_len(set_sizes[m]))), set_names)
  for (dv in sd$depvars) {
    ns <- attr(dv, "nodeSet"); dn <- dimnames(dv)
    if (is.null(dn)) next
    if (!is.null(dn[[1]]) && length(dn[[1]]) == set_sizes[ns[1]])
      labs[[ns[1]]] <- dn[[1]]
    if (length(ns) > 1 && length(dn) > 1 && !is.null(dn[[2]]) &&
        length(dn[[2]]) == set_sizes[ns[2]]) labs[[ns[2]]] <- dn[[2]]
  }
  labs
}

# A [n, n(/m), W] siena array -> tie tibble (from, to, layer, weight, time).
.siena_array_to_ties <- function(arr, nm, ns, offsets, W, onemode) {
  off_from <- unname(offsets[ns[1]]); off_to <- unname(offsets[ns[length(ns)]])
  rows <- lapply(seq_len(W), function(w) {
    g <- if (length(dim(arr)) == 3) arr[, , w] else arr
    if (onemode) diag(g) <- 0
    idx <- which(g != 0 | is.na(g), arr.ind = TRUE)
    if (!nrow(idx)) return(NULL)
    dplyr::tibble(from = off_from + idx[, 1], to = off_to + idx[, 2],
                  layer = nm, weight = as.vector(g[idx]), time = w)
  })
  dplyr::bind_rows(rows)
}

# A constant [n, n] siena matrix -> tie tibble (no time).
.siena_matrix_to_ties <- function(m, nm, ns, offsets) {
  m <- as.matrix(m)
  off_from <- unname(offsets[ns[1]]); off_to <- unname(offsets[ns[length(ns)]])
  idx <- which(m != 0 | is.na(m), arr.ind = TRUE)
  if (!nrow(idx)) return(NULL)
  dplyr::tibble(from = off_from + idx[, 1], to = off_to + idx[, 2],
                layer = nm, weight = as.vector(m[idx]))
}

# A behaviour [n, 1, W] siena array -> change rows for all waves.
.siena_behavior_to_changes <- function(dv, nm, off, W) {
  off <- unname(off)
  rows <- lapply(seq_len(W), function(w) {
    v <- as.vector(dv[, 1, w])
    dplyr::tibble(time = w, node = as.integer(off + seq_along(v)), var = nm,
                  value = as.list(v))
  })
  dplyr::bind_rows(rows)
}

# A varying covariate [n, periods] matrix -> change rows.
.siena_matrix_to_changes <- function(vc, nm, off) {
  off <- unname(off); vc <- as.matrix(vc)
  rows <- lapply(seq_len(ncol(vc)), function(p) {
    v <- vc[, p]
    dplyr::tibble(time = p, node = as.integer(off + seq_along(v)), var = nm,
                  value = as.list(v))
  })
  dplyr::bind_rows(rows)
}

# Composition change list -> active change rows (only non-trivial nodes).
.siena_composition_to_changes <- function(cclist, ns, offsets, W) {
  off <- unname(offsets[ns])
  rows <- lapply(seq_along(cclist), function(i) {
    v <- as.numeric(cclist[[i]])
    if (identical(v, c(1, W))) return(NULL)
    dplyr::tibble(time = v[1], node = as.integer(off + i), var = "active",
                  value = list(v))
  })
  dplyr::bind_rows(rows)
}

# Tie rows -> a [sz_from, sz_to, W] array (weights, preserving NA).
.siena_ties_to_array <- function(sub, W, sz_from, sz_to, off_from, off_to,
                                 onemode) {
  arr <- array(0, dim = c(sz_from, sz_to, W))
  if (nrow(sub)) {
    tv <- if ("time" %in% names(sub)) sub$time else rep(1, nrow(sub))
    wv <- if ("weight" %in% names(sub)) sub$weight else rep(1, nrow(sub))
    for (r in seq_len(nrow(sub))) {
      w <- if (is.na(tv[r])) 1 else as.integer(tv[r])
      if (w >= 1 && w <= W)
        arr[sub$from[r] - off_from, sub$to[r] - off_to, w] <- wv[r]
    }
  }
  if (onemode) for (w in seq_len(W)) diag(arr[, , w]) <- 0
  arr
}

# Change rows for one variable -> a [sz, W] matrix (NULL W -> infer periods).
.siena_changes_to_matrix <- function(chg, V, sz, off, W) {
  sub <- chg[as.character(chg$var) == V, , drop = FALSE]
  periods <- if (is.null(W)) max(as.numeric(sub$time)) else W
  mat <- matrix(NA_real_, nrow = sz, ncol = periods)
  for (r in seq_len(nrow(sub))) {
    val <- sub$value[[r]]
    mat[sub$node[r] - off, as.integer(sub$time[r])] <- as.numeric(val)[1]
  }
  mat
}

# Build a sienaCompositionChange from active-variable change rows. Two encodings
# are supported: the interval encoding produced by as_stocnet.sienadata (each
# value a numeric c(enter, leave, ...) vector), and the native manynet encoding
# (initial presence in nodes$active plus per-wave logical 'active' changes).
.siena_build_composition <- function(x, sizes, offsets, set_names, W, smeta) {
  if (is.null(x$changes) || !any(as.character(x$changes$var) == "active"))
    return(NULL)
  ns <- smeta$compositionNodeSet %||% set_names[1]
  off <- unname(offsets[ns]); sz <- unname(sizes[ns])
  sub <- x$changes[as.character(x$changes$var) == "active", , drop = FALSE]
  option <- smeta$ccOption %||% 1
  interval_encoded <- any(vapply(sub$value,
                                 function(v) length(as.numeric(unlist(v))) >= 2,
                                 logical(1)))
  if (interval_encoded) {
    changelist <- lapply(seq_len(sz), function(i) c(1, W)) # default present
    for (r in seq_len(nrow(sub))) {
      local <- sub$node[r] - off
      if (local >= 1 && local <= sz)
        changelist[[local]] <- as.numeric(unlist(sub$value[[r]]))
    }
  } else { # native per-wave presence -> intervals
    initial <- rep(TRUE, sz)
    if ("active" %in% names(x$nodes)) {
      av <- as.logical(x$nodes[["active"]])[.siena_mode_rows(x$nodes, ns, set_names)]
      if (length(av) == sz) initial <- av
    }
    present <- matrix(rep(initial, W), nrow = sz)
    ord <- order(as.numeric(sub$time))
    for (r in ord) {
      local <- sub$node[r] - off; w <- as.integer(sub$time[r])
      if (local >= 1 && local <= sz && !is.na(w) && w >= 1 && w <= W)
        present[local, w:W] <- as.logical(sub$value[[r]])[1]
    }
    changelist <- lapply(seq_len(sz), function(i)
      .siena_presence_to_interval(present[i, ], W))
  }
  RSiena::sienaCompositionChange(changelist, nodeSet = ns, option = option)
}

# A logical presence vector across waves -> RSiena (enter, leave, ...) intervals.
.siena_presence_to_interval <- function(p, W) {
  if (!any(p)) return(c(1, 1))
  r <- rle(p); times <- numeric(0); pos <- 1
  for (k in seq_along(r$lengths)) {
    len <- r$lengths[k]
    if (isTRUE(r$values[k])) times <- c(times, pos, pos + len - 1)
    pos <- pos + len
  }
  times
}

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
# Diffusion ####

#' @rdname coerce_graph
#' @param events A table (data frame or tibble) of diffusion events
#'   with columns `t` indicating the time (typically an integer) of the event, 
#'   `nodes` indicating the number or name of the node involved in the event,
#'   and `event`, which can take on the values "I" for an infection event,
#'   "E" for an exposure event, or "R" for a recovery event.
#' @returns 
#'   `as_diffusion()` and `play_diffusion()` return a 'diff_model' object
#'   that contains two different tibbles (tables) --
#'   a table of diffusion events and 
#'   a table of the number of nodes in each relevant component (S, E, I, or R) --
#'   as well as a copy of the network upon which the diffusion ran.
#'   By default, a compact version of the component table is printed
#'   (to print all the changes at each time point, use `print(..., verbose = T)`).
#'   To retrieve the diffusion events table, use `summary(...)`.
#' @importFrom dplyr tibble
#' @examples
#'   # How to create a diff_model object from (basic) observed data
#'   events <- data.frame(time = c(0,1,1,2,3), 
#'                        node = c(1,2,3,2,4),
#'                        var = "diffusion", 
#'                        value = c("I","I","I","R","I"))
#'   bind_changes(create_filled(4), events)
#' @export
as_diffusion <- function(.data, twomode = FALSE, events) UseMethod("as_diffusion")

#' @export
as_diffusion.diff_model <- function(.data, twomode = FALSE, events) {
  .data
}

#' @export
as_diffusion.mnet <- function(.data, twomode = FALSE, events) {
  events <- as_changelist(.data)
  nodes <- c(net_nodes(.data))
  sumchanges <- events |> dplyr::group_by(time) |> 
    dplyr::reframe(S_new = sum(value == "S"),
                   E_new = sum(value == "E"),
                   I_new = sum(value == "I"),
                   R_new = sum(value == "R"))
  report <- dplyr::tibble(time = 0:max(events$time),
                          n = nodes) |> 
    dplyr::left_join(sumchanges, by = dplyr::join_by(time))
  report[is.na(report)] <- 0
  
  if(all(report$E_new == 0)){
    report$S <- report$n + cumsum(report$S_new - report$I_new)
    report$E <- rep(0, nrow(report))
  } else {
    report$S <- report$n + cumsum(report$S_new - report$E_new) # susceptible decreases as they become exposed
    report$E <- cumsum(report$E_new) - cumsum(report$I_new) # exposed become infectious
  }
  report$I <- cumsum(report$I_new) - cumsum(report$R_new) # infectious recover
  report$R <- cumsum(report$R_new) - cumsum(report$S_new) # recovered accumulate
  report$s <- vapply(report$time, function(t){
    twin <- dplyr::filter(events, events$time <= t)
    infected <- dplyr::filter(twin, twin$value == "I")$node
    recovered <- dplyr::filter(twin, twin$value == "R")$node
    infected <- setdiff(infected, recovered)
    expos <- .node_is_exposed(.data, infected)
    expos[recovered] <- F
    sum(expos)
  }, numeric(1) )
  if (any((report$R + report$I + report$E + report$S) != report$n)) {
    snet_abort("Oops, something is wrong")
  }
  report <- dplyr::select(report, 
                          dplyr::any_of(c("time", "n", "S", "s", "S_new", "E", "E_new", 
                                          "I", "I_new", "R", "R_new")))
  # make_diff_model(events, report, .data)
  class(report) <- c("diff_model", class(report))
  report
}

#' @export
as_diffusion.igraph <- function(.data, twomode = FALSE, events) {
  net <- as_tidygraph(.data)
  if (missing(events)) {
    events <- as_changelist(.data)
  }
  events <- events |>
    dplyr::filter(var == "diffusion") |>
    dplyr::transmute(
      t = time,
      nodes = node,
      event = value
    )
  event <- NULL
  sumchanges <- events |> dplyr::group_by(t) |> 
    dplyr::reframe(I_new = sum(event == "I"),
                   E_new = sum(event == "E"),
                   R_new = sum(event == "R"))
  report <- dplyr::tibble(t = seq_len(max(events$t)) - 1,
                          n = net_nodes(net)) |> 
    dplyr::left_join(sumchanges, by = dplyr::join_by(t))
  report[is.na(report)] <- 0
  report$R <- cumsum(report$R_new)
  report$I <- cumsum(report$I_new) - report$R
  report$E <- ifelse(report$E_new == 0 & 
                       cumsum(report$E_new) == max(cumsum(report$E_new)),
                     report$E_new, cumsum(report$E_new))
  report$E <- ifelse(report$R + report$I + report$E > report$n,
                     report$n - (report$R + report$I),
                     report$E)
  report$S <- report$n - report$R - report$I - report$E
  report$s <- vapply(report$t, function(time){
    twin <- dplyr::filter(events, events$t <= time)
    infected <- dplyr::filter(twin, twin$event == "I")$nodes
    recovered <- dplyr::filter(twin, twin$event == "R")$nodes
    infected <- setdiff(infected, recovered)
    expos <- .node_is_exposed(net, infected)
    expos[recovered] <- F
    sum(expos)
  }, numeric(1) )
  if (any(report$R + report$I + report$E + report$S != report$n)) {
    snet_abort("Oops, something is wrong")
  }
  report <- dplyr::select(report, dplyr::any_of(c("t", "n", "S", "s", "E", "E_new", "I", "I_new", "R", "R_new")))
  make_diff_model(events, report, .data)
}

#' @export
as_diffusion.diffnet <- function(.data, twomode = FALSE, events) {
  diffnet <- .data
  net <- as.matrix(.data$graph[[1]])
  event <- NULL
  events <- data.frame(t = .data$toa, 
                       nodes = attr(.data$toa, "names"), 
                       event = "I")
  if(!all.equal(diffnet$graph[[1]], diffnet$graph[[length(diffnet$graph)]]))
    warning(paste("This function currently only takes the first network.",
                  "Network changes are not currently retained."))
  rownames(net) <- diffnet$meta$ids
  colnames(net) <- diffnet$meta$ids
  sumchanges <- events |> dplyr::group_by(t) |> 
    dplyr::reframe(I_new = sum(event == "I"),
                   E_new = sum(event == "E"),
                   R_new = sum(event == "R"))
  report <- dplyr::tibble(t = min(events$t):max(events$t),
                          n = diffnet$meta$n) |> 
    dplyr::left_join(sumchanges, by = dplyr::join_by(t))
  report[is.na(report)] <- 0
  report$R <- cumsum(report$R_new)
  report$I <- cumsum(report$I_new) - report$R
  report$E <- ifelse(report$E_new == 0 & 
                       cumsum(report$E_new) == max(cumsum(report$E_new)),
                     report$E_new, cumsum(report$E_new))
  report$E <- ifelse(report$R + report$I + report$E > report$n,
                     report$n - (report$R + report$I),
                     report$E)
  report$S <- report$n - report$R - report$I - report$E
  report$s <- vapply(report$t, function(time){
    twin <- dplyr::filter(events, events$t <= time)
    infected <- dplyr::filter(twin, twin$event == "I")$nodes
    recovered <- dplyr::filter(twin, twin$event == "R")$nodes
    infected <- setdiff(infected, recovered)
    expos <- .node_is_exposed(as_igraph(net), infected)
    expos[infected] <- F
    expos[recovered] <- F
    sum(expos)
  }, numeric(1) )
  if (any(report$R + report$I + report$E + report$S != report$n)) {
    snet_abort("Oops, something is wrong")
  }
  if(is_labelled(net)) events$nodes <- match(events$nodes, node_labels(net))
  events <- events |> dplyr::arrange(t)
  report <- dplyr::select(report, dplyr::any_of(c("t", "n", "S", "s", "E", "E_new", "I", "I_new", "R", "R_new")))
  make_diff_model(events, report, net)
}

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
      snet_unavailable()
      # netdiffuseR::igraph_to_diffnet(graph.list = to_waves(.data))
    } else {
      graph <- as_tidygraph(.data) |> mutate(toa = as.numeric(toa)) |> as_igraph()
      # suppressWarnings(netdiffuseR::igraph_to_diffnet(graph = graph,
      #                               toavar = "toa"))
      return(structure(list(graph = graph, toa = toa#, 
                            # adopt = adopt, 
                            # cumadopt = cumadopt, vertex.static.attrs = vertex.static.attrs, 
                            # vertex.dyn.attrs = vertex.dyn.attrs, graph.attrs = graph.attrs, 
                            # meta = meta
      ), class = "diffnet"))
    }
    
}
# nocov end

