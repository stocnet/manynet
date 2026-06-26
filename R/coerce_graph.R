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

#' @export
as_igraph.stocnet <- function(.data, twomode = FALSE) {
  if(is.null(as_nodelist(.data)) || length(as_nodelist(.data)) == 0){
    out <- igraph::graph_from_data_frame(as_edgelist(.data))
    out <- to_unnamed(out)
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

#' @export
as_tidygraph.stocnet <- function(.data, twomode = FALSE) {
  as_tidygraph(as_igraph.stocnet(.data, twomode = twomode))
}

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
as_network.stocnet <- function(.data, twomode = FALSE) {
  out <- as_network(as_igraph.stocnet(.data))
  out$gal <- as_infolist(.data)
  if(!is.null(as_changelist(.data)) && length(as_changelist(.data)) > 0)
    network::set.network.attribute(out, "changes", as_changelist(.data))
  if(!is.null(as_globallist(.data)) && length(as_globallist(.data)) > 0)
    network::set.network.attribute(out, "global", as_globallist(.data))
  out
}

#' @export
as_network.networkDynamic <- function(.data, twomode = FALSE) {
  out <- .data
  class(out) <- setdiff(class(out), "networkDynamic")
  out
}

# RSiena ####

#' @rdname coerce_graph
#' @export
as_siena <- function(.data,
                     twomode = FALSE) UseMethod("as_siena")

#' @export
as_siena.igraph <- function(.data, twomode = FALSE) {
  thisRequires("RSiena")
  # First separate out the dependent ties
  nets <- igraph::edge_attr_names(as_igraph(.data))
  ties <- unique(gsub("_t[0-9]","", nets))
  waves <- max(vapply(strsplit(nets, "_t"), function(t)
    as.numeric(t[2]), numeric(1)))
  depnet <- ties[1]
  depnetArray <- simplify2array(lapply(1:waves, function(t)
    as_matrix(to_uniplex(.data, paste0(depnet, "_t", t)))))
  depnet <- RSiena::sienaDependent(depnetArray, 
                                   type = ifelse(is_twomode(.data) | twomode,
                                                 "bipartite", "oneMode"))
  # nodeatts <- net_node_attributes(.data)
  # nodeatts <- nodeatts[nodeatts != "name"]
  # # Add constant covariates
  # consatts <- nodeatts[!grepl("_t[0-9]",nodeatts)]
  # consvars <- lapply(consatts, function(cons) 
  #   RSiena::coCovar(node_attribute(.data, cons)))
  # names(consvars) <- consatts
  # .newEnv <- new.env(parent=globalenv())
  # list2env(consvars, envir = .newEnv)
  # RSiena::varCovar()
  RSiena::sienaDataCreate(list("depnet" = depnet))
}

#' @export
as_siena.tbl_graph <- function(.data, twomode = FALSE) {
  as_siena.igraph(.data, twomode = twomode)
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
as_graphAM.matrix <- function(.data, twomode = NULL) {
  thisRequires("methods")
  methods::new("graphAM", adjMat = to_onemode(.data), 
               edgemode = ifelse(is_directed(.data), "directed", "undirected"))
}

#' @export
as_graphAM.igraph <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.tbl_graph <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.network <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.data.frame <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.siena <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.network.goldfish <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

#' @export
as_graphAM.stocnet <- function(.data, twomode = NULL) {
  as_graphAM(as_matrix(.data), twomode)
}

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
  if(is_labelled(net)) events$nodes <- match(events$nodes, node_names(net))
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
      out$nodes <- node_names(as_igraph(.data))[out$nodes]
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
  if(is_multiplex(.data)){
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
  out <- list(info = as_infolist(.data), 
              nodes = as_nodelist(.data), 
              changes = as_changelist(.data), 
              ties = as_edgelist(.data))
  class(out) <- c("stocnet", class(out))
  if(inherits(network::network.vertex.names(.data), "integer"))
    out$nodes$vertex.names <- NULL
  out
}
