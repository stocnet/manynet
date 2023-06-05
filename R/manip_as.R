#' Coercion between manynet-compatible object classes
#'
#' @description
#' The `as_` functions in `{manynet}` coerce objects
#' between several common classes of social network objects.
#' These include:
#' - edgelists, as data frames or tibbles
#' - adjacency (one-mode/unipartite) and incidence (two-mode/bipartite) matrices
#' - `{igraph}` `graph` objects
#' - `{tidygraph}` `tbl_graph` objects
#' - `{network}` `network` objects
#'
#' An effort is made for all of these coercion routines to be as lossless
#' as possible, though some object classes are better at retaining certain
#' kinds of information than others.
#' Note also that there are some reserved column names in one or more
#' object classes, which could otherwise lead to some unexpected results.
#' @name as
#' @family manipulations
#' @inheritParams is
#' @param twomode Logical option used to override heuristics for
#'   distinguishing incidence (two-mode/bipartite) from
#'   adjacency (one-mode/unipartite) networks.
#'   By default FALSE.
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
#' This information is usually already embedded in `{igraph}`,
#' `{tidygraph}`, and `{network}` objects.
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
#' |  to/from  | edgelists  | matrices  |igraph  |tidygraph  |network  | siena | goldfish
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|
#' | edgelists (data frames)  | X | X | X | X | X | X | X |
#' | matrices                 | X | X | X | X | X | X | X |
#' | igraph                   | X | X | X | X | X | X | X |
#' | tidygraph                | X | X | X | X | X | X | X |
#' | network                  | X | X | X | X | X | X | X |
#' | graphAM                  | X | X | X | X | X | X | X |
NULL

# Edgelists ####

#' @describeIn as Coercing various network objects into an edgelist
#' @importFrom igraph as_data_frame
#' @importFrom dplyr as_tibble arrange
#' @importFrom network get.edge.attribute as.edgelist
#' @export
as_edgelist <- function(.data,
                        twomode = FALSE) UseMethod("as_edgelist")

#' @export
as_edgelist.igraph <- function(.data,
                               twomode = FALSE) {
  igraph::as_data_frame(.data, what = "edges") %>% 
    dplyr::as_tibble()
}

#' @export
as_edgelist.tbl_graph <- function(.data,
                                  twomode = FALSE) {
  igraph::as_data_frame(.data, what = "edges") %>% 
    dplyr::as_tibble()
}

#' @export
as_edgelist.network <- function(.data,
                                twomode = FALSE) {
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
  # Handle edge weights
  if (is_weighted(.data)) {
    edges[,3] <- network::get.edge.attribute(.data, "weight")
    names(edges) <- c("from", "to", "weight")
  } else names(edges) <- c("from", "to")
  # Remove weight column if only unity weights.
  if (all(edges$weight == 1)) edges <- edges[, -3]
  dplyr::arrange(dplyr::as_tibble(edges), from, to)
}

#' @export
as_edgelist.matrix <- function(.data,
                               twomode = FALSE) {
  as_edgelist(as_igraph(.data,
                        twomode = FALSE))
}

#' @export
as_edgelist.data.frame <- function(.data,
                                   twomode = FALSE) {
  if (ncol(.data) == 2 && any(names(.data) != c("from", "to"))) {
    names(.data) <- c("from", "to")
    .data
  } else if(ncol(.data) == 3 && 
            (any(names(.data) != c("from", "to", "weight")) | 
            any(names(.data) != c("from", "to", "sign")))) {
    names(.data) <- c("from", "to", "weight")
    .data
  } else .data
}

#' @export
as_edgelist.network.goldfish <- function(.data,
                                         twomode = FALSE) {
  as_matrix(as_igraph(.data, twomode = twomode))
}

#' @export
as_edgelist.siena <- function(.data,
                              twomode = NULL) {
  as_edgelist(as_igraph(.data, twomode = twomode))
}

# Matrices ####

#' @rdname as
#' @importFrom dplyr arrange
#' @importFrom igraph edge_attr_names as_adjacency_matrix as_incidence_matrix
#' @importFrom network is.bipartite list.edge.attributes as.matrix.network
#' @export
as_matrix <- function(.data,
                      twomode = NULL) UseMethod("as_matrix")

#' @export
as_matrix.data.frame <- function(.data,
                                 twomode = NULL) {
  if ("tbl_df" %in% class(.data)) .data <- as.data.frame(.data)
  if (ncol(.data) == 2 | !is_weighted(.data)) {
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
                     .Dim = c(as.integer(length(nodes1)),
                              as.integer(length(nodes2))),
                     .Dimnames = list(nodes1, nodes2))
  }
  .data
}

#' @export
as_matrix.matrix <- function(.data,
                             twomode = NULL) {
  .data
}

#' @export
as_matrix.igraph <- function(.data,
                             twomode = NULL) {
  if ((!is.null(twomode) && twomode) | (is.null(twomode) & is_twomode(.data))) {
    if (is_weighted(.data) | is_signed(.data)) {
      mat <- igraph::as_incidence_matrix(.data, sparse = FALSE,
                                         attr = igraph::edge_attr_names(.data)[[1]])
    } else {
      mat <- igraph::as_incidence_matrix(.data, sparse = FALSE,
                                         attr = NULL)
    }
  } else {
    if (is_weighted(.data) | is_signed(.data)) {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = igraph::edge_attr_names(.data)[[1]])
    } else {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = NULL)
    }
  }
  mat
}

#' @export
as_matrix.tbl_graph <- function(.data,
                                twomode = NULL) {
  as_matrix(as_igraph(.data), twomode = twomode)
}

#' @export
as_matrix.network <- function(.data,
                              twomode = NULL) {
  if (network::is.bipartite(.data)) {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      network::as.matrix.network(.data,
                                 attrname = "weight",
                                 expand.bipartite = FALSE)
      # Note: if expand.bipartite is true it returns the adjacency matrix. If
      # false it returns the incidence matrix that we want. Use
      # to_multilevel(mat) on the resulting matrix to do the conversion if needed.
    } else {
      network::as.matrix.network(.data,
                                 expand.bipartite = FALSE)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      network::as.matrix.network(.data, attrname = "weight")
    } else {
      network::as.matrix.network(.data)
    }
  }
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
  ddvs <- names(which(dvs == TRUE))
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
  for (k in seq_len(length(.data$dycCovars))) {
    out <- .data$dycCovars[[ddvs[k]]] + out
  }
  # Add dyvCovars
  for (k in seq_len(length(.data$dyvCovars))) {
    for (d in seq_len(dim(.data$dyvCovars[[k]])[3])) {
      out <- .data$dyvCovars[[k]][,,d] + out
    }
  }
  out
}

# igraph ####

#' @rdname as
#' @importFrom igraph graph_from_data_frame graph_from_incidence_matrix
#'  graph_from_adjacency_matrix delete_vertex_attr V vertex_attr
#'  edge_attr delete_edge_attr set_edge_attr
#' @importFrom network list.edge.attributes as.sociomatrix
#' @export
as_igraph <- function(.data,
                      twomode = FALSE) UseMethod("as_igraph")

#' @export
as_igraph.data.frame <- function(.data,
                                 twomode = FALSE) {
  if (inherits(.data, "tbl_df")) .data <- as.data.frame(.data)
  # Warn if no column named weight and weight set to true
  if (is_weighted(.data) & !("weight" %in% names(.data))) {
    names(.data)[3] <- "weight"
    # stop("Please rename the weight column of your dataframe to 'weight'")
  }
  if (!is_labelled(.data)) {
    graph <- igraph::graph_from_data_frame(.data, 
                      vertices = data.frame(name = 1:max(c(.data$from, .data$to))))
  } else graph <- igraph::graph_from_data_frame(.data)
  if (!is_labelled(.data)) {
    graph <- igraph::delete_vertex_attr(graph, "name")
  }
  # length(intersect(c(.data[,1]), c(.data[,2]))) == 0 && length(.data[,1])>1
  if (twomode) {
    igraph::V(graph)$type <- igraph::V(graph)$name %in% .data[,2]
  }
  graph
}

#' @export
as_igraph.matrix <- function(.data,
                             twomode = FALSE) {
  if (nrow(.data) != ncol(.data) | twomode) {
    if (!(all(.data %in% c(0, 1)))) {
      graph <- igraph::graph_from_incidence_matrix(.data, 
                                                   weighted = TRUE, 
                                                   directed = FALSE)
    } else {
      graph <- igraph::graph_from_incidence_matrix(.data, 
                                                   directed = FALSE)
    }
  } else {
    if (!(all(.data %in% c(0, 1)))) {
      graph <- igraph::graph_from_adjacency_matrix(.data, 
                                                   mode = ifelse(all(.data == t(.data)),
                                                                 "undirected", "directed"),
                                                   weighted = TRUE)
    } else {
      graph <- igraph::graph_from_adjacency_matrix(.data, 
                                                   mode = ifelse(all(.data == t(.data)),
                                                                 "undirected", "directed"))
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
      graph <- igraph::graph_from_incidence_matrix(graph, weighted = TRUE)
    } else {
      graph <- network::as.sociomatrix(.data)
      graph <- igraph::graph_from_incidence_matrix(graph) 
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(.data)) {
      graph <- network::as.sociomatrix(.data, attrname = "weight")
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   weighted = TRUE,
                                                   mode = ifelse(.data$gal$directed,
                                                                 "directed",
                                                                 "undirected"))
    } else if (length(network::list.edge.attributes(.data)) > 1) {
      .data$gal$multiple <- FALSE
      graph <- network::as.sociomatrix(.data, attrname = network::list.edge.attributes(.data)[1])
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   weighted = TRUE,
                                                   mode = ifelse(.data$gal$directed,
                                                                 "directed",
                                                                 "undirected"))
    } else {
      graph <- network::as.sociomatrix(.data)
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   mode = ifelse(.data$gal$directed,
                                                                 "directed",
                                                                 "undirected"))
    }
  }
  # Add remaining node level attributes
  if (length(attr) > 2) {
    for (a in attr[2:length(attr)]) {
      graph <- igraph::set_vertex_attr(graph, name = a,
                                       value = sapply(.data[[3]], "[[", a))
    }
  }
  graph
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
  } else stop("Non-empty starts are not yet supported by this function.")
  out
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
          y <- y %>%
            dplyr::mutate(weight = 1)
          x <- dplyr::bind_rows(y, as_edgelist(x)) %>%
            as_igraph()
          x <- igraph::set_edge_attr(x, name = paste0(name, "_", "t", d),
                                     value = igraph::edge_attr(as_igraph(x),
                                                               "weight")) %>%
            igraph::delete_edge_attr("weight")
        }
      } else {
        # add names for one-mode y
        y <- igraph::vertex_attr(y, name = "name",
                                 value = as.character(seq_len(igraph::vcount(as_igraph(y)))))
        # join ties
        if (isTRUE(is_twomode(x))) { # x is twomode but y is onemode
          y <- as_edgelist(y)
          y <- y %>%
            dplyr::mutate(weight = 1)
          x <- dplyr::bind_rows(y, as_edgelist(x)) %>%
            as_igraph()
          x <- igraph::set_edge_attr(x, name = paste0(name, "_", "t", d),
                                     value = igraph::edge_attr(as_igraph(x),
                                                               "weight")) %>%
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
      x <- igraph::vertex_attr(x, name = paste0(name, "_", "t", d),
                               value = as.vector(ndy[,d]))
    }
    x
  }
  # We always get the dependent network(s) first
  # Identify all dyadic and non-dyadic depvars
  dvs <- lapply(.data$depvars, function(x) is.matrix(x[,,1]) )
  ddvs <- names(which(dvs == TRUE))
  # Add in first network as base and add names
  out <- .data$depvars[[ddvs[1]]][,,1] # first wave
  if (is_twomode(out) == FALSE) {
    out <- igraph::vertex_attr(out, name = "name",
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
                                                         "orig")) %>%
    igraph::delete_edge_attr("orig")
  # Add rest of the dyadic depvars
  if (length(ddvs) > 1) {
    for (l in 2:length(ddvs)) {
      out <- .get_all_time_periods(.data$depvars[[ddvs[l]]], out,
                                   name = ddvs[l])
    }
  }
  # Add dycCovar
  for (k in seq_len(length(.data$dycCovars))) {
    out <- join_ties(out, as_igraph(.data$dycCovars[k]), 
                     attr_name = paste0(names(.data$dycCovars)[k]))
  }
  # Add dyvCovars
  for (k in seq_len(length(.data$dyvCovars))) {
    out <- .get_all_time_periods(.data$dyvCovars[[k]], out,
                                 name = paste0(names(.data$dyvCovars)[k]))
  }
  # Add any behavioral depvars
  if(length(which(dvs == FALSE)) > 0) {
    bdvs <- names(which(dvs == FALSE))
    for (b in seq_len(length(bdvs))) {
      out <- .get_attributes(.data$depvars[[bdvs[b]]], out,
                             name = bdvs[b])
    }
  }
  # Add composition change
  for (k in seq_len(length(.data$compositionChange))) {
    out <- igraph::vertex_attr(out, name =  paste0(names(.data$compositionChange)[k]),
                               value = as.vector(.data$compositionChange[[k]]))
  }
  # Add cCovar
  for (k in seq_len(length(.data$cCovars))) {
    out <- igraph::vertex_attr(out, name = paste0(names(.data$cCovars)[k]),
                               value = as.vector(.data$cCovars[[k]]))
  }
  # Add vCovar
  for (k in seq_len(length(.data$vCovars))) {
    out <- .get_attributes(.data$vCovars[[k]], out,
                           name = paste0(names(.data$vCovars)[k]))
  }
  out
}

# tidygraph ####

#' @rdname as
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_data_frame
#' @export
as_tidygraph <- function(.data, twomode = FALSE) UseMethod("as_tidygraph")

#' @export
as_tidygraph.data.frame <- function(.data, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(.data))
}

#' @importFrom tidygraph tbl_graph
#' @export
as_tidygraph.list <- function(.data, twomode = FALSE) {
  if (!is.null(names(.data))){
    if ("nodes" %in% names(.data) & "ties" %in% names(.data)) {
      tidygraph::tbl_graph(nodes = .data[["nodes"]],
                           edges = .data[["ties"]])
    } else if ("nodes" %in% names(.data) & "edges" %in% names(.data)) {
      tidygraph::tbl_graph(nodes = .data[["nodes"]],
                           edges = .data[["edges"]])
    } else stop("Please name the list elements 'nodes' and 'ties'.")
  } else stop("Please name the list elements 'nodes' and 'ties'.")
}
  
#' @export
as_tidygraph.matrix <- function(.data, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(.data))
}

#' @export
as_tidygraph.igraph <- function(.data, twomode = FALSE) {
  tidygraph::as_tbl_graph(.data)
}

#' @export
as_tidygraph.tbl_graph <- function(.data, twomode = FALSE) {
  .data
}

#' @export
as_tidygraph.network <- function(.data, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(.data))
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
  } else stop("Non-empty starts are not yet supported by this function.")

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
  as_tidygraph(as_igraph.siena(.data, twomode = FALSE))
}

# Network ####

#' @rdname as
#' @importFrom network as.network set.vertex.attribute
#' @importFrom igraph get.vertex.attribute
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
  attr <- as.data.frame(igraph::get.vertex.attribute(.data))
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
  nodes <- NULL
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
as_network.siena <- function(.data, twomode = FALSE) {
  as_network(as_igraph.siena(.data, twomode = FALSE))
}

# RSiena ####

#' @rdname as
#' @export
as_siena <- function(.data,
                      twomode = FALSE) UseMethod("as_siena")

#' @export
as_siena.igraph <- function(.data, twomode = FALSE) {
  if (!requireNamespace("RSiena", quietly = TRUE)) {
    if(utils::askYesNo(msg = "The `RSiena` package is required.
                       Would you like to install `RSiena` from CRAN?")) {
      utils::install.packages('RSiena')
    } else {
      stop("Please install `RSiena` from CRAN to coerce into RSiena data objects.")
    }
  }
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
  # nodeatts <- network_node_attributes(.data)
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

# graphAM ####

#' @rdname as
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
  if (!requireNamespace("RSiena", quietly = TRUE)) {
    if(utils::askYesNo(msg = "The `RSiena` package is required.
                       Would you like to install `RSiena` from CRAN?")) {
      utils::install.packages('RSiena')
    } else {
      stop("Please install `RSiena` from CRAN to coerce into RSiena data objects.")
    }
  }
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
