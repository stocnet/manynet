#' Coercing into lists or matrices
#' @name coerce_list
#' @description
#'   These functions coerce objects into different objects by extracting
#'   and translating the information contained in the original object:
#'   - `as_edgelist()` coerces the object into an edgelist, as data frames or tibbles.
#'   - `as_nodelist()` coerces the object into a nodelist, as a data frame or tibble.
#'   - `as_changelist()` coerces the object into a changelist, as a data frame or tibble.
#'   - `as_infolist()` coerces the object into a list of network-level information, 
#'   such as the names of the nodes and ties, if not given in the nodelist or edgelist.
#'   - `as_matrix()` coerces the object into an adjacency (one-mode/unipartite) or incidence (two-mode/bipartite) matrix.
#'   If the network is a cognitive social structure (i.e. the edgelist contains a 'by' column
#'   indicating who reported/recorded each tie), `as_matrix()` returns a three-dimensional array
#'   instead, with dimensions for senders, receivers, and reporters.
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

#' @export
as_nodelist.tbl_graph <- function(.data) {
  out <- .data
  dplyr::tibble(data.frame(out))
}

#' @export
as_nodelist.igraph <- function(.data) {
  out <- .data
  dplyr::tibble(data.frame(as_tidygraph(out)))
}

#' @export
as_nodelist.stocnet <- function(.data) {
  .data$nodes
}

#' @export
as_nodelist.network <- function(.data) {
  out <- .data
  out <- network::as.data.frame.network(out, unit = "vertices")
  dplyr::as_tibble(out)
}

# Changelists ####

#' @rdname coerce_list
#' @export
as_changelist <- function(.data) UseMethod("as_changelist")

#' @export
as_changelist.tbl_graph <- function(.data) {
  out <- igraph::graph_attr(as_igraph(.data), "changes")
  dplyr::tibble(data.frame(out))
}

#' @export
as_changelist.igraph <- function(.data) {
  out <- igraph::graph_attr(.data, "changes")
  dplyr::tibble(data.frame(out))
}

#' @export
as_changelist.stocnet <- function(.data) {
  .data$changes
}

#' @export
as_changelist.network <- function(.data) {
  out <- network::get.network.attribute(.data, "changes")
  dplyr::tibble(data.frame(out))
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
  igraph::as_data_frame(.data, what = "edges") |>
    dplyr::as_tibble()
}

#' @export
as_edgelist.tbl_graph <- function(.data, twomode = FALSE) {
  igraph::as_data_frame(.data, what = "edges") |> 
    dplyr::as_tibble()
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
as_edgelist.matrix <- function(.data, twomode = FALSE) {
  as_edgelist(as_igraph(.data, twomode = twomode))
}

#' @export
as_edgelist.data.frame <- function(.data, twomode = FALSE) {
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
as_edgelist.network.goldfish <- function(.data, twomode = FALSE) {
  as_matrix(as_igraph(.data, twomode = twomode))
}

#' @export
as_edgelist.siena <- function(.data, twomode = NULL) {
  as_edgelist(as_igraph(.data, twomode = twomode))
}

#' @export
as_edgelist.stocnet <- function(.data, twomode = NULL) {
  .data$ties
}

# Infolists ####

#' @rdname coerce_list
#' @export
as_infolist <- function(.data) UseMethod("as_infolist")

#' @export
as_infolist.igraph <- function(.data){
  out <- igraph::graph_attr(.data)
  if("grand" %in% names(out)){ 
    out <- out$grand 
    if("mode" %in% names(out)) out$mode <- NULL
  }
  if("changes" %in% names(out)) out$changes <- NULL
  out
}

#' @export
as_infolist.tbl_graph <- function(.data){
  out <- igraph::graph_attr(.data)
  if("grand" %in% names(out)){ 
    out <- out$grand 
    if("mode" %in% names(out)) out$mode <- NULL
  }
  if("changes" %in% names(out)) out$changes <- NULL
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

# Matrices ####

#' @rdname coerce_list
#' @importFrom dplyr arrange
#' @importFrom igraph edge_attr_names as_adjacency_matrix as_biadjacency_matrix
#' @importFrom network is.bipartite list.edge.attributes as.matrix.network
#' @export
as_matrix <- function(.data,
                      twomode = NULL) UseMethod("as_matrix")

# Helper to convert cognitive social structure edgelist to 3D array
.cognitive_to_array <- function(.data, twomode = NULL) {
  if (is.data.frame(.data) && all(c("from", "to", "by") %in% names(.data))) {
    el <- .data
  } else {
    el <- as_edgelist(.data)
  }
  if (!"by" %in% names(el)) {
    stop("Expected a cognitive social structure with a 'by' column in the edgelist.")
  }
  reporters <- sort(unique(el$by))
  from_nodes <- sort(unique(as.character(el$from)))
  to_nodes <- sort(unique(as.character(el$to)))
  # Determine if twomode
  twomode_net <- if (!is.null(twomode)) twomode else is_twomode(.data)
  if (twomode_net) {
    row_nodes <- from_nodes
    col_nodes <- to_nodes
  } else {
    all_nodes <- sort(unique(c(from_nodes, to_nodes)))
    row_nodes <- all_nodes
    col_nodes <- all_nodes
  }
  # Create 3D array: rows x cols x reporters

  out <- array(0L, dim = c(length(row_nodes), length(col_nodes), 
                           length(reporters)),
               dimnames = list(row_nodes, col_nodes, reporters))
  # Fill in the array

  for (i in seq_len(nrow(el))) {
    r <- as.character(el$from[i])
    cc <- as.character(el$to[i])
    b <- as.character(el$by[i])
    val <- if ("weight" %in% names(el)) el$weight[i] else 1L
    out[r, cc, b] <- val
  }
  out
}

#' @export
as_matrix.data.frame <- function(.data,
                                 twomode = NULL) {
  if (is_cognitive(.data)) return(.cognitive_to_array(.data, twomode = twomode))
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
  if(!is_twomode(.data) && all(rownames(.data) == as.character(seq_nodes(.data)))) attr(.data, "dimnames") <- NULL
  if(!is_twomode(.data) && sum(.data[lower.tri(.data)])==0) .data <- .data + t(.data)
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
  if (is_cognitive(.data)) return(.cognitive_to_array(.data, twomode = twomode))
  if ((!is.null(twomode) && twomode) | 
      (is.null(twomode) & is_twomode(.data) & !is_multiplex(.data))) {
    if (is_weighted(.data) | is_signed(.data)) {
      mat <- igraph::as_biadjacency_matrix(.data, sparse = FALSE,
                                           attr = ifelse(is_weighted(.data), "weight", 
                                                         ifelse(is_signed(.data), "sign", NULL)))
    } else {
      mat <- igraph::as_biadjacency_matrix(.data, sparse = FALSE,
                                           attr = NULL)
    }
  } else {
    if (is_weighted(.data) | is_signed(.data)) {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = ifelse(is_weighted(.data), "weight", 
                                                       ifelse(is_signed(.data), "sign", NULL)))
      # Where multiplex network 
      if(anyNA(mat) && is_multiplex(.data)) mat[is.na(mat)] <- 1
    } else {
      mat <- igraph::as_adjacency_matrix(.data, sparse = FALSE,
                                         attr = NULL)
    }
  }
  if(!is_labelled(.data)) attr(mat, "dimnames") <- NULL
  mat
}

#' @export
as_matrix.tbl_graph <- function(.data,
                                twomode = NULL) {
  if (is_cognitive(.data)) return(.cognitive_to_array(.data, twomode = twomode))
  as_matrix(as_igraph(.data), twomode = twomode)
}

#' @export
as_matrix.network <- function(.data,
                              twomode = NULL) {
  if (is_cognitive(.data)) return(.cognitive_to_array(.data, twomode = twomode))
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
  if (is_cognitive(.data)) return(.cognitive_to_array(.data, twomode = twomode))
  as_matrix(as_igraph(.data, twomode = twomode))
}

