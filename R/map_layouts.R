# Configurational ####

#' Layout algorithms based on configurational positions
#' 
#' @description
#'   Configurational layouts locate nodes at symmetric coordinates
#'   to help illustrate particular configurations.
#'   Currently "triad" and "quad" layouts are available.
#'   The "configuration" layout will choose the appropriate configurational
#'   layout automatically.
#' 
#' @name map_layout_configuration
#' @family mapping
#' @inheritParams map_layout_partition
NULL

#' @rdname map_layout_configuration
#' @export
layout_tbl_graph_configuration <- function(.data,
                                           circular = FALSE, times = 1000){
  if (net_nodes(.data) == 2) {
    layout_tbl_graph_dyad(.data, circular = circular, times = times)
  } else if (net_nodes(.data) == 3) {
    layout_tbl_graph_triad(.data, circular = circular, times = times)
  } else if (net_nodes(.data) == 4) {
    layout_tbl_graph_quad(.data, circular = circular, times = times)
  }
}

#' @rdname map_layout_configuration
#' @export
layout_tbl_graph_dyad <- function(.data,
                                   circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  1,0), 2, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname map_layout_configuration
#' @export
layout_tbl_graph_triad <- function(.data,
                                   circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  2,3.5,
                  4,0), 3, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname map_layout_configuration
#' @export
layout_tbl_graph_quad <- function(.data,
                                  circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  0,1,
                  1,0,
                  1,1), 4, 2, byrow = TRUE)
  .to_lo(res)  
}

# Partitions ####

#' Layout algorithms based on bi- or other partitions
#' 
#' @description
#'   These algorithms layout networks based on two or more partitions,
#'   and are recommended for use with `graphr()` or `{ggraph}`.
#'   Note that these layout algorithms use `{Rgraphviz}`, 
#'   a package that is only available on Bioconductor.
#'   It will first need to be downloaded using `BiocManager::install("Rgraphviz")`.
#'   If it has not already been installed, there is a prompt the first time
#'   these functions are used though.
#'
#'   The "hierarchy" layout layers the first node set along the bottom,
#'   and the second node set along the top, 
#'   sequenced and spaced as necessary to minimise edge overlap.
#'   The "alluvial" layout is similar to "hierarchy", 
#'   but places successive layers horizontally rather than vertically.
#'   The "railway" layout is similar to "hierarchy",
#'   but nodes are aligned across the layers.
#'   The "ladder" layout is similar to "railway",
#'   but places successive layers horizontally rather than vertically.
#'   The "concentric" layout places a "hierarchy" layout
#'   around a circle, with successive layers appearing as concentric circles.
#'   The "multilevel" layout places successive layers as multiple levels.
#'   The "lineage" layout ranks nodes in Y axis according to values.
#' @name map_layout_partition
#' @inheritParams mark_is
#' @param circular Should the layout be transformed into a radial representation. 
#' Only possible for some layouts. Defaults to FALSE.
#' @param times Maximum number of iterations, where appropriate
#' @param radius A vector of radii at which the concentric circles
#'   should be located for "concentric" layout.
#'   By default this is equal placement around an empty centre, 
#'   unless one (the core) is a single node,
#'   in which case this node occupies the centre of the graph.
#' @param order.by An attribute label indicating the (decreasing) order
#'   for the nodes around the circles for "concentric" layout.
#'   By default ordering is given by a bipartite placement that reduces
#'   the number of edge crossings.
#' @param membership A node attribute or a vector to draw concentric circles
#'   for "concentric" layout.
#' @param center Further split "hierarchical" layouts by
#'   declaring the "center" argument as the "events", "actors",
#'   or by declaring a node name in hierarchy layout. 
#'   Defaults to NULL.
#' @param level A node attribute or a vector to hierarchically order levels for
#'   "multilevel" layout.
#' @param rank A numerical node attribute to place nodes in Y axis
#'   according to values for "lineage" layout.
#' @family mapping
#' @source
#'   Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#'   "Systematic identification of transcriptional regulatory modules from
#'   protein-protein interaction networks". 
#'   _Nucleic Acids Research_, 42 (1) e6.
NULL

#' @rdname map_layout_partition
#' @examples
#' #graphr(ison_southern_women, layout = "hierarchy", center = "events",
#' #           node_color = "type", node_size = 3)
#' @export
layout_tbl_graph_hierarchy <- function(.data, center = NULL,
                                       circular = FALSE, times = 1000) {
  if (is.null(center)) {
    thisRequiresBio("Rgraphviz")
    prep <- as_matrix(.data)
    if (anyDuplicated(rownames(prep))) {
      rownames(prep) <- seq_len(nrow(prep))
      colnames(prep) <- seq_len(ncol(prep)) + max(nrow(prep))
    }
    if (any(prep<0)) prep[prep<0] <- 0
    out <- as_graphAM(prep)
    out <- suppressMessages(Rgraphviz::layoutGraph(out, layoutType = 'dot',
                                                   attrs = list(graph = list(rankdir = "BT"))))
    nodeX <- .rescale(out@renderInfo@nodes$nodeX)
    nodeY <- .rescale(out@renderInfo@nodes$nodeY)
    if (is_twomode(.data) & "name" %in% igraph::vertex_attr_names(.data)) {
      names <- igraph::vertex_attr(.data, "name")
      nodeX <- nodeX[order(match(names(nodeX), names))]
      nodeY <- nodeY[order(match(names(nodeY), names))]
    }
    # nodeY <- abs(nodeY - max(nodeY))
    out <- .to_lo(cbind(nodeX, nodeY))
  } else {
    if (!is_twomode(.data)) cli::cli_abort("Please declare a two-mode network.")
    net <- as_matrix(.data)
    nn <- dim(net)[1]
    mm <- dim(net)[2]
    if (center == "actors") {
      Act <- cbind(rep(1, nrow(net)), nrm(rng(nn)))
      Evt1 <- cbind(rep(0, ceiling(ncol(net)/2)), nrm(rng(ceiling(mm/2))))
      Evt2 <- cbind(rep(2, floor(ncol(net)/2)), nrm(rng(floor(mm/2))))
      crd <- rbind(Act, Evt1, Evt2)
      crd[which(is.nan(crd))] <- 0.5
      rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
    } else if (center == "events") {
      Act1 <- cbind(rep(0, ceiling(nrow(net)/2)), nrm(rng(ceiling(nn/2))))
      Act2 <- cbind(rep(2, floor(nrow(net)/2)), nrm(rng(floor(nn/2))))
      Evt <- cbind(rep(1, ncol(net)), nrm(rng(mm)))
      crd <- rbind(Act1, Act2, Evt)
      crd[which(is.nan(crd))] <- 0.5
      rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
    } else {
      if (center %in% node_names(.data)) {
        side1 <- suppressWarnings(cbind(rep(0, nrow(net)), nrm(rng(nn))))
        side2 <- suppressWarnings(cbind(rep(2, ncol(net)), nrm(rng(mm))))
        if (any(rownames(net) == center)) {
          side1[,1] <- ifelse(rownames(net) == center, 1, side1[,1])
          side1[,2] <- ifelse(rownames(net) == center, 0.5, side1[,2])
        } else {
          side2[,1] <- ifelse(rownames(net) == center, 1, side2[,1])
          side2[,2] <- ifelse(rownames(net) == center, 0.5, side2[,2])
        }
        crd <- rbind(side1, side2)
        crd[which(is.nan(crd))] <- 0.5
        rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
      } else cli::cli_abort("Please declare actors, events, or a node name as center.")
    }
    out <- .to_lo(crd)
  }
  out
}

#' @rdname map_layout_partition
#' @examples
#' #graphr(ison_southern_women, layout = "alluvial")
#' @export
layout_tbl_graph_alluvial <- function(.data,
                                      circular = FALSE, times = 1000){
  thisRequiresBio("Rgraphviz")
  prep <- as_matrix(.data, twomode = FALSE)
    if(anyDuplicated(rownames(prep))){
      rownames(prep) <- seq_len(nrow(prep))
      colnames(prep) <- seq_len(ncol(prep))
    }
    if(any(prep<0)) prep[prep<0] <- 0
    out <- as_graphAM(prep)
    out <- suppressMessages(Rgraphviz::layoutGraph(out, layoutType = 'dot',
                                                   attrs = list(graph = list(rankdir = "LR"))))
    nodeX <- .rescale(out@renderInfo@nodes$nodeX)
    nodeY <- .rescale(out@renderInfo@nodes$nodeY)
    # nodeY <- abs(nodeY - max(nodeY))
    .to_lo(cbind(nodeX, nodeY))  
}

#' @rdname map_layout_partition
#' @export
layout_tbl_graph_railway <- function(.data,
                                     circular = FALSE, times = 1000) {
  res <- layout_tbl_graph_hierarchy(as_igraph(.data))
  res$x <- c(match(res[res[,2]==0,1], sort(res[res[,2]==0,1])),
             match(res[res[,2]==1,1], sort(res[res[,2]==1,1])))
  res
}

#' @rdname map_layout_partition
#' @export
layout_tbl_graph_ladder <- function(.data,
                                    circular = FALSE, times = 1000){
  res <- layout_tbl_graph_alluvial(as_igraph(.data))
  res$y <- c(match(res[res[,2]==1,1], sort(res[res[,2]==1,1])),
             match(res[res[,2]==0,1], sort(res[res[,2]==0,1])))
  res
}

#' @rdname map_layout_partition
#' @examples
#' #graphr(ison_southern_women, layout = "concentric", membership = "type",
#' #           node_color = "type", node_size = 3)
#' @export
layout_tbl_graph_concentric <- function(.data, membership,
                                        radius = NULL, 
                                        order.by = NULL, 
                                        circular = FALSE, times = 1000) {
  if (any(igraph::vertex_attr(.data, "name") == "")) {
    ll <- unlist(lapply(seq_len(length(.data)), function(x) {
      ifelse(igraph::vertex_attr(.data, "name")[x] == "",
             paste0("ramdom", x), igraph::vertex_attr(.data, "name")[x])
    }))
    .data <- set_vertex_attr(.data, "name", value = ll)
  }
  if (missing(membership)) { 
    if (is_twomode(.data)) membership <- node_is_mode(.data) else 
      cli::cli_abort("Please pass the function a `membership` node attribute or a vector.")
  } else {
    if (length(membership) > 1 & length(membership) != length(.data)) {
      cli::cli_abort("Please pass the function a `membership` node attribute or a vector.")
    } else if (length(membership) != length(.data)) {
      membership <- node_attribute(.data, membership)
    }
  }
  names(membership) <- node_names(.data)
  membership <- to_list(membership)
  all_c  <- unlist(membership, use.names = FALSE)
  if (any(table(all_c) > 1)) cli::cli_abort("Duplicated nodes in layers!")
  if (is_labelled(.data)) all_n <- node_names(.data) else all_n <- 1:net_nodes(.data)
  sel_other  <- all_n[!all_n %in% all_c]
  if (length(sel_other) > 0) membership[[length(membership) + 1]] <- sel_other
  if (is.null(radius)) {
    radius <- seq(0, 1, 1/(length(membership)))
    if (length(membership[[1]]) == 1) 
      radius <- radius[-length(radius)] else radius <- radius[-1]
  }
  if (!is.null(order.by)) {
    order.values <- lapply(order.by, 
                           function(b) node_attribute(.data, b))
  } else {
    if (is_twomode(.data) & length(membership) == 2) {
      xnet <- as_matrix(to_multilevel(.data))[membership[[2-1]], 
                                              membership[[2]]]
      lo <- layout_tbl_graph_hierarchy(as_igraph(xnet, twomode = TRUE))
      lo$names <- node_names(.data)
      if (ncol(lo) == 2) lo[,1] <- seq_len(dim(lo)[1])
      order.values <- lapply(1:0, function(x)
        if(ncol(lo) >= 3) sort(lo[lo[,2] == x,])[,3] 
        else sort(lo[lo[,2] == x,1])) 
    } else order.values <- membership[order(sapply(membership, length))]
    # order.values <- getNNvec(.data, members)
  }
  res <- matrix(NA, nrow = length(all_n), ncol = 2)
  for (k in seq_along(membership)) {
    r <- radius[k]
    l <- order.values[[k]]
    if(is_labelled(.data))
      l <- match(l, node_names(.data))
    res[l, ] <- getCoordinates(l, r)
  }
  .to_lo(res)
}

#' @rdname map_layout_partition
#' @examples
#' #graphr(ison_lotr, layout = "multilevel",
#' #           node_color = "Race", level = "Race", node_size = 3)
#' @export
layout_tbl_graph_multilevel <- function(.data, level, circular = FALSE) {
  if (missing(level)) {
    if (any(grepl("lvl", names(node_attribute(.data))))) {
      message("Level attribute 'lvl' found in data.")
      } else {
        cli::cli_abort("Please pass the function a `level` node attribute or a vector.")
      }
  } else {
    if (length(level) > 1 & length(level) != length(.data)) {
      cli::cli_abort("Please pass the function a `level` node attribute or a vector.")
    } else if (length(level) != length(.data)) {
      level <- as.factor(node_attribute(.data, level))
    }
  }
  out <- igraph::set_vertex_attr(.data, "lvl", value = level)
  thisRequires("graphlayouts")
  out <- graphlayouts::layout_as_multilevel(out, alpha = 25)
  .to_lo(out)
}

#' @rdname map_layout_partition
#' @examples
#' # ison_adolescents %>%
#' #   mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2),
#' #          cut = node_is_cutpoint(ison_adolescents)) %>%
#' #   graphr(layout = "lineage", rank = "year", node_color = "cut",
#' #              node_size = migraph::node_degree(ison_adolescents)*10)
#' @export
layout_tbl_graph_lineage <- function(.data, rank, circular = FALSE) {
  if (length(rank) > 1 & length(rank) != length(.data)) {
    cli::cli_abort("Please pass the function a `rank` node attribute or a vector.")
  } else if (length(rank) != length(.data)) {
    rank <- as.numeric(node_attribute(.data, rank))
  }
  thisRequiresBio("Rgraphviz")
  out <- layout_tbl_graph_alluvial(
    as_igraph(mutate(.data, type = ifelse(
      rank > mean(rank), TRUE, FALSE)), twomode = TRUE))
  out$x <- .rescale(rank)
  .check_dup(out)
}

.rescale <- function(vector){
  (vector - min(vector)) / (max(vector) - min(vector))
}

.to_lo <- function(mat) {
  res <- as.data.frame(mat)
  names(res) <- c("x","y")
  res
}

to_list <- function(members) {
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

.check_dup <- function(mat) {
  mat$y <- ifelse(duplicated(mat[c('x','y')]), mat$y*0.95, mat$y)
  mat
}

#' @importFrom igraph degree
getNNvec <- function(.data, members){
  lapply(members, function(circle){
    diss <- 1 - stats::cor(to_multilevel(as_matrix(.data))[, circle])
    diag(diss) <- NA
    if(is_labelled(.data))
      starts <- names(sort(igraph::degree(.data)[circle], decreasing = TRUE)[1])
    else starts <- paste0("V",1:net_nodes(.data))[sort(igraph::degree(.data)[circle], 
                                                           decreasing = TRUE)[1]]
    if(length(circle)>1)
      starts <- c(starts, names(which.min(diss[starts,])))
    out <- starts
    if(length(circle)>2){
      for(i in 1:(length(circle)-2)){
        diss <- diss[,!colnames(diss) %in% starts]
        if(is.matrix(diss)){
          side <- names(which.min(apply(diss[starts,], 1, min, na.rm = TRUE)))
          new <- names(which.min(diss[side,]))
        } else {
          side <- names(which.min(diss[starts]))
          new <- setdiff(circle,out)
        }
        if(side == out[1]){
          out <- c(new, out)
          starts <- c(new, starts[2])
        } else {
          out <- c(out, new)
          starts <- c(starts[1], new)
        }
      }
    }
    out
  })
}

getCoordinates <- function(x, r) {
  l <- length(x)
  d <- 360/l
  c1 <- seq(0, 360, d)
  c1 <- c1[1:(length(c1) - 1)]
  tmp <- t(vapply(c1, 
                  function(cc) c(cos(cc * pi/180) * 
                                   r, sin(cc *
                                            pi/180) * r),
                  FUN.VALUE = numeric(2)))
  rownames(tmp) <- x
  tmp
}

rng <- function(r) {
  if (r == 1L) return(0)
  if (r > 1L) {
    x <- vector()
    x <- append(x, (-1))
    for (i in 1:(r - 1)) x <- append(x, ((-1) + (2L/(r - 1L)) * i))
    return(x * (r/50L))
  } else cli::cli_abort("no negative values")
}

nrm <- function(x, digits = 3) {
  if (isTRUE(length(x) == 1L) == TRUE) return(x)
  if (is.array(x) == TRUE) {
    xnorm <- (x[, 1] - min(x[, 1]))/(max(x[, 1]) - min(x[, 1]))
    rat <- (max(x[, 1]) - min(x[, 1]))/(max(x[, 2]) - min(x[, 2]))
    ynorm <- ((x[, 2] - min(x[, 2]))/(max(x[, 2]) - min(x[, 2]))) * (rat)
    ifelse(isTRUE(rat > 0) == FALSE,
           ynorm <- ((x[, 2] - min(x[, 2]))/(max(x[, 2]) -
                                               min(x[, 2]))) * (1L/rat), NA)
    return(round(data.frame(X = xnorm, Y = ynorm), digits))
  }
  else if (is.vector(x) == TRUE) {
    return(round((x - min(x))/(max(x) - min(x)), digits))
  }
}

# Grid ####

depth_first_recursive_search <- function(layout) {
  if("ggraph" %in% class(layout)) layout <- layout$data[,c("x","y")]
  layout <- as.data.frame(layout)
  dims <- ceiling(2 * sqrt(nrow(layout)))
  # evens <- 0:dims[0:dims %% 2 == 0]
  vacant_points <- expand.grid(seq.int(0, dims, 1), seq.int(0, dims, 1)) # create options
  vacant_points <- vacant_points - floor(dims / 2) # centre options
  names(vacant_points) <- c("x", "y")
  gridout <- layout[order(abs(layout[,1]) + abs(layout[,2])), ] # sort centroid distance
  nodes <- seq_len(nrow(gridout))
  for (i in nodes) {
    dists <- as.matrix(dist(rbind(gridout[i, 1:2], vacant_points),
                            method = "manhattan"))[, 1]
    mindist <- which(dists == min(dists[2:length(dists)]))[1] - 1
    vacpoint <- vacant_points[mindist, ]
    changes <- vacpoint - gridout[i, 1:2]
    gridout[nodes >= i, 1] <- gridout[nodes >= i, 1] + 
      changes[[1]]
    gridout[nodes >= i, 2] <- gridout[nodes >= i, 2] + 
      changes[[2]]
    vacant_points <- vacant_points[-mindist, ]
  }
  gridout[order(row.names(gridout)),] # reorder from centroid
  # gridout
  # plot(gridout[order(row.names(gridout)),])
}

# localmin <- function(layout, graph) {
#   repeat {
#     f0 <- sum(cost_function(layout, graph))
#     L <- get_vacant_points(layout)
#     for (a in seq_len(nrow(layout))) {
#       out <- t(apply(L, 1, function(y) {
#         layout_new <- layout
#         layout_new[a, 1:2] <- y
#         c(a, y, sum(cost_function(layout_new, graph)))
#       }))
#     }
#     if (out[which.min(out[, 4]), 4] < f0) {
#       layout[out[which.min(out[, 4]), 1], 1:2] <- out[which.min(out[, 4]), 2:3]
#     } else{
#       break
#     }
#   }
#   layout
# }
# 
# get_vacant_points <- function(layout) {
#   all_points <- expand.grid(min(layout$x):max(layout$x),
#                             min(layout$y):max(layout$y))
#   names(all_points) <- c("x", "y")
#   vacant_points <- rbind(all_points,
#                          layout[, c("x", "y")])
#   vacant_points <- subset(vacant_points,
#                           !(duplicated(vacant_points) |
#                               duplicated(vacant_points, fromLast = TRUE)))
#   vacant_points
# }
# 
# cost_function <- function(layout, graph, max_repulse_distance = max(layout[, 1]) * .75) {
#   d <- as.matrix(dist(layout[, 1:2], method = "manhattan"))
#   a <- as_matrix(graph)
#   i <- diag(nrow(a))
#   m <- a + i
#   w <- ifelse(m > 0, 3,
#               ifelse(m == 0 & m %*% t(m) > 0, 0, -2)) # only three levels here
#   # see Li and Kurata (2005: 2037) for more granulated option
#   ifelse(w >= 0, w * d, w * min(d, max_repulse_distance))
# }
# 
# plot_gl <- function(x, tmax, tmin, rmin, fmin, ne, rc, p) {
#   l <- index <- a <- NULL # initialize variables to avoid CMD check notes
#   x <- as_tidygraph(x)
#   lo <- ggraph::create_layout(x, layout = "igraph", algorithm = "randomly")
#   lo[, 1] <- round(lo[, 1] * 1000)
#   lo[, 2] <- round(lo[, 2] * 1000)
#   dists <- as.matrix(dist(lo[, 1:2], method = "manhattan"))
#   colMax <- function(data) apply(data, MARGIN = 1, FUN = max, na.rm = TRUE)
#   diag(dists) <- NA
#   rsep <- l * sum(ifelse(colMax(a / dists - 1) > 0, colMax(a / dists - 1), 0))
#   ggraph::ggraph(x, graph = lo) +
#     ggraph::geom_edge_link(ggplot2::aes(alpha = ggplot2::stat(index)), 
#                            show.legend = FALSE) +
#     ggraph::geom_node_point()
# }

