#' Layout algorithms based on bi- or other partitions
#' 
#' @description
#'   These algorithms layout networks based on two or more partitions,
#'   and are recommended for use with `autographr()` or `{ggraph}`.
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
#' @name partition_layouts
#' @inheritParams transform
#' @param circular Should the layout be transformed into a radial representation. 
#' Only possible for some layouts. Defaults to FALSE
#' @param times Maximum number of iterations, where appropriate
#' @param radius A vector of radii at which the concentric circles
#'   should be located.
#'   By default this is equal placement around an empty centre, 
#'   unless one (the core) is a single node,
#'   in which case this node occupies the centre of the graph.
#' @param order.by An attribute label indicating the (decreasing) order
#'   for the nodes around the circles. 
#'   By default ordering is given by a bipartite placement that reduces
#'   the number of edge crossings.
#' @family mapping
#' @source
#'   Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#'   "Systematic identification of transcriptional regulatory modules from
#'   protein-protein interaction networks". 
#'   _Nucleic Acids Research_, 42 (1) e6.
NULL

#' @rdname partition_layouts
#' @export
layout_tbl_graph_hierarchy <- function(.data,
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
                                                 attrs = list(graph = list(rankdir = "BT"))))
  nodeX <- .rescale(out@renderInfo@nodes$nodeX)
  nodeY <- .rescale(out@renderInfo@nodes$nodeY)
  # nodeY <- abs(nodeY - max(nodeY))
  .to_lo(cbind(nodeX, nodeY))
}

#' @rdname partition_layouts
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

#' @rdname partition_layouts
#' @export
layout_tbl_graph_railway <- function(.data,
                                     circular = FALSE, times = 1000){
  res <- layout_tbl_graph_hierarchy(as_igraph(.data))
  res$x <- c(match(res[res[,2]==0,1], sort(res[res[,2]==0,1])),
             match(res[res[,2]==1,1], sort(res[res[,2]==1,1])))
  res
}

#' @rdname partition_layouts
#' @export
layout_tbl_graph_ladder <- function(.data,
                                    circular = FALSE, times = 1000){
  res <- layout_tbl_graph_alluvial(as_igraph(.data))
  res$y <- c(match(res[res[,2]==1,1], sort(res[res[,2]==1,1])),
             match(res[res[,2]==0,1], sort(res[res[,2]==0,1])))
  res
}

#' @rdname partition_layouts
#' @export
layout_tbl_graph_concentric <- function(.data, membership = NULL, radius = NULL, 
                                        order.by = NULL, 
                                        circular = FALSE, times = 1000) {
  if (is.null(membership)) { 
    if (is_twomode(.data)) membership <- node_mode(.data) else 
      stop("Please pass the function a `membership` node attribute.")
  } else {
    membership <- node_attribute(.data, membership)
  }
  names(membership) <- node_names(.data)
  membership <- to_list(membership)
  all_c  <- unlist(membership, use.names = FALSE)
  if (any(table(all_c) > 1)) stop("Duplicated nodes in layers!")
  if (is_labelled(.data)) all_n <- node_names(.data) else all_n <- 1:network_nodes(.data)
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
    if (is_twomode(.data)) {
      for(k in 2:length(membership)) {
      xnet <- as_matrix(to_multilevel(.data))[membership[[k-1]], 
                                              membership[[k]]]
      lo <- layout_tbl_graph_hierarchy(as_igraph(xnet, twomode = TRUE))
      lo$names <- node_names(.data)
      if (ncol(lo) == 2) lo[,1] <- seq_len(lo)
      order.values <- lapply(1:0, function(x)
        if(ncol(lo) >= 3) sort(lo[lo[,2] == x,])[,3] 
        else sort(lo[lo[,2] == x,1]) ) 
      } 
    } else order.values <- membership
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

.rescale <- function(vector){
  (vector - min(vector)) / (max(vector) - min(vector))
}

.to_lo <- function(mat){
  res <- as.data.frame(mat)
  names(res) <- c("x","y")
  res
}

to_list <- function(members){
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

#' @importFrom igraph degree
getNNvec <- function(.data, members){
  lapply(members, function(circle){
    diss <- 1 - stats::cor(to_multilevel(as_matrix(.data))[, circle])
    diag(diss) <- NA
    if(is_labelled(.data))
      starts <- names(sort(igraph::degree(.data)[circle], decreasing = TRUE)[1])
    else starts <- paste0("V",1:network_nodes(.data))[sort(igraph::degree(.data)[circle], 
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

getCoordinates <- function(x, r){
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
