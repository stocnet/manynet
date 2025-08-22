# Structural properties ####

#' Marking ties based on structural properties
#' 
#' @description 
#'   These functions return logical vectors the length of the ties
#'   in a network identifying which hold certain properties or positions in the network.
#'   
#'   - `tie_is_multiple()` marks ties that are multiples.
#'   - `tie_is_loop()` marks ties that are loops.
#'   - `tie_is_reciprocated()` marks ties that are mutual/reciprocated.
#'   - `tie_is_feedback()` marks ties that are feedback arcs causing the network to not be acyclic.
#'   - `tie_is_bridge()` marks ties that cut or act as articulation points in a network.
#'   - `tie_is_path()` marks ties on a path from one node to another.
#'   
#'   They are most useful in highlighting parts of the network that
#'   are particularly well- or poorly-connected.
#' @inheritParams mark_nodes
#' @family marks
#' @name mark_ties
NULL

#' @rdname mark_ties
#' @importFrom igraph which_multiple
#' @examples 
#' tie_is_multiple(ison_marvel_relationships)
#' @export
tie_is_multiple <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  make_tie_mark(igraph::which_multiple(manynet::as_igraph(.data)), .data)
}

#' @rdname mark_ties
#' @importFrom igraph which_loop
#' @examples 
#' tie_is_loop(ison_marvel_relationships)
#' @export
tie_is_loop <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  make_tie_mark(igraph::which_loop(manynet::as_igraph(.data)), .data)
}

#' @rdname mark_ties
#' @importFrom igraph which_mutual
#' @examples 
#' tie_is_reciprocated(ison_algebra)
#' @export
tie_is_reciprocated <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  make_tie_mark(igraph::which_mutual(manynet::as_igraph(.data)), .data)
}

#' @rdname mark_ties
#' @importFrom igraph feedback_arc_set
#' @examples 
#' tie_is_feedback(ison_algebra)
#' @export
tie_is_feedback <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  .data <- manynet::as_igraph(.data)
  make_tie_mark(igraph::E(.data) %in% igraph::feedback_arc_set(.data), 
                .data)
}

#' @rdname mark_ties
#' @importFrom igraph decompose delete_edges
#' @examples 
#' tie_is_bridge(ison_brandes)
#' @export
tie_is_bridge <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  num_comp <- length( igraph::decompose(manynet::as_igraph(.data)) )
  out <- vapply(seq_len(manynet::net_ties(.data)), function(x){
    length( igraph::decompose(igraph::delete_edges(.data, x)) ) > num_comp
  }, FUN.VALUE = logical(1))
  if(manynet::is_labelled(.data)) 
    names(out) <- attr(igraph::E(.data), "vnames")
  make_tie_mark(out, .data)
}

#' @rdname mark_ties
#' @inheritParams manip_paths
#' @param to The index or name of the node to which the path should be traced.
#' @param all_paths Whether to return a list of paths or sample just one.
#'   By default FALSE, sampling just a single path.
#' @importFrom igraph all_shortest_paths
#' @examples 
#' ison_adolescents %>%
#'   mutate_ties(route = tie_is_path(from = "Jane", to = 7)) 
#'   #graphr(edge_colour = "route")
#' @export
tie_is_path <- function(.data, from, to, all_paths = FALSE){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  out <- igraph::all_shortest_paths(.data, from = from, to = to,
                                     mode = "out")$epath
  if(all_paths){
    out <- igraph::E(.data) %in% unique(unlist(out))
  } else out <- igraph::E(.data) %in% out[[sample(length(out),1)]]
  make_tie_mark(out, .data)
}

# Triangular properties ####

#' Marking ties based on structural properties
#' 
#' @description 
#'   These functions return logical vectors the length of the ties
#'   in a network identifying which hold certain properties or positions in the network.
#'   
#'   - `tie_is_triangular()` marks ties that are in triangles.
#'   - `tie_is_cyclical()` marks ties that are in cycles.
#'   - `tie_is_transitive()` marks ties that complete transitive closure.
#'   - `tie_is_triplet()` marks ties that are in a transitive triplet.
#'   - `tie_is_simmelian()` marks ties that are both in a triangle 
#'   and fully reciprocated.
#'   
#'   They are most useful in highlighting parts of the network that
#'   are cohesively connected.
#' @inheritParams mark_nodes
#' @family marks
#' @name mark_triangles
NULL

#' @rdname mark_triangles
#' @importFrom igraph triangles
#' @examples 
#' ison_monks %>% to_uniplex("like") %>% 
#'   mutate_ties(tri = tie_is_triangular())
#'   #graphr(edge_color = "tri")
#' @export
tie_is_triangular <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  out <- .triangle_ties(.data)
  ties <- as_edgelist(to_unnamed(.data))[,c("from","to")]
  out <- do.call(paste, ties) %in% do.call(paste, as.data.frame(out))
  make_tie_mark(out, .data)
}

.triangle_ties <- function(.data){
  out <- t(matrix(igraph::triangles(as_igraph(.data)), nrow = 3))
  out <- rbind(out[,c(1,2)],out[,c(2,3)],out[,c(3,1)],
               out[,c(1,3)],out[,c(3,2)],out[,c(2,1)])
  out
}

#' @rdname mark_triangles
#' @examples 
#' ison_adolescents %>% to_directed() %>% 
#'   mutate_ties(trans = tie_is_transitive())
#'   #graphr(edge_color = "trans")
#' @export
tie_is_transitive <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  nodes <- as_edgelist(to_unnamed(.data))
  out <- vapply(seq_len(net_ties(.data)), function(x){
    igraph::distances(delete_ties(.data, x), 
                      v = nodes[x,1], to = nodes[x,2], 
                      mode = "out") == 2
  }, FUN.VALUE = logical(1))
  make_tie_mark(out, .data)
}

#' @rdname mark_triangles
#' @examples 
#' ison_adolescents %>% to_directed() %>% 
#'   mutate_ties(trip = tie_is_triplet())
#'   #graphr(edge_color = "trip")
#' @export
tie_is_triplet <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  nodes <- as_edgelist(to_unnamed(.data))
  trans <- tie_is_transitive(.data)
  altpath <- unlist(lapply(which(trans), function(x){
    tie_is_path(delete_ties(.data, x),
                      from = nodes[x,1], to = nodes[x,2],
                      all_paths = TRUE)
  }))
  if(!is.null(names(altpath)))
    names(altpath) <- gsub("^.*\\.", "", names(altpath))
  altpath <- altpath[altpath]
  trans[names(trans) %in% names(altpath)] <- TRUE
  make_tie_mark(trans, .data)
}

#' @rdname mark_triangles
#' @examples 
#' ison_adolescents %>% to_directed() %>% 
#'   mutate_ties(cyc = tie_is_cyclical())
#'   #graphr(edge_color = "cyc")
#' @export
tie_is_cyclical <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  out <- vapply(seq_len(net_ties(.data)), function(x){
    nodes <- as_edgelist(to_unnamed(.data))[x,]
    igraph::distances(delete_ties(.data, x), 
                      v = nodes[2], to = nodes[1], 
                      mode = "out") == 2
  }, FUN.VALUE = logical(1))
  make_tie_mark(out, .data)
}

#' @rdname mark_triangles
#' @examples 
#' ison_monks %>% to_uniplex("like") %>% 
#'   mutate_ties(simmel = tie_is_simmelian())
#'   #graphr(edge_color = "simmel")
#' @export
tie_is_simmelian <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  recip <- filter_ties(.data, tie_is_reciprocated())
  simmel <- filter_ties(recip, tie_is_triangular())
  ties <- as_edgelist(to_unnamed(.data))[,c("from","to")]
  simmel <- as_edgelist(to_unnamed(simmel))[,c("from","to")]
  out <- do.call(paste, ties) %in% do.call(paste, simmel)
  make_tie_mark(out, .data)
}

#' @rdname mark_triangles
#' @examples 
#' generate_random(8, directed = TRUE) %>% 
#'   mutate_ties(forbid = tie_is_forbidden())
#'   #graphr(edge_color = "forbid")
#' @export
tie_is_forbidden <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  dists <- igraph::distances(.data, mode = "out")==2
  ends <- which(dists * t(dists)==1, arr.ind = TRUE)
  ends <- t(apply(ends, 1, function(x) sort(x)))
  ends <- ends[!duplicated(ends),]
  tris <- apply(ends, 1, function(x){
    y <- unlist(igraph::all_shortest_paths(.data, x[1], x[2], mode = "out")$res)
    y <- matrix(y, ncol = 3, byrow = TRUE)
    y <- do.call("paste", c(as.data.frame(y)[, , drop = FALSE], sep = "-"))
    z <- unlist(igraph::all_shortest_paths(.data, x[1], x[2], mode = "in")$res)
    z <- matrix(z, ncol = 3, byrow = TRUE)
    z <- do.call("paste", c(as.data.frame(z)[, , drop = FALSE], sep = "-"))
    as.numeric(unlist(strsplit(y[y %in% z], "-")))
  })
  out <- matrix(unlist(tris), ncol = 3, byrow = TRUE)
  out <- unique(c(apply(out, 1, function(x){
    c(paste0(x[1],"->",x[2]),
    paste0(x[2],"->",x[1]),
    paste0(x[2],"->",x[3]),
    paste0(x[3],"->",x[2]))
  } )))
  out <- names(tie_is_reciprocated(.data)) %in% out
  make_tie_mark(out, .data)
}

#' @rdname mark_triangles
#' @examples
#' tie_is_imbalanced(ison_marvel_relationships)
#' @export
tie_is_imbalanced <- function(.data){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  
  # identify_imbalanced_ties <- function(adj_matrix) {
  adj_matrix <- as_matrix(.data)
  
  # Check if the input is a square matrix
  if (!is.matrix(adj_matrix) || nrow(adj_matrix) != ncol(adj_matrix)) {
    snet_unavailable("This function only works with undirected one-mode networks.")
  }
  
  # Get the number of nodes
  n <- net_nodes(.data)
  
  # Identify all edges in the upper triangle (non-zero)
  edges <- as_edgelist(to_unnamed(.data))
  num_edges <- nrow(edges)
  
  # Initialize a logical vector to store the result for each edge
  is_imbalanced <- logical(num_edges)
  
  # Loop over each edge to check if it participates in an imbalanced triad
  for (e in 1:num_edges) {
    i <- unname(unlist(edges[e, 1]))
    j <- unname(unlist(edges[e, 2]))
    
    # Variable to track if the current edge is part of any imbalanced triad
    imbalanced_found <- FALSE
    
    # Check all possible third nodes k to form a triad (i, j, k)
    for (k in setdiff(1:n, c(i, j))) {
      # Get the ties for the triad (i, j, k)
      a <- adj_matrix[i, j]
      b <- adj_matrix[j, k]
      c <- adj_matrix[i, k]
      
      # Check if the triad is complete (no zeros)
      if (b != 0 && c != 0) {
        # Count the number of positive ties in the triad
        positive_count <- sum(c(a, b, c) > 0)
        
        # Determine if the triad is imbalanced
        if (positive_count == 2 || positive_count == 0) {
          # Mark as imbalanced and exit loop for this edge
          imbalanced_found <- TRUE
          break
        }
      }
    }
    
    # Store the result for the current edge
    is_imbalanced[e] <- imbalanced_found
  }
  
  make_tie_mark(is_imbalanced, .data)
}

# Selection properties ####

#' Marking ties for selection based on measures
#' 
#' @description 
#'   These functions return logical vectors the length of the ties in a network:
#'   
#'   - `tie_is_random()` marks one or more ties at random.
#'   - `tie_is_max()` and `tie_is_min()` are more useful
#'   for converting the results from some tie measure into a mark-class object.
#'   They can be particularly useful for highlighting which tie or ties
#'   are key because they minimise or, more often, maximise some measure.
#' @inheritParams mark_select
#' @family marks
#' @name mark_tie_select
NULL

#' @rdname mark_tie_select
#' @export
tie_is_random <- function(.data, size = 1){
  if(missing(.data)) {expect_edges(); .data <- .G()}
  n <- manynet::net_ties(.data)
  out <- rep(FALSE, n)
  out[sample.int(n, size)] <- TRUE
  make_node_mark(out, .data)
}

#' @rdname mark_tie_select
#' @param tie_measure An object created by a `tie_` measure.
#' @examples 
#' # tie_is_max(migraph::tie_betweenness(ison_brandes))
#' @export
tie_is_max <- function(tie_measure){
  out <- as.numeric(tie_measure) == max(as.numeric(tie_measure))
  class(out) <- c("tie_mark", class(out))
  out
}

#' @rdname mark_tie_select
#' @examples 
#' #tie_is_min(migraph::tie_betweenness(ison_brandes))
#' @export
tie_is_min <- function(tie_measure){
  out <- as.numeric(tie_measure) == min(as.numeric(tie_measure))
  class(out) <- c("tie_mark", class(out))
  out
}
