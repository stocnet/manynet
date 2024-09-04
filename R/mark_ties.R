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
#' tie_is_triangular(ison_monastery_like)
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
  # out <- as.data.frame(out)
  out <- rbind(out[,c(1,2)],out[,c(2,3)],out[,c(3,1)],
               out[,c(1,3)],out[,c(3,2)],out[,c(2,1)])
  out
}

#' @rdname mark_triangles
#' @examples 
#' ison_adolescents %>% to_directed() %>% 
#'   mutate_ties(trans = tie_is_transitive()) %>% 
#'   graphr(edge_color = "trans")
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
#'   mutate_ties(cyc = tie_is_cyclical()) %>% 
#'   graphr(edge_color = "cyc")
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
#' ison_monastery_like %>% 
#'   mutate_ties(simmel = tie_is_simmelian()) %>% 
#'   graphr(edge_color = "simmel")
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
