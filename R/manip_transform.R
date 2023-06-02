# Transforming ####

#' Tools for transforming networks, graphs, and matrices
#' 
#' @description
#' These functions offer tools for transforming migraph-consistent objects
#' (matrices, igraph, tidygraph, or network objects).
#' Transforming means that the returned object may have different dimensions
#' than the original object.
#' @details
#' Since some modifications are easier to implement for some objects than others,
#' here are the currently implemented modifications:
#' 
#' |  to_      | edgelists | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | mode1 | X | X | X | X | X |
#' | mode2 | X | X | X | X | X |
#' | giant  | X | X | X | X | X |
#' | subgraph  | X | X | X | X | X |
#' | ties  | X | X | X | X | X |
#' | blocks  | X | X | X | X | X |
#' | matching | X | X | X | X | X |
#' @name transform
#' @family manipulations
#' @inheritParams reformat
NULL

#' @describeIn transform Results in a weighted one-mode object
#'   that retains the row nodes from a two-mode object,
#'   and weights the ties between them on the basis of
#'   their joint ties to nodes in the second mode (columns)
#' @param similarity Method for establishing ties,
#'   currently "count" (default), "jaccard", or "rand".
#'   "count" calculates the number of coinciding ties,
#'   and can be interpreted as indicating the degree of opportunities
#'   between nodes.
#'   "jaccard" uses this count as the numerator in a proportion,
#'   where the denominator consists of any cell where either node has a tie.
#'   It can be interpreted as opportunity weighted by participation.
#'   "rand", or the Simple Matching Coefficient,
#'   is a proportion where the numerator consists of the count of cells where
#'   both nodes are present or both are absent,
#'   over all possible cells.
#'   It can be interpreted as the (weighted) degree of behavioral mirroring
#'   between two nodes.
#'   "pearson" (Pearson's coefficient) and "yule" (Yule's Q)
#'   produce correlations for valued and binary data, respectively.
#'   Note that Yule's Q has a straightforward interpretation related to the odds ratio.
#' @importFrom igraph bipartite.projection
#' @examples
#' autographr(ison_southern_women) /
#' (autographr(to_mode1(ison_southern_women)) |
#' autographr(to_mode2(ison_southern_women)))
#' @export
to_mode1 <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode1")

#' @export
to_mode1.matrix <- function(object, 
                            similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- object %*% t(object)
  b <- object %*% (1 - t(object))
  c <- (1 - object) %*% t(object)
  d <- ncol(object) - a - b - c
  out <- switch(similarity,
         "count" = a,
         "jaccard" = a/(a + b + c),
         "rand" = (a + d)/(a + b + c + d),
         "sokalsneath1" = a/(a + 2 * (b + c)),
         "sokalsneath2" = a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),
         "gowerlegendre" = (a - (b + c) + d)/(a + b + c + d),
         "rogerstanimoto" = (a + d)/(a + 2 * (b + c) + d),
         "czekanowski" = 2*a/(2 * a + b + c),
         "ochiai" = a/sqrt((a+b)*(a+c)),
         "pearson" = cor(t(object)),
         "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode1.igraph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite.projection(object)$proj1
  else as_igraph(to_mode1(as_matrix(object), similarity))
}

#' @export
to_mode1.tbl_graph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_tidygraph(to_mode1(as_igraph(object), similarity = similarity))
}

#' @export
to_mode1.network <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode1(as_matrix(object), similarity = similarity))
}

#' @export
to_mode1.data.frame <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_edgelist(to_mode1(as_matrix(object), similarity = similarity))
}

#' @describeIn transform Results in a weighted one-mode object
#' that retains the column nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the first mode (rows).
#' @export
to_mode2 <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode2")

#' @export
to_mode2.matrix <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- t(object) %*% object
  b <- t(object) %*% (1 - object)
  c <- (1 - t(object)) %*% object
  d <- nrow(object) - a - b - c
  out <- switch(similarity,
                "count" = a,
                "jaccard" = a/(a + b + c),
                "rand" = (a + d)/(a + b + c + d),
                "sokalsneath1" = a/(a + 2 * (b + c)),
                "sokalsneath2" = a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),
                "gowerlegendre" = (a - (b + c) + d)/(a + b + c + d),
                "rogerstanimoto" = (a + d)/(a + 2 * (b + c) + d),
                "czekanowski" = 2*a/(2 * a + b + c),
                "ochiai" = a/sqrt((a+b)*(a+c)),
                "pearson" = cor(object),
                "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode2.igraph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite.projection(object)$proj2
  else as_igraph(to_mode2(as_matrix(object), similarity))
}

#' @export
to_mode2.tbl_graph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_tidygraph(to_mode2(as_igraph(object), similarity))
}

#' @export
to_mode2.network <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode2(as_matrix(object), similarity))
}

#' @export
to_mode2.data.frame <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_edgelist(to_mode2(as_matrix(object), similarity))
}

#' @describeIn transform Returns an object that includes only the main component
#' without any smaller components or isolates
#' @export
to_giant <- function(object) UseMethod("to_giant")

#' @export
to_giant.igraph <- function(object) {
  comps <- igraph::components(object)
  max.comp <- which.max(comps$csize)
  igraph::delete.vertices(object, comps$membership != max.comp)
}

#' @export
to_giant.network <- function(object) {
  network::delete.vertices(object, 
                           which(!sna::component.largest(object,
                                                         result = "membership")))
}

#' @export
to_giant.tbl_graph <- function(object) {
  as_tidygraph(to_giant(as_igraph(object)))
}

#' @export
to_giant.data.frame <- function(object) {
  as_edgelist(to_giant(as_igraph(object)))
}

#' @export
to_giant.matrix <- function(object) {
  as_matrix(to_giant(as_igraph(object)))
}

#' @describeIn transform Returns a network subgraph filtered
#'   on the basis of some node-related logical statement.
#' @param ... Arguments passed on to dplyr::filter
#' @importFrom dplyr filter
#' @export
to_subgraph <- function(object, ...) UseMethod("to_subgraph")

#' @export
to_subgraph.tbl_graph <- function(object, ...){
  dplyr::filter(.data = object, ..., 
                .preserve = FALSE)
}

#' @export
to_subgraph.igraph <- function(object, ...){
  as_igraph(to_subgraph(as_tidygraph(object), ...))
}

#' @export
to_subgraph.network <- function(object, ...){
  as_network(to_subgraph(as_tidygraph(object), ...))
}

#' @export
to_subgraph.data.frame <- function(object, ...){
  as_edgelist(to_subgraph(as_tidygraph(object), ...))
}

#' @export
to_subgraph.matrix <- function(object, ...){
  as_matrix(to_subgraph(as_tidygraph(object), ...))
}

#' @describeIn transform Returns a matrix (named if possible) 
#'   where the edges are the nodes
#' @importFrom igraph make_line_graph E
#' @examples
#' autographr(ison_adolescents) +  
#' autographr(to_ties(ison_adolescents))
#' @export
to_ties <- function(object) UseMethod("to_ties")

#' @export
to_ties.igraph <- function(object){
  out <- igraph::make_line_graph(object)
  out <- add_node_attribute(out, "name", attr(igraph::E(object), "vnames"))
  igraph::V(out)$name <- gsub("\\|", "-", igraph::V(out)$name)
  out
}

#' @export
to_ties.tbl_graph <- function(object){
  as_tidygraph(to_ties(as_igraph(object)))
}

#' @export
to_ties.network <- function(object){
  as_network(to_ties(as_igraph(object)))
}

#' @export
to_ties.data.frame <- function(object){
  as_edgelist(to_ties(as_igraph(object)))
}

#' @export
to_ties.matrix <- function(object){
  as_matrix(to_ties(as_igraph(object)))
}

#' @describeIn transform Returns a reduced graph from a given
#'   partition membership vector.
#'   Reduced graphs provide summary representations of network structures 
#'   by collapsing groups of connected nodes into single nodes 
#'   while preserving the topology of the original structures.
#' @param membership A vector of partition memberships.
#' @param FUN A function for summarising block content.
#'   By default `mean`.
#'   Other recommended options include `median`, `sum`,
#'   `min` or `max`.
#' @examples 
#' (adolblock <- to_blocks(ison_adolescents, 
#'   node_regular_equivalence(ison_adolescents, k = 3)))
#' autographr(adolblock)
#' @export
to_blocks <- function(object, membership, FUN = mean) UseMethod("to_blocks")

#' @export
to_blocks.matrix <- function(object, membership, FUN = mean){
  if(is_twomode(object)){
    mat <- to_onemode(object)
    m1_membs <- membership[!node_mode(object)]
    m2_membs <- membership[node_mode(object)]
    x <- length(unique(m1_membs))
    y <- length(unique(m2_membs))
    out <- matrix(nrow = unique(m1_membs)[x],
                  ncol = unique(m2_membs)[y])
    for(i in unique(m1_membs)) for (j in unique(m2_membs))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(unique(m1_membs)[x]))
    colnames(out) <- paste("Block", seq_len(unique(m2_membs)[y]))
  } else {
    mat <- object
    parts <- max(membership)
    out <- matrix(nrow = parts, 
                  ncol = parts)
    for(i in seq_len(parts)) for (j in seq_len(parts))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(parts))
    colnames(out) <- paste("Block", seq_len(parts))
  }
  out[is.na(out)] <- 0
  out
}

#' @export
to_blocks.igraph <- function(object, membership, FUN = mean){
  as_igraph(to_blocks(as_matrix(object), membership, FUN))
}

#' @export
to_blocks.network <- function(object, membership, FUN = mean){
  as_network(to_blocks(as_matrix(object), membership, FUN))
}

#' @export
to_blocks.data.frame <- function(object, membership, FUN = mean){
  as_edgelist(to_blocks(as_matrix(object), membership, FUN))
}

#' @export
to_blocks.tbl_graph <- function(object, membership, FUN = mean){
  as_tidygraph(to_blocks(as_matrix(object), membership, FUN))
}

#' @describeIn transform Returns a network with only
#'   matching ties
#' @section to_matching:
#'   `to_matching()` uses `{igraph}`'s `max_bipartite_match()`
#'   to return a network in which each node is only tied to
#'   one of its previous ties.
#'   The number of these ties left is its _cardinality_,
#'   and the algorithm seeks to maximise this such that,
#'   where possible, each node will be associated with just one
#'   node in the other mode or some other mark.
#'   The algorithm used is the push-relabel algorithm
#'   with greedy initialization and a global relabelling
#'   after every \eqn{\frac{n}{2}} steps,
#'   where \eqn{n} is the number of nodes in the network.
#' @references 
#'   Goldberg, A V; Tarjan, R E (1986). 
#'   "A new approach to the maximum flow problem". 
#'   _Proceedings of the eighteenth annual ACM symposium on Theory of computing – STOC '86_. p. 136. 
#'   \doi{10.1145/12130.12144}
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph max_bipartite_match
#' @examples 
#' autographr(to_matching(ison_southern_women), "hierarchy")
#' @export
to_matching <- function(object, mark = "type") UseMethod("to_matching")

#' @export
to_matching.igraph <- function(object, mark = "type"){
  if(length(unique(node_attribute(object, mark)))>2)
    stop("This function currently only works with binary attributes.")
  el <- igraph::max_bipartite_match(object, 
                 types = node_attribute(object, mark))$matching
  el <- data.frame(from = names(el), to = el)
  out <- suppressWarnings(as_igraph(el, twomode = TRUE))
  out <- igraph::delete_vertices(out, "NA")
  out <- to_twomode(out, node_attribute(object, mark))
  out
}

#' @export
to_matching.tbl_graph <- function(object, mark = "type"){
  as_tidygraph(to_matching(as_igraph(object), mark))
}

#' @export
to_matching.network <- function(object, mark = "type"){
  as_network(to_matching(as_igraph(object), mark))
}

#' @export
to_matching.data.frame <- function(object, mark = "type"){
  as_edgelist(to_matching(as_igraph(object), mark))
}

#' @export
to_matching.matrix <- function(object, mark = "type"){
  as_matrix(to_matching(as_igraph(object), mark))
}

#' @describeIn transform Returns the complement of a network
#'   where only ties _not_ present in the original network
#'   are included in the new network.
#' @importFrom igraph complementer
#' @examples 
#' autographr(to_anti(ison_southern_women), "hierarchy")
#' @export
to_anti <- function(object) UseMethod("to_anti")

#' @export
to_anti.matrix <- function(object){
  matrix(1, nrow(object), ncol(object)) - object
}

#' @export
to_anti.data.frame <- function(object){
  as_edgelist.matrix(to_anti.matrix(as_matrix(object)))
}

#' @export
to_anti.igraph <- function(object){
  if(is_twomode(object)){
    as_igraph(to_anti.matrix(as_matrix(object)))
  } else {
    igraph::complementer(as_igraph(object), 
                         loops = is_complex(object))
  }
}

#' @export
to_anti.tbl_graph <- function(object){
  if(is_twomode(object)){
    as_tidygraph(to_anti.matrix(as_matrix(object)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(object), 
                         loops = is_complex(object)))
  }
}

#' @export
to_anti.network <- function(object){
  as_network(to_anti(as_igraph(object)))
}

#' @describeIn transform Removes all nodes without ties
#' @importFrom tidygraph node_is_isolated
#' @importFrom dplyr filter
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   to_subgraph(from == 1:5) %>%
#'   to_no_isolates()
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   to_no_isolates()
#' @export
to_no_isolates <- function(.data) {
  # Check if object is a list of lists
  if (is.list(.data[1])) {
    # Delete edges not present vertices in each list
    lapply(.data, function(x) {
      x %>% activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
    })
  } else {
    # Delete edges not present vertices
    .data %>% activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
  }
}
