# Transforming ####

#' Tools for transforming networks, graphs, and matrices
#' 
#' @description
#'   These functions offer tools for transforming migraph-consistent objects
#'   (matrices, igraph, tidygraph, or network objects).
#'   Transforming means that the returned object may have different dimensions
#'   than the original object.
#' 
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'    ```{r, echo = FALSE, cache = TRUE} 
#'  knitr::kable(available_methods(c("to_mode1", "to_mode2", 
#'     "to_giant", "to_subgraph", "to_ties", "to_blocks", 
#'     "to_matching", "to_eulerian", "to_anti", "to_no_isolates")))
#'  ```
#' @name transform
#' @family manipulations
#' @inheritParams reformat
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
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
#' @importFrom stats cor
#' @examples
#' to_mode1(ison_southern_women)
#' to_mode2(ison_southern_women)
#' #autographr(to_mode1(ison_southern_women))
#' #autographr(to_mode2(ison_southern_women))
#' @export
to_mode1 <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode1")

#' @export
to_mode1.matrix <- function(.data, 
                            similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- .data %*% t(.data)
  b <- .data %*% (1 - t(.data))
  c <- (1 - .data) %*% t(.data)
  d <- ncol(.data) - a - b - c
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
         "pearson" = stats::cor(t(.data)),
         "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode1.igraph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite.projection(.data)$proj1
  else as_igraph(to_mode1(as_matrix(.data), similarity))
}

#' @export
to_mode1.tbl_graph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_tidygraph(to_mode1(as_igraph(.data), similarity = similarity))
}

#' @export
to_mode1.network <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode1(as_matrix(.data), similarity = similarity))
}

#' @export
to_mode1.data.frame <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_edgelist(to_mode1(as_matrix(.data), similarity = similarity))
}

#' @describeIn transform Results in a weighted one-mode object
#' that retains the column nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the first mode (rows).
#' @export
to_mode2 <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode2")

#' @export
to_mode2.matrix <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- t(.data) %*% .data
  b <- t(.data) %*% (1 - .data)
  c <- (1 - t(.data)) %*% .data
  d <- nrow(.data) - a - b - c
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
                "pearson" = stats::cor(.data),
                "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode2.igraph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite.projection(.data)$proj2
  else as_igraph(to_mode2(as_matrix(.data), similarity))
}

#' @export
to_mode2.tbl_graph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_tidygraph(to_mode2(as_igraph(.data), similarity))
}

#' @export
to_mode2.network <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode2(as_matrix(.data), similarity))
}

#' @export
to_mode2.data.frame <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_edgelist(to_mode2(as_matrix(.data), similarity))
}

#' @describeIn transform Returns an object that includes only the main component
#' without any smaller components or isolates
#' @export
to_giant <- function(.data) UseMethod("to_giant")

#' @export
to_giant.igraph <- function(.data) {
  comps <- igraph::components(.data)
  max.comp <- which.max(comps$csize)
  igraph::delete.vertices(.data, comps$membership != max.comp)
}

#' @export
to_giant.network <- function(.data) {
  comps <- igraph::components(as_igraph(.data))
  network::delete.vertices(.data, 
                           which(comps$membership != which.max(comps$csize)))
}

#' @export
to_giant.tbl_graph <- function(.data) {
  as_tidygraph(to_giant(as_igraph(.data)))
}

#' @export
to_giant.data.frame <- function(.data) {
  as_edgelist(to_giant(as_igraph(.data)))
}

#' @export
to_giant.matrix <- function(.data) {
  as_matrix(to_giant(as_igraph(.data)))
}

#' @describeIn transform Returns a network subgraph filtered
#'   on the basis of some node-related logical statement.
#' @param ... Arguments passed on to dplyr::filter
#' @importFrom dplyr filter
#' @export
to_subgraph <- function(.data, ...) UseMethod("to_subgraph")

#' @export
to_subgraph.tbl_graph <- function(.data, ...){
  dplyr::filter(.data = .data, ..., 
                .preserve = FALSE)
}

#' @export
to_subgraph.igraph <- function(.data, ...){
  as_igraph(to_subgraph(as_tidygraph(.data), ...))
}

#' @export
to_subgraph.network <- function(.data, ...){
  as_network(to_subgraph(as_tidygraph(.data), ...))
}

#' @export
to_subgraph.data.frame <- function(.data, ...){
  as_edgelist(to_subgraph(as_tidygraph(.data), ...))
}

#' @export
to_subgraph.matrix <- function(.data, ...){
  as_matrix(to_subgraph(as_tidygraph(.data), ...))
}

#' @describeIn transform Returns a matrix (named if possible) 
#'   where the edges are the nodes
#' @importFrom igraph make_line_graph E
#' @examples
#' to_ties(ison_adolescents)
#' #autographr(to_ties(ison_adolescents))
#' @export
to_ties <- function(.data) UseMethod("to_ties")

#' @export
to_ties.igraph <- function(.data){
  out <- igraph::make_line_graph(.data)
  out <- add_node_attribute(out, "name", attr(igraph::E(.data), "vnames"))
  igraph::V(out)$name <- gsub("\\|", "-", igraph::V(out)$name)
  out
}

#' @export
to_ties.tbl_graph <- function(.data){
  as_tidygraph(to_ties(as_igraph(.data)))
}

#' @export
to_ties.network <- function(.data){
  as_network(to_ties(as_igraph(.data)))
}

#' @export
to_ties.data.frame <- function(.data){
  as_edgelist(to_ties(as_igraph(.data)))
}

#' @export
to_ties.matrix <- function(.data){
  as_matrix(to_ties(as_igraph(.data)))
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
#' @export
to_blocks <- function(.data, membership, FUN = mean) UseMethod("to_blocks")

#' @export
to_blocks.matrix <- function(.data, membership, FUN = mean){
  if(is_twomode(.data)){
    mat <- to_onemode(.data)
    m1_membs <- membership[!node_mode(.data)]
    m2_membs <- membership[node_mode(.data)]
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
    mat <- .data
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
to_blocks.igraph <- function(.data, membership, FUN = mean){
  as_igraph(to_blocks(as_matrix(.data), membership, FUN))
}

#' @export
to_blocks.network <- function(.data, membership, FUN = mean){
  as_network(to_blocks(as_matrix(.data), membership, FUN))
}

#' @export
to_blocks.data.frame <- function(.data, membership, FUN = mean){
  as_edgelist(to_blocks(as_matrix(.data), membership, FUN))
}

#' @export
to_blocks.tbl_graph <- function(.data, membership, FUN = mean){
  as_tidygraph(to_blocks(as_matrix(.data), membership, FUN))
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
#' to_matching(ison_southern_women)
#' #autographr(to_matching(ison_southern_women))
#' @export
to_matching <- function(.data, mark = "type") UseMethod("to_matching")

#' @export
to_matching.igraph <- function(.data, mark = "type"){
  if(length(unique(node_attribute(.data, mark)))>2)
    stop("This function currently only works with binary attributes.")
  el <- igraph::max_bipartite_match(.data, 
                 types = node_attribute(.data, mark))$matching
  el <- data.frame(from = names(el), to = el)
  out <- suppressWarnings(as_igraph(el, twomode = TRUE))
  out <- igraph::delete_vertices(out, "NA")
  out <- to_twomode(out, node_attribute(.data, mark))
  out
}

#' @export
to_matching.tbl_graph <- function(.data, mark = "type"){
  as_tidygraph(to_matching.igraph(.data), mark)
}

#' @export
to_matching.network <- function(.data, mark = "type"){
  as_network(to_matching(as_igraph(.data), mark))
}

#' @export
to_matching.data.frame <- function(.data, mark = "type"){
  as_edgelist(to_matching(as_igraph(.data), mark))
}

#' @export
to_matching.matrix <- function(.data, mark = "type"){
  as_matrix(to_matching(as_igraph(.data), mark))
}

#' @describeIn transform Returns a network where each node is
#'   connected only to their closest mentor
#' @param elites The proportion of nodes to be selected as mentors.
#'   By default this is set at 0.1.
#'   This means that the top 10% of nodes in terms of degree,
#'   or those equal to the highest rank degree in the network,
#'   whichever is the higher, will be used to select the mentors.
#'   
#'   Note that if nodes are equidistant from two mentors,
#'   they will choose one at random.
#'   If a node is without a path to a mentor,
#'   for example because they are an isolate,
#'   a tie to themselves (a loop) will be created instead.
#'   Note that this is a different default behaviour than that
#'   described in Valente and Davis (1999).
#' @references
#' Valente, Thomas, and Rebecca Davis. 1999.
#' "Accelerating the Diffusion of Innovations Using Opinion Leaders",
#' _Annals of the American Academy of Political and Social Science_ 566: 56-67.
#' @examples
#' autographr(to_mentoring(ison_adolescents))
#' @export
to_mentoring <- function(.data, elites = 0.1) UseMethod("to_mentoring")

#' @export
to_mentoring.tbl_graph <- function(.data, elites = 0.1){
  as_tidygraph(to_mentoring.igraph(.data, elites = elites))
}

#' @export
to_mentoring.igraph <- function(.data, elites = 0.1){
  md <- as_matrix(.data)
  if(!is_labelled(.data)) rownames(md) <- colnames(md) <- 1:nrow(md)
  ranks <- sort(colSums(md), decreasing = TRUE) # get rank order of indegrees
  mentors <- ranks[ranks == max(ranks)]
  if(length(mentors) < length(ranks)*elites)
    mentors <- ranks[1:(length(ranks)*elites)]
  dists <- igraph::distances(.data) # compute geodesic matrix
  if(!is_labelled(.data)) rownames(dists) <- colnames(dists) <- 1:nrow(dists)
  dists <- dists[!rownames(dists) %in% names(mentors),
                 colnames(dists) %in% names(mentors)]
  if(!is.matrix(dists)){ # if only one mentor available
    out <- dists
    out[is.infinite(out)] <- names(out[is.infinite(out)])
    # Note that unlike Valente & Davis, we do not assign an isolate a random
    # mentor, but instead assign themselves as their own mentor.
    # This results in a complex network.
    if(is.numeric(as.numeric(out))){
      names <- names(out)
      out <- as.numeric(out)
      names(out) <- names
    } 
  } else {
    out <- apply(dists, 1, # for each node, find mentor
                 function(x){
                   if(all(x == Inf)) "Self" else
                     sample(names(mentors[x == min(x)]), 1)
                 })
    out[out == "Self"] <- names(out[out == "Self"])
  }
  out <- data.frame(from = names(out),
                    to = as.character(out), row.names = NULL)
  as_igraph(out)
}

#' @describeIn transform Returns a network with only
#'   the Eulerian path
#' @importFrom igraph eulerian_path
#' @examples
#'   to_eulerian(delete_nodes(ison_konigsberg, "Lomse"))
#'   #autographr(to_eulerian(delete_nodes(ison_konigsberg, "Lomse")))
#' @export
to_eulerian <- function(.data) UseMethod("to_eulerian")

#' @export
to_eulerian.igraph <- function(.data){
  if(!is_eulerian(.data))
    stop("This is not a Eulerian graph.")
  out <- paste(attr(igraph::eulerian_path(.data)$vpath, "names"), 
               collapse = "-+")
  out <- create_explicit(out)
  as_igraph(out)
}

#' @export
to_eulerian.tbl_graph <- function(.data){
  if(!is_eulerian(.data))
    stop("This is not a Eulerian graph.")
  out <- paste(attr(igraph::eulerian_path(.data)$vpath, "names"), 
               collapse = "-+")
  out <- create_explicit(out)
  out
}

#' @describeIn transform Returns the complement of a network
#'   where only ties _not_ present in the original network
#'   are included in the new network.
#' @importFrom igraph complementer
#' @examples
#' to_anti(ison_southern_women)
#' #autographr(to_anti(ison_southern_women))
#' @export
to_anti <- function(.data) UseMethod("to_anti")

#' @export
to_anti.matrix <- function(.data){
  matrix(1, nrow(.data), ncol(.data)) - .data
}

#' @export
to_anti.data.frame <- function(.data){
  as_edgelist.matrix(to_anti.matrix(as_matrix(.data)))
}

#' @export
to_anti.igraph <- function(.data){
  if(is_twomode(.data)){
    as_igraph(to_anti.matrix(as_matrix(.data)))
  } else {
    igraph::complementer(as_igraph(.data), 
                         loops = is_complex(.data))
  }
}

#' @export
to_anti.tbl_graph <- function(.data){
  if(is_twomode(.data)){
    as_tidygraph(to_anti.matrix(as_matrix(.data)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(.data), 
                         loops = is_complex(.data)))
  }
}

#' @export
to_anti.network <- function(.data){
  as_network(to_anti(as_igraph(.data)))
}

#' @describeIn transform Removes all nodes without ties
#' @importFrom tidygraph node_is_isolated
#' @importFrom dplyr filter
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   to_no_isolates()
#' @export
to_no_isolates <- function(.data) UseMethod("to_no_isolates")

#' @export
to_no_isolates.tbl_graph <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices
  .data %>% activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
}

#' @export
to_no_isolates.list <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices in each list
  lapply(.data, function(x) {
    x %>% activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
  })
}

#' @export
to_no_isolates.igraph <- function(.data) {
  as_igraph(to_no_isolates(as_tidygraph(.data)))
}

#' @export
to_no_isolates.matrix <- function(.data) {
  as_matrix(to_no_isolates(as_tidygraph(.data)))
}

#' @export
to_no_isolates.network <- function(.data) {
  as_network(to_no_isolates(as_tidygraph(.data)))
}

#' @export
to_no_isolates.data.frame <- function(.data) {
  as_edgelist(to_no_isolates(as_tidygraph(.data)))
}

#' @describeIn transform Galois derivations
#' @export
to_galois <- function(.data) {
  x <- as_matrix(.data)
  thisRequires("multiplex")
  out <- multiplex::galois(x, labeling = "reduced")
  out <- multiplex::partial.order(out, type = "galois")
  class(out) <- c("matrix", class(out))
  rownames(out)[!startsWith(rownames(out), "{")] <- ""
  colnames(out)[!startsWith(colnames(out), "{")] <- ""
  out
}
