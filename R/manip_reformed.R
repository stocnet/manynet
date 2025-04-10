# Projecting ####

#' Modifying networks projection
#' 
#' @description
#'   These functions offer tools for projecting manynet-consistent data:
#' 
#'   - `to_mode1()` projects a two-mode network to a one-mode network
#'   of the first node set's (e.g. rows) joint affiliations to nodes in the second node set (columns). 
#'   - `to_mode2()` projects a two-mode network to a one-mode network
#'   of the second node set's (e.g. columns) joint affiliations to nodes in the first node set (rows).
#'   - `to_ties()` projects a network to one where the ties become nodes and incident nodes become their ties.
# #'   - `to_galois()` projects a network to its Galois derivation.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'  |         | data.frame| igraph| matrix| network| tbl_graph|
#'  |:--------|----------:|------:|------:|-------:|---------:|
#'  |to_mode1 |          1|      1|      1|       1|         1|
#'  |to_mode2 |          1|      1|      1|       1|         1|
#'  |to_ties  |          1|      1|      1|       1|         1|
#' @name manip_project
#' @family modifications
#' @inheritParams manip_reformat
#' @inheritParams manip_split
#' @returns
#'   All `to_` functions return an object of the same class as that provided. 
#'   So passing it an igraph object will return an igraph object
#'   and passing it a network object will return a network object,
#'   with certain modifications as outlined for each function.
NULL

#' @rdname manip_project
#' @param similarity Method for establishing ties,
#'   currently "count" (default), "jaccard", or "rand".
#'   
#'   - "count" calculates the number of coinciding ties,
#'   and can be interpreted as indicating the degree of opportunities
#'   between nodes.
#'   - "jaccard" uses this count as the numerator in a proportion,
#'   where the denominator consists of any cell where either node has a tie.
#'   It can be interpreted as opportunity weighted by participation.
#'   - "rand", or the Simple Matching Coefficient,
#'   is a proportion where the numerator consists of the count of cells where
#'   both nodes are present or both are absent,
#'   over all possible cells.
#'   It can be interpreted as the (weighted) degree of behavioral mirroring
#'   between two nodes.
#'   - "pearson" (Pearson's coefficient) and "yule" (Yule's Q)
#'   produce correlations for valued and binary data, respectively.
#'   Note that Yule's Q has a straightforward interpretation related to the odds ratio.
#' @importFrom igraph bipartite_projection
#' @importFrom stats cor
#' @examples
#' to_mode1(ison_southern_women)
#' to_mode2(ison_southern_women)
#' #graphr(to_mode1(ison_southern_women))
#' #graphr(to_mode2(ison_southern_women))
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
  if(similarity == "count") igraph::bipartite_projection(.data)$proj1
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

#' @rdname manip_project
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
  if(similarity == "count") igraph::bipartite_projection(.data)$proj2
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

#' @rdname manip_project
#' @importFrom igraph make_line_graph E
#' @examples
#' to_ties(ison_adolescents)
#' #graphr(to_ties(ison_adolescents))
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

# #' @rdname manip_project
# #' @section Galois lattices: 
# #'   Note that the output from `to_galois()` is very busy at the moment.
# #' @export
# to_galois <- function(.data) {
#   x <- as_matrix(.data)
#   thisRequires("multiplex")
#   out <- multiplex::galois(x, labeling = "reduced")
#   out <- multiplex::partial.order(out, type = "galois")
#   class(out) <- c("matrix", class(out))
#   rownames(out)[!startsWith(rownames(out), "{")] <- ""
#   colnames(out)[!startsWith(colnames(out), "{")] <- ""
#   out
# }

# Scoping ####

#' Modifying networks scope
#' 
#' @description
#'   These functions offer tools for transforming manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects).
#'   Transforming means that the returned object may have different dimensions
#'   than the original object.
#' 
#'   - `to_ego()` scopes a network into the local neighbourhood of a given node.
#'   - `to_giant()` scopes a network into one including only the main component and no smaller components or isolates.
#'   - `to_no_isolates()` scopes a network into one excluding all nodes without ties
#'   - `to_subgraph()` scopes a network into a subgraph by filtering on some node-related logical statement.
#'   - `to_blocks()` reduces a network to ties between a given partition membership vector.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'  |               | data.frame| igraph| list| matrix| network| tbl_graph|
#'  |:--------------|----------:|------:|----:|------:|-------:|---------:|
#'  |to_blocks      |          1|      1|    0|      1|       1|         1|
#'  |to_ego         |          0|      1|    0|      0|       0|         1|
#'  |to_giant       |          1|      1|    0|      1|       1|         1|
#'  |to_no_isolates |          1|      1|    1|      1|       1|         1|
#'  |to_subgraph    |          1|      1|    0|      1|       1|         1|
#' @name manip_scope
#' @family modifications
#' @inheritParams manip_reformat
#' @returns
#'   All `to_` functions return an object of the same class as that provided. 
#'   So passing it an igraph object will return an igraph object
#'   and passing it a network object will return a network object,
#'   with certain modifications as outlined for each function.
NULL

#' @rdname manip_scope
#' @param node Name or index of node.
#' @param max_dist The maximum breadth of the neighbourhood.
#'   By default 1.
#' @param min_dist The minimum breadth of the neighbourhood.
#'   By default 0. 
#'   Increasing this to 1 excludes the ego,
#'   and 2 excludes ego's direct alters.
#' @param direction String, either "out" or "in".
#' @export
to_ego <- function(.data, node, max_dist = 1, min_dist = 0,
                   direction = c("out","in")) UseMethod("to_ego")

#' @export
to_ego.igraph <- function(.data, node, max_dist = 1, min_dist = 0,
                          direction = c("out","in")){
  egos <- to_egos(.data, max_dist = max_dist, min_dist = min_dist,
                  direction = direction)
  as_igraph(egos[[node]])
}

#' @export
to_ego.tbl_graph <- function(.data, node, max_dist = 1, min_dist = 0,
                             direction = c("out","in")){
  egos <- to_egos(.data, max_dist = max_dist, min_dist = min_dist,
                  direction = direction)
  as_tidygraph(egos[[node]])
}

#' @rdname manip_scope
#' @param time A time point or wave at which to present the network.
#' @export
to_time <- function(.data, time) UseMethod("to_time")

#' @export
to_time.tbl_graph <- function(.data, time){
  if(time > net_waves(.data)){
    snet_info("Sorry, there are not that many waves in this dataset.",
              "Reverting to the maximum wave:", net_waves(.data))
    time <- net_waves(.data)
  }
  if(is_dynamic(.data)){
    snet_unavailable()
  } else if(is_longitudinal(.data)){
    out <- .data
    if(is_changing(out)){
      if(any(time >= as_changelist(.data)$time)){
        out <- apply_changes(out, time)
      } else {
        igraph::graph_attr(out, "changes") <- NULL
      } 
      if("active" %in% net_node_attributes(out)){
        out <- out %>% 
          filter_nodes(active) %>% 
          select_nodes(-active)
      }
    }
    if("wave" %in% net_tie_attributes(out)){
      out %>% 
        # trim ties
        filter_ties(wave == time) %>% 
        select_ties(-wave)
    } else out
  } else {
    .data
  }
}

#' @rdname manip_scope
#' @export
to_giant <- function(.data) UseMethod("to_giant")

#' @export
to_giant.igraph <- function(.data) {
  comps <- igraph::components(.data)
  max.comp <- which.max(comps$csize)
  igraph::delete_vertices(.data, comps$membership != max.comp)
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

#' @rdname manip_scope
#' @importFrom tidygraph node_is_isolated
#' @importFrom dplyr filter
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   to_no_isolates()
#' @export
to_no_isolates <- function(.data) UseMethod("to_no_isolates")

#' @export
to_no_isolates.tbl_graph <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices
  .data %>% tidygraph::activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
}

#' @export
to_no_isolates.list <- function(.data) {
  nodes <- NULL
  # Delete edges not present vertices in each list
  lapply(.data, function(x) {
    x %>% tidygraph::activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
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

#' @rdname manip_scope
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

#' @rdname manip_scope
#' @section `to_blocks()`: 
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
    m1_membs <- membership[!node_is_mode(.data)]
    m2_membs <- membership[node_is_mode(.data)]
    x <- length(unique(m1_membs))
    y <- length(unique(m2_membs))
    out <- matrix(nrow = unique(m1_membs)[x],
                  ncol = unique(m2_membs)[y])
    membership <- as.numeric(as.factor(membership))
    for(i in unique(m1_membs)) for (j in unique(m2_membs))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(unique(m1_membs)[x]))
    colnames(out) <- paste("Block", seq_len(unique(m2_membs)[y]))
  } else {
    mat <- .data
    membership <- as.numeric(as.factor(membership))
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

# Pathing ####

#' Modifying networks paths
#' 
#' @description
#'   These functions return tidygraphs containing only special sets of ties:
#' 
#'   - `to_matching()` returns only the matching ties in some network data.
#'   - `to_mentoring()` returns only ties to nodes' closest mentors.
#'   - `to_eulerian()` returns only the Eulerian path within some network data.
#'   - `to_tree()` returns the spanning tree in some network data or, 
#'   if the data is unconnected, a forest of spanning trees.
#'   - `to_dominating()` returns the dominating tree of the network
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'  |             | data.frame| igraph| matrix| network| tbl_graph|
#'  |:------------|----------:|------:|------:|-------:|---------:|
#'  |to_eulerian  |          0|      1|      0|       0|         1|
#'  |to_matching  |          1|      1|      1|       1|         1|
#'  |to_mentoring |          0|      1|      0|       0|         1|
#' @name manip_paths
#' @family modifications
#' @inheritParams manip_scope
#' @returns
#'   All `to_` functions return an object of the same class as that provided. 
#'   So passing it an igraph object will return an igraph object
#'   and passing it a network object will return a network object,
#'   with certain modifications as outlined for each function.
NULL

#' @rdname manip_paths
#' @section `to_matching()`:
#'   This function attempts to solve the stable matching problem,
#'   also known as the stable marriage problem, upon a given
#'   two-mode network (or other network with a binary mark).  
#' 
#'   In the basic version,
#'   `to_matching()` uses `igraph::max_bipartite_match()`
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
#'   
#'   In the more general version, each node may have a larger capacity,
#'   or even different capacities.
#'   Here an implementation of the Gale-Shapley algorithm is used,
#'   in which an iterative process of proposal and acceptance is repeated until
#'   all are matched or have exhausted their lists of preferences.
#'   This is, however, computationally slower.
#' @references 
#' ## On matching
#'   Gale, David, and Lloyd Stowell Shapley. 1962. 
#'   "College admissions and the stability of marriage". 
#'   _The American Mathematical Monthly_, 69(1): 9–14. 
#'   \doi{10.2307/2312726}
#' 
#'   Goldberg, Andrew V., and Robert E. Tarjan. 1986. 
#'   "A new approach to the maximum flow problem". 
#'   _Proceedings of the eighteenth annual ACM symposium on Theory of computing – STOC '86_. 
#'   136-146. 
#'   \doi{10.1145/12130.12144}
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph max_bipartite_match
#' @examples 
#' to_matching(ison_southern_women)
#' #graphr(to_matching(ison_southern_women))
#' @export
to_matching <- function(.data, mark = "type", capacities = NULL) UseMethod("to_matching")

#' @export
to_matching.igraph <- function(.data, mark = "type", capacities = NULL){
  if(length(unique(node_attribute(.data, mark)))>2)
    snet_abort("This function currently only works with binary attributes.")
  if(is.null(capacities)){
    el <- igraph::max_bipartite_match(.data, 
                                      types = node_attribute(.data, mark))$matching
    el <- data.frame(from = names(el), to = el)
    out <- suppressWarnings(as_igraph(el, twomode = TRUE))
    out <- igraph::delete_vertices(out, "NA")
    out <- to_twomode(out, node_attribute(.data, mark))
  } else {
    if(length(capacities) == 1) 
      capacities <- rep(capacities, net_dims(.data)[2])
    as_matrix(.data)
    
    unmatched_m1 <- 1:net_dims(.data)[1]  # First mode nodes who haven't been matched yet
    m1_matches <- list()  # Student -> College mapping
    m2_matches <- list()  # College -> Students mapping
    for (m2 in 1:net_dims(.data)[2]) {
      m2_matches[[m2]] <- c()
    }
    
    # Gale-Shapley Algorithm
    while (length(unmatched_m1) > 0) {
      m1 <- unmatched_m1[1]
      student_prefs <- students[[student]]
      
      for (college in student_prefs) {
        # If the college has capacity, admit the student
        if (length(college_matches[[college]]) < capacities[[college]]) {
          college_matches[[college]] <- c(college_matches[[college]], student)
          student_matches[[student]] <- college
          unmatched_students <- unmatched_students[-1]  # Remove the matched student
          break
        } else {
          # If college is full, check if the student can replace a current match
          current_students <- college_matches[[college]]
          college_prefs <- colleges[[college]]
          
          # Check if the college prefers this student over any current matches
          worst_student <- current_students[which.max(sapply(current_students, function(s) which(college_prefs == s)))]
          if (which(college_prefs == student) < which(college_prefs == worst_student)) {
            # Replace the worst student
            college_matches[[college]] <- setdiff(current_students, worst_student)
            college_matches[[college]] <- c(college_matches[[college]], student)
            student_matches[[student]] <- college
            unmatched_students <- c(unmatched_students, worst_student)
            unmatched_students <- unmatched_students[unmatched_students != student]
            break
          }
        }
      }
    }
  }
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

#' @rdname manip_paths 
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
#' ## On mentoring
#' Valente, Thomas, and Rebecca Davis. 1999.
#' "Accelerating the Diffusion of Innovations Using Opinion Leaders",
#' _Annals of the American Academy of Political and Social Science_ 566: 56-67.
#' \doi{10.1177/000271629956600105}
#' @examples
#' graphr(to_mentoring(ison_adolescents))
#' @export
to_mentoring <- function(.data, elites = 0.1) UseMethod("to_mentoring")

#' @export
to_mentoring.tbl_graph <- function(.data, elites = 0.1){
  as_tidygraph(to_mentoring.igraph(.data, elites = elites))
}

#' @export
to_mentoring.igraph <- function(.data, elites = 0.1){
  md <- as_matrix(.data)
  if(!is_labelled(.data)) rownames(md) <- colnames(md) <- seq_len(nrow(md))
  ranks <- sort(colSums(md), decreasing = TRUE) # get rank order of indegrees
  mentors <- ranks[ranks == max(ranks)]
  if(length(mentors) < length(ranks)*elites)
    mentors <- ranks[seq_len(length(ranks)*elites)]
  dists <- igraph::distances(.data) # compute geodesic matrix
  if(!is_labelled(.data)) rownames(dists) <- colnames(dists) <- seq_len(nrow(dists))
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

#' @rdname manip_paths
#' @importFrom igraph eulerian_path
#' @references
#' ## On Eulerian trails
#' Euler, Leonard. 1736.
#' "Solutio problematis ad geometriam situs pertinentis". 
#' _Comment. Academiae Sci. I. Petropolitanae_ 8: 128–140.
#' 
#' Hierholzer, Carl. 1873. 
#' "Ueber die Möglichkeit, einen Linienzug ohne Wiederholung und ohne Unterbrechung zu umfahren".
#' _Mathematische Annalen_, 6(1): 30–32.
#' \doi{10.1007/BF01442866}
#' @examples
#'   to_eulerian(delete_nodes(ison_koenigsberg, "Lomse"))
#'   #graphr(to_eulerian(delete_nodes(ison_koenigsberg, "Lomse")))
#' @export
to_eulerian <- function(.data) UseMethod("to_eulerian")

#' @export
to_eulerian.igraph <- function(.data){
  if(!is_eulerian(.data))
    snet_abort("This is not a Eulerian graph.")
  out <- paste(attr(igraph::eulerian_path(.data)$vpath, "names"), 
               collapse = "-+")
  out <- create_explicit(out)
  as_igraph(out)
}

#' @export
to_eulerian.tbl_graph <- function(.data){
  if(!is_eulerian(.data))
    snet_abort("This is not a Eulerian graph.")
  out <- paste(attr(igraph::eulerian_path(.data)$vpath, "names"), 
               collapse = "-+")
  out <- create_explicit(out)
  out
}

#' @rdname manip_paths 
#' @references
#' ## On minimum spanning trees
#' Boruvka, Otakar. 1926.
#' "O jistem problemu minimalnim".
#' _Prace Mor. Prirodoved. Spol. V Brne III_ 3: 37-58.
#' 
#' Kruskal, Joseph B. 1956.
#' "On the shortest spanning subtree of a graph and the travelling salesman problem".
#' _Proceedings of the American Mathematical Society_ 7(1): 48-50.
#' \doi{10.1090/S0002-9939-1956-0078686-7}
#' 
#' Prim, R.C. 1957.
#' "Shortest connection networks and some generalizations".
#' _Bell System Technical Journal_ 36(6):1389-1401.
#' \doi{10.1002/j.1538-7305.1957.tb01515.x}
#' @export
to_tree <- function(.data) {
  .data <- as_igraph(.data)
  out <- igraph::subgraph.edges(.data, igraph::sample_spanning_tree(.data))
  as_tidygraph(out)
}

#' @rdname manip_paths 
#' @param from The index or name of the node from which the path should be traced.
#' @export
to_dominating <- function(.data, from, direction = c("out","in")) {
  direction <- match.arg(direction)
  .data <- as_igraph(.data)
  out <- igraph::dominator_tree(.data, root = from, mode = direction)$domtree
  as_tidygraph(out)
}
