# Pathing ####

#' Modifying networks paths
#' @name modif_paths
#' @description
#'   These functions return tidygraphs containing only special sets of ties:
#' 
#'   - `to_matching()` returns only the matching ties in some network data.
#'   - `to_mentoring()` returns only ties to nodes' closest mentors.
#'   - `to_eulerian()` returns only the Eulerian path within some network data.
#'   - `to_tree()` returns the spanning tree in some network data or, 
#'   if the data is unconnected, a forest of spanning trees.
#'   - `to_dominating()` returns the dominating tree of the network.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(match|mentor|euler|tree|dominat)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_paths
#' @section Matching:
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
#'   _Proceedings of the 18th Annual ACM Symposium on Theory of Computing_. 
#'   136-146. 
#'   \doi{10.1145/12130.12144}
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @param capacities An integer or vector of integers the same length as the
#'   nodes in the network that describes the maximum possible degree the node
#'   can have in the matched network.
#' @importFrom igraph max_bipartite_match
#' @examples 
#' to_matching(ison_southern_women)
#' @export
to_matching <- function(.data, mark = "type", 
                        capacities = NULL) UseMethod("to_matching")

#' @export
to_matching.default <- function(.data, mark = "type", 
                                capacities = NULL){
  as_input(.data, to_matching, mark = mark, capacities = capacities)
}

#' @export
to_matching.igraph <- function(.data, mark = "type", capacities = NULL){
  if(length(unique(node_attribute(.data, mark)))>2)
    snet_abort("This function currently only works with binary attributes.")
  if(is.null(capacities)){
    el <- igraph::max_bipartite_match(.data, 
                                      types = node_attribute(.data, mark))$matching
    el <- data.frame(from = names(el), to = el)
    el$from[is.na(el$from)] <- "dummy"
    el$to[is.na(el$to)] <- "dummy"
    out <- as_igraph(el, twomode = TRUE)
    out <- igraph::delete_vertices(out, "dummy")
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
to_matching.tbl_graph <- function(.data, mark = "type", capacities = NULL){
  as_tidygraph(to_matching.igraph(.data, mark, capacities = capacities)) |> 
    add_info(name = paste(net_name(.data, prefix = "Stable matching of")))
}

#' @export
to_matching.network <- function(.data, mark = "type", capacities = NULL){
  as_network(to_matching(as_igraph(.data), mark, capacities = capacities))
}

#' @export
to_matching.data.frame <- function(.data, mark = "type", capacities = NULL){
  as_edgelist(to_matching(as_igraph(.data), mark, capacities = capacities))
}

#' @export
to_matching.matrix <- function(.data, mark = "type", capacities = NULL){
  as_matrix(to_matching(as_igraph(.data), mark, capacities = capacities))
}

#' @rdname modif_paths 
#' @section Mentoring: 
#'   This function returns a network in which each node is tied to its closest mentor.
#'   The mentors are selected as the top nodes in terms of degree, or those equal
#'   to the highest rank degree in the network, whichever is the higher.
#'   The mentees are then tied to the closest mentor in terms of geodesic distance.
#'   This can be useful for showing the hierarchical structure of a network, 
#'   for example in an organisational context, 
#'   or identifying the most influential nodes in a network.
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
#' @export
to_mentoring <- function(.data, elites = 0.1) UseMethod("to_mentoring")

#' @export
to_mentoring.default <- function(.data, elites = 0.1){
  as_input(.data, to_mentoring, elites = elites)
}

# #' @export
# to_mentoring.tbl_graph <- function(.data, elites = 0.1){
#   as_tidygraph(to_mentoring.igraph(.data, elites = elites)) |> 
#     add_info(name = paste(net_name(.data), "mentorship"))
# }

#' @export
to_mentoring.igraph <- function(.data, elites = 0.1){
  md <- as_matrix(.data)
  if(!is_labelled(.data)) rownames(md) <- colnames(md) <- seq_len(nrow(md))
  ranks <- sort(colSums(md), decreasing = TRUE) # get rank order of indegrees
  mentors <- ranks[ranks == max(ranks)]
  if(length(mentors) == length(ranks)) 
    mentors <- ranks[1:max(1, round(length(ranks)*elites))] # if no mentors, select the top node
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
  if(!is_labelled(.data)) out <- to_unnamed(out)
  as_igraph(out)
}

#' @rdname modif_paths
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
  out |> 
    add_info(name = paste(net_name(.data, prefix = "Eulerian path of")))
}

#' @rdname modif_paths 
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
  out <- igraph::subgraph_from_edges(.data, igraph::sample_spanning_tree(.data))
  as_tidygraph(out)
}

#' @rdname modif_paths 
#' @template param_dir
#' @param from The index or name of the node from which the path should be traced.
#' @export
to_dominating <- function(.data, from, direction = c("out","in")) {
  direction <- match.arg(direction)
  .data <- as_igraph(.data)
  out <- igraph::dominator_tree(.data, root = from, mode = direction)$domtree
  as_tidygraph(out)
}
