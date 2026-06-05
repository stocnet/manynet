#' Marking networks features
#' 
#' @description
#'   These functions implement logical tests for various network
#'   features.
#'   
#'   - `is_connected()` tests whether network is strongly connected, 
#'   or weakly connected if undirected.
#'   - `is_perfect_matching()` tests whether there is a matching 
#'   for a network that covers every node in the network.
#'   - `is_eulerian()` tests whether there is a Eulerian path for a network
#'   where that path passes through every tie exactly once.
#'   - `is_acyclic()` tests whether network is a directed acyclic graph.
#'   - `is_aperiodic()` tests whether network is aperiodic.
#' @template param_data
#' @eval detail_avail("is_(connected|perfect_matching|eulerian|acyclic|aperiodic)")
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marking
#' @name mark_features
NULL

#' @rdname mark_features
#' @section is_connected: 
#'   To test weak connection on a directed network,
#'   please see `to_undirected()`.
#' @importFrom igraph is_connected
#' @examples
#' is_connected(ison_southern_women)
#' @export
is_connected <- function(.data) UseMethod("is_connected")

#' @export
is_connected.default <- function(.data) {
  is_connected(as_igraph(.data))
}

#' @export
is_connected.igraph <- function(.data) {
  igraph::is_connected(.data, 
                       mode = ifelse(is_directed(.data),
                                     "strong", "weak"))
}

#' @rdname mark_features
#' @section is_perfect_matching: 
#'   For two-mode or bipartite networks, `to_matching()` is used
#'   to identify whether a perfect matching is possible.
#'   For one-mode networks, we use the Tutte theorem.
#'   Note that currently only subgraphs with cutpoints removed are tested,
#'   and not all possible subgraphs.
#'   This is to avoid computationally expensive combinatorial operations,
#'   but may come at the cost of some edge cases where a one-mode network
#'   cannot perfectly match as suggested.
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @references
#' ## On perfect matching
#'   Tutte, William T. 1950. 
#'   "The factorization of locally finite graphs". 
#'   _Canadian Journal of Mathematics_. 2: 44–49. 
#'   \doi{10.4153/cjm-1950-005-2}
#' @examples
#' is_perfect_matching(ison_southern_women)
#' @export
is_perfect_matching <- function(.data, mark = "type") UseMethod("is_perfect_matching")

#' @export
is_perfect_matching.default <- function(.data, mark = "type") {
  is_perfect_matching(as_igraph(.data), mark = mark)
}

#' @export
is_perfect_matching.igraph <- function(.data, mark = "type"){
  if(mark %in% net_node_attributes(.data)){
    matches <- to_matching(.data, mark = mark)
    net_ties(matches)*2 == net_nodes(matches)
  } else {
    if (net_nodes(.data) %% 2 != 0) FALSE else # odd number of nodes cannot match perfectly
      if (!igraph::is_connected(.data) && # any odd components cannot match perfectly
          any(igraph::component_distribution(.data)[c(F,T)]!=0)) FALSE else { # note first index is 0...
            cutpoints <- igraph::articulation_points(.data)
            gminusu <- igraph::delete_vertices(.data, cutpoints)
            sum((igraph::component_distribution(gminusu) * igraph::count_components(gminusu))[c(F,T)]) <= length(cutpoints)
          }
  }
}

#' @rdname mark_features
#' @importFrom igraph has_eulerian_path
#' @examples
#' is_eulerian(ison_brandes)
#' @export
is_eulerian <- function(.data) UseMethod("is_eulerian")

#' @export
is_eulerian.default <- function(.data) {
  is_eulerian(as_igraph(.data))
}

#' @export
is_eulerian.igraph <- function(.data){
  igraph::has_eulerian_path(.data)
}

#' @rdname mark_features
#' @importFrom igraph is_dag
#' @examples 
#' is_acyclic(ison_algebra)
#' @export
is_acyclic <- function(.data) UseMethod("is_acyclic")

#' @export
is_acyclic.default <- function(.data) {
  is_acyclic(as_igraph(.data))
}

#' @export
is_acyclic.igraph <- function(.data){
  igraph::is_dag(.data)
}

#' @rdname mark_features
#' @section Aperiodicity: 
#'   Aperiodicity is a property of directed networks that can be interpreted as 
#'   the absence of cycles of a common length.
#'   Aperiodicity is a necessary condition for the existence of a 
#'   unique stationary distribution in Markov chains, and thus is 
#'   an important property for the analysis of dynamic processes on networks.
#'   The function `is_aperiodic()` tests for aperiodicity by finding 
#'   all simple paths from each node back to itself, and then 
#'   calculating the greatest common divisor of the lengths of these paths. 
#'   If the greatest common divisor is 1, the network is aperiodic.
#' @param max_path_length Maximum path length considered.
#'   If negative, paths of all lengths are considered.
#'   By default 4, to avoid potentially very long computation times.
#' @source https://stackoverflow.com/questions/55091438/r-igraph-find-all-cycles
#' @references
#' ## On aperiodicity
#' Jarvis, J.P, and D.R. Shier. 1996.
#' "Graph-theoretic analysis of finite Markov chains",
#' in Shier, D.R., Wallenius, K.T. (eds) _Applied Mathematical Modeling: A Multidisciplinary Approach_.
#' CRC Press.
#' @examples 
#' is_aperiodic(ison_algebra)
#' @export
is_aperiodic <- function(.data, max_path_length = 4) UseMethod("is_aperiodic")

#' @export
is_aperiodic.default <- function(.data, max_path_length = 4) {
  is_aperiodic(as_igraph(.data), max_path_length = max_path_length)
}

#' @export
is_aperiodic.igraph <- function(.data, max_path_length = 4){
  # thisRequires("minMSE") # >80x faster than e.g. cheapr::gcd()
  if(is_twomode(.data)) return(FALSE)
  snet_info("Obtaining paths no greater than {max_path_length}.")
  out <- suppressMessages(.quiet(unlist(lapply(seq_nodes(.data), function(v1){
    if(igraph::degree(.data, v1, mode="in") == 0) NULL else {
      v1_vertex <- igraph::V(.data)[v1]
      goodNeighbors <- igraph::neighbors(.data, v1_vertex, mode="out")
      goodNeighbors <- goodNeighbors[goodNeighbors > v1_vertex]
      unlist(lapply(goodNeighbors, function(v2){
        vapply(igraph::all_simple_paths(.data, v2, igraph::V(.data)[v1], mode="out", 
                                        cutoff = max_path_length), length, 
               FUN.VALUE = numeric(1))
      }))
    }
  }))))
  snet_info("Finding greatest common divisor of all paths.")
  out <- unique(sort(out))
  while(out[1]!=1 && length(out)>1){
    cd <- .gcd(out[1], out[2])
    if(length(out)==2) out <- cd else
      out <- c(cd, out[2:length(out)])
  }
  return(as.logical(out[1]==1))
}

.gcd <- function(x, y){
  ifelse(y, Recall(y, x %% y), x)
}