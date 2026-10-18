#' Marking networks features
#' 
#' @description
#'   These functions implement logical tests for various network
#'   features.
#'   
#'   - `is_connected()` tests whether network is connected,
#'   by default in the strong sense where ties are directed.
#'   Pass `connectivity = "weak"` to ignore tie direction.
#'   - `is_perfect_matching()` tests whether there is a matching 
#'   for a network that covers every node in the network.
#'   - `is_eulerian()` tests whether there is a Eulerian path for a network
#'   where that path passes through every tie exactly once.
#'   - `is_acyclic()` tests whether network is a directed acyclic graph.
#'   - `is_aperiodic()` tests whether network is aperiodic.
#' @template param_data
#' @template param_connectivity
#' @eval detail_avail("is_(connected|perfect_matching|eulerian|acyclic|aperiodic)")
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marking
#' @name mark_features
NULL

#' @rdname mark_features
#' @importFrom igraph is_connected
#' @examples
#' is_connected(ison_southern_women)
#' is_connected(ison_algebra, connectivity = "weak")
#' @export
is_connected <- function(.data,
                         connectivity = c("strong", "weak")) UseMethod("is_connected")

#' @export
is_connected.default <- function(.data, connectivity = c("strong", "weak")) {
  is_connected(as_igraph(.data), connectivity = connectivity)
}

#' @export
is_connected.igraph <- function(.data, connectivity = c("strong", "weak")) {
  # igraph ignores mode for undirected networks, where the notions coincide.
  igraph::is_connected(.data, mode = match.arg(connectivity))
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
#' @references
#' ## On Eulerian cycles and paths
#' Euler, Leonard. 1736.
#' "Solutio problematis ad geometriam situs pertinentis". 
#' _Comment. Academiae Sci. I. Petropolitanae_ 8: 128–140.
#' 
#' Hierholzer, Carl. 1873. 
#' "Ueber die Möglichkeit, einen Linienzug ohne Wiederholung und ohne Unterbrechung zu umfahren".
#' _Mathematische Annalen_, 6(1): 30–32.
#' \doi{10.1007/BF01442866}
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
  # A cycle closes a path of at most `max_path_length` ties with one more, and
  # a negative length leaves the paths unbounded, so that no simple cycle can
  # be longer than the network has nodes.
  longest <- if(max_path_length < 0) igraph::vcount(.data) else
    max_path_length + 1
  out <- .closed_walk_lengths(.data, longest)
  snet_info("Finding greatest common divisor of all paths.")
  out <- unique(sort(out))
  if(!length(out)){
    # No cycles at all, so there is no period to divide; following the
    # Markov-chain convention, treat a graph with no return paths as aperiodic
    # rather than returning the NA the gcd loop would otherwise produce.
    snet_info("No cycles found within {max_path_length} steps; treating as aperiodic.")
    return(TRUE)
  }
  # the first two lengths give way to their divisor, until one is left
  while(out[1]!=1 && length(out)>1){
    out <- c(.gcd(out[1], out[2]), out[-(1:2)])
  }
  as.logical(out[1]==1)
}

# The lengths, from two up to `longest`, of the closed walks in a network.
# Every closed walk is made of cycles no longer than itself, and every cycle
# is a closed walk, so the walks up to a length share their greatest common
# divisor with the simple cycles up to that length. Self-ties close no cycle
# through another node, and are set aside. The walks are extended one tie at
# a time, and kept only as the pairs of nodes they start and end at, which
# are as many as the cells of the matching power of the adjacency matrix.
.closed_walk_lengths <- function(.data, longest){
  n <- igraph::vcount(.data)
  el <- igraph::as_edgelist(.data, names = FALSE)
  if(!igraph::is_directed(.data)) el <- rbind(el, el[, 2:1, drop = FALSE])
  el <- unique(el[el[, 1] != el[, 2], , drop = FALSE])
  onward <- split(el[, 2], factor(el[, 1], levels = seq_len(n)))
  from <- el[, 1]
  to <- el[, 2]
  out <- integer(0)
  for(k in seq_len(longest)[-1]){
    from <- rep(from, lengths(onward)[to])
    to <- unlist(onward[to], use.names = FALSE)
    keep <- !duplicated((from - 1) * n + to)
    from <- from[keep]
    to <- to[keep]
    if(!length(from)) break
    if(any(from == to)) out <- c(out, k)
  }
  out
}

.gcd <- function(x, y){
  ifelse(y, Recall(y, x %% y), x)
}