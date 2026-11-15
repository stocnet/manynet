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
#' is_eulerian(ison_centrality)
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
#'   The function `is_aperiodic()` finds the period of the network,
#'   the greatest common divisor of the lengths of all its cycles.
#'   If the period is 1, the network is aperiodic.
#'
#'   Each cycle lies inside one strongly connected component.
#'   In each component, the nodes are numbered by their distance from one node,
#'   and the period of the component is the greatest common divisor of
#'   `level[i] + 1 - level[j]` over the ties from i to j inside it
#'   (Jarvis and Shier 1996).
#'   This is exact, and takes time in proportion to the number of ties.
#'   The network is aperiodic where every component with a cycle has
#'   a period of 1, as a Markov chain is aperiodic where every class is.
#'   So two components with cycles of 2 and of 3 are each periodic,
#'   and the network is not aperiodic, although the lengths 2 and 3
#'   have no common divisor.
#'   An undirected tie is a cycle of two, so an undirected network is
#'   aperiodic where it has a cycle of odd length.
#'   A self-tie is a cycle of length one, as a state that can stay where it is
#'   in a Markov chain, so a component with a self-tie is aperiodic.
#'   In an opinion model, a self-tie is a self-weight (see [to_influence()]).
#'   A network with no cycle counts as aperiodic.
#' @param max_path_length Deprecated, and no longer used,
#'   since the period is now found exactly.
#' @references
#' ## On aperiodicity
#' Jarvis, J.P, and D.R. Shier. 1996.
#' "Graph-theoretic analysis of finite Markov chains",
#' in Shier, D.R., Wallenius, K.T. (eds) _Applied Mathematical Modeling: A Multidisciplinary Approach_.
#' CRC Press.
#' @examples 
#' is_aperiodic(ison_algebra)
#' is_aperiodic(create_ring(31))
#' @export
is_aperiodic <- function(.data, max_path_length) UseMethod("is_aperiodic")

#' @export
is_aperiodic.default <- function(.data, max_path_length) {
  if(!missing(max_path_length))
    snet_deprecated("max_path_length", type = "argument",
                    msg = paste("{.arg max_path_length} is deprecated, and no longer",
                                "used: the period is now found exactly."))
  is_aperiodic(as_igraph(.data))
}

#' @export
is_aperiodic.igraph <- function(.data, max_path_length){
  if(!missing(max_path_length))
    snet_deprecated("max_path_length", type = "argument",
                    msg = paste("{.arg max_path_length} is deprecated, and no longer",
                                "used: the period is now found exactly."))
  if(is_twomode(.data)) return(FALSE)
  periods <- .network_period(.data)
  # No cycles at all, so there is no period to divide; following the
  # Markov-chain convention, a network with no return paths is aperiodic.
  # Otherwise every component with a cycle must have a period of 1.
  all(is.na(periods) | periods == 1)
}

# The period of each strongly connected component that has a cycle: the
# greatest common divisor of the lengths of its cycles. NA where no component
# has a cycle. A self-tie is a cycle of length one, so it makes the period
# of its component 1, as in a Markov chain. In each component, the nodes are
# numbered by their distance from a root, and every tie i -> j inside it
# gives level[i] + 1 - level[j], a multiple of the period, while the ties of
# a breadth-first tree give 0; the greatest common divisor of these is the
# period of the component.
.network_period <- function(.data){
  n <- igraph::vcount(.data)
  el <- igraph::as_edgelist(.data, names = FALSE)
  if(!igraph::is_directed(.data)) el <- rbind(el, el[, 2:1, drop = FALSE])
  el <- unique(el)
  if(!nrow(el)) return(NA_real_)
  g <- igraph::graph_from_edgelist(el, directed = TRUE)
  g <- igraph::add_vertices(g, max(0, n - igraph::vcount(g)))
  comp <- igraph::components(g, mode = "strong")$membership
  inside <- comp[el[, 1]] == comp[el[, 2]]
  if(!any(inside)) return(NA_real_)
  el <- el[inside, , drop = FALSE]
  level <- rep(NA_real_, n)
  for(r in unique(comp[el[, 1]])){
    root <- which(comp == r)[1]
    d <- igraph::distances(g, v = root, mode = "out")[1, ]
    level[comp == r] <- d[comp == r]
  }
  diffs <- abs(level[el[, 1]] + 1 - level[el[, 2]])
  periods <- tapply(diffs, comp[el[, 1]], function(x){
    x <- unique(x[x > 0])
    if(length(x)) Reduce(.gcd, x) else NA_real_
  })
  stats::setNames(as.numeric(periods), names(periods))
}

.gcd <- function(x, y){
  ifelse(y, Recall(y, x %% y), x)
}