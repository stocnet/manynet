# Degree-like centralities ####

#' Measures of degree-like centrality and centralisation
#'
#' @description
#'   These functions calculate common degree-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_degree()` measures the degree centrality of nodes in an unweighted network,
#'   or weighted degree/strength of nodes in a weighted network; 
#'   there are several related shortcut functions:
#'     - `node_deg()` returns the unnormalised results.
#'     - `node_indegree()` returns the `direction = 'in'` results.
#'     - `node_outdegree()` returns the `direction = 'out'` results.
#'   - `node_multidegree()` measures the ratio between types of ties in a multiplex network.
#'   - `node_posneg()` measures the PN (positive-negative) centrality of a signed network.
#'   - `node_leverage()` measures the leverage centrality of nodes in a network.
#'   - `tie_degree()` measures the degree centrality of ties in a network
#'   - `net_degree()` measures a network's degree centralization; 
#'   there are several related shortcut functions:
#'     - `net_indegree()` returns the `direction = 'out'` results.
#'     - `net_outdegree()` returns the `direction = 'out'` results.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name measure_central_degree
#' @family centrality
#' @family measures
#' @seealso [to_undirected()] for removing edge directions
#'   and [to_unweighted()] for removing weights from a graph.
#' @inheritParams mark_is
#' @param normalized Logical scalar, whether the centrality scores are normalized.
#'   Different denominators are used depending on whether the object is one-mode or two-mode,
#'   the type of centrality, and other arguments.
#' @param alpha Numeric scalar, the positive tuning parameter introduced in
#'   Opsahl et al (2010) for trading off between degree and strength centrality measures.
#'   By default, `alpha = 0`, which ignores tie weights and the measure is solely based
#'   upon degree (the number of ties).
#'   `alpha = 1` ignores the number of ties and provides the sum of the tie weights 
#'   as strength centrality.
#'   Values between 0 and 1 reflect different trade-offs in the relative contributions of
#'   degree and strength to the final outcome, with 0.5 as the middle ground.
#'   Values above 1 penalise for the number of ties.
#'   Of two nodes with the same sum of tie weights, the node with fewer ties will obtain
#'   the higher score.
#'   This argument is ignored except in the case of a weighted network.
#' @param direction Character string, “out” bases the measure on outgoing ties, 
#'   “in” on incoming ties, and "all" on either/the sum of the two. 
#'   For two-mode networks, "all" uses as numerator the sum of differences
#'   between the maximum centrality score for the mode 
#'   against all other centrality scores in the network,
#'   whereas "in" uses as numerator the sum of differences
#'   between the maximum centrality score for the mode 
#'   against only the centrality scores of the other nodes in that mode.
#' @return A single centralization score if the object was one-mode,
#'   and two centralization scores if the object was two-mode.
#' @importFrom igraph graph_from_incidence_matrix is_bipartite degree V
#' @references 
#' ## On multimodal centrality
#' Faust, Katherine. 1997. 
#' "Centrality in affiliation networks." 
#' _Social Networks_ 19(2): 157-191.
#' \doi{10.1016/S0378-8733(96)00300-0}
#' 
#' Borgatti, Stephen P., and Martin G. Everett. 1997. 
#' "Network analysis of 2-mode data." 
#' _Social Networks_ 19(3): 243-270.
#' \doi{10.1016/S0378-8733(96)00301-2}
#' 
#' Borgatti, Stephen P., and Daniel S. Halgin. 2011. 
#' "Analyzing affiliation networks." 
#' In _The SAGE Handbook of Social Network Analysis_, 
#' edited by John Scott and Peter J. Carrington, 417–33. 
#' London, UK: Sage.
#' \doi{10.4135/9781446294413.n28}
#' 
#' ## On strength centrality
#' Opsahl, Tore, Filip Agneessens, and John Skvoretz. 2010. 
#' "Node centrality in weighted networks: Generalizing degree and shortest paths." 
#' _Social Networks_ 32, 245-251.
#' \doi{10.1016/j.socnet.2010.03.006}
#' @examples
#' node_degree(ison_southern_women)
#' @return Depending on how and what kind of an object is passed to the function,
#' the function will return a `tidygraph` object where the nodes have been updated
NULL

#' @rdname measure_central_degree 
#' @importFrom manynet as_igraph
#' @export
node_degree <- function (.data, normalized = TRUE, alpha = 1,
                         direction = c("all","out","in")){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  graph <- manynet::as_igraph(.data)
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  direction <- match.arg(direction)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    degrees <- igraph::degree(graph = graph, 
                              v = igraph::V(graph), 
                              mode = direction, 
                              loops = manynet::is_complex(.data))
    other_set_size <- ifelse(igraph::V(graph)$type, 
                             sum(!igraph::V(graph)$type), 
                             sum(igraph::V(graph)$type))
    out <- degrees/other_set_size
  } else {
    if (all(is.na(weights))) {
      out <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = manynet::is_complex(.data),
                     normalized = normalized)
    }
    else {
      ki <- igraph::degree(graph = graph, v = igraph::V(graph), 
                     mode = direction, 
                     loops = manynet::is_complex(.data))
      si <- igraph::strength(graph = graph, vids = igraph::V(graph), 
                       mode = direction,
                       loops = manynet::is_complex(.data), weights = weights)
      out <- ki * (si/ki)^alpha
      out[is.nan(out)] <- 0
      if(normalized) out <- out/max(out)
    }
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_degree
#' @export
node_deg <- function (.data, alpha = 0, direction = c("all","out","in")){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  node_degree(.data, normalized = FALSE, alpha = alpha, direction = direction)
}

#' @rdname measure_central_degree
#' @export
node_outdegree <- function (.data, normalized = TRUE, alpha = 0){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  node_degree(.data, normalized = normalized, alpha = alpha, direction = "out")
}

#' @rdname measure_central_degree
#' @export
node_indegree <- function (.data, normalized = TRUE, alpha = 0){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  node_degree(.data, normalized = normalized, alpha = alpha, direction = "in")
}

#' @rdname measure_central_degree
#' @param tie1 Character string indicating the first uniplex network.
#' @param tie2 Character string indicating the second uniplex network.
#' @export
node_multidegree <- function (.data, tie1, tie2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  stopifnot(manynet::is_multiplex(.data))
  out <- node_degree(manynet::to_uniplex(.data, tie1)) - 
    node_degree(manynet::to_uniplex(.data, tie2))
  make_node_measure(out, .data)
}

#' @rdname measure_central_degree
#' @references
#' ## On signed centrality
#' Everett, Martin G., and Stephen P. Borgatti. 2014. 
#' “Networks Containing Negative Ties.” 
#' _Social Networks_ 38:111–20. 
#' \doi{10.1016/j.socnet.2014.03.005}
#' @export
node_posneg <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  stopifnot(manynet::is_signed(.data))
  pos <- manynet::as_matrix(manynet::to_unsigned(.data, keep = "positive"))
  neg <- manynet::as_matrix(manynet::to_unsigned(.data, keep = "negative"))
  nn <- manynet::net_nodes(.data)
  pn <- pos-neg*2
  diag(pn) <- 0
  idmat <- diag(nn)
  v1 <- matrix(1,nn,1)
  out <- solve(idmat - ((pn%*%t(pn))/(4*(nn-1)^2))) %*% (idmat+( pn/(2*(nn-1)) )) %*% v1
  make_node_measure(out, .data)
}

#' @rdname measure_central_degree
#' @section Leverage centrality: 
#'   Leverage centrality concerns the degree of a node compared with that of its
#'   neighbours, \eqn{J}:
#'   \deqn{C_L(i) = \frac{1}{deg(i)} \sum_{j \in J(i)} \frac{deg(i) - deg(j)}{deg(i) + deg(j)}}
#' @references
#' ## On leverage centrality
#' Joyce, Karen E., Paul J. Laurienti, Jonathan H. Burdette, and Satoru Hayasaka. 2010.
#' "A New Measure of Centrality for Brain Networks". 
#' _PLoS ONE_ 5(8): e12200.
#' \doi{10.1371/journal.pone.0012200}
#' @export
node_leverage <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- (node_deg(.data) - node_neighbours_degree(.data))/
    (node_deg(.data) + node_neighbours_degree(.data))
  make_node_measure(out, .data)
}

#' @rdname measure_central_degree
#' @examples 
#' tie_degree(ison_adolescents)
#' @export
tie_degree <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  edge_adj <- manynet::to_ties(.data)
  out <- node_degree(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname measure_central_degree
#' @examples
#' net_degree(ison_southern_women, direction = "in")
#' @export
net_degree <- function(.data, normalized = TRUE,
                           direction = c("all", "out", "in")){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  direction <- match.arg(direction)
  
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
    mode <- c(rep(FALSE, nrow(mat)), rep(TRUE, ncol(mat)))
    
    out <- list()
    if (direction == "all") {
      if (!normalized) {
        allcent <- c(rowSums(mat), colSums(mat))
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat))*ncol(mat) - 2*(ncol(mat) + nrow(mat) - 1))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((nrow(mat) + ncol(mat))*nrow(mat) - 2*(ncol(mat) + nrow(mat) - 1))
      } else if (normalized) {
        allcent <- node_degree(mat, normalized = TRUE)
        out$nodes1 <- sum(max(allcent[!mode]) - allcent)/((nrow(mat) + ncol(mat) - 1) - (ncol(mat) - 1) / nrow(mat) - (ncol(mat) + nrow(mat) - 1)/nrow(mat))
        out$nodes2 <- sum(max(allcent[mode]) - allcent)/((ncol(mat) + nrow(mat) - 1) - (nrow(mat) - 1) / ncol(mat) - (nrow(mat)  + ncol(mat) - 1)/ncol(mat))
      }
    } else if (direction == "in") {
      out$nodes1 <- sum(max(rowSums(mat)) - rowSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
      out$nodes2 <- sum(max(colSums(mat)) - colSums(mat))/((ncol(mat) - 1)*(nrow(mat) - 1))
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_degree(graph = .data, mode = direction, 
                                normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}

#' @rdname measure_central_degree
#' @export
net_outdegree <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  net_degree(.data, normalized = normalized, direction = "out")
}

#' @rdname measure_central_degree
#' @export
net_indegree <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  net_degree(.data, normalized = normalized, direction = "in")
}

# Betweenness-like centralities ####

#' Measures of betweenness-like centrality and centralisation
#' @description
#'   These functions calculate common betweenness-related centrality measures for one- and two-mode networks:
#'   
#'   - `node_betweenness()` measures the betweenness centralities of nodes in a network.
#'   - `node_induced()` measures the induced betweenness centralities of nodes in a network.
#'   - `node_flow()` measures the flow betweenness centralities of nodes in a network,
#'   which uses an electrical current model for information spreading 
#'   in contrast to the shortest paths model used by normal betweenness centrality.
#'   - `node_stress()` measures the stress centrality of nodes in a network.
#'   - `tie_betweenness()` measures the number of shortest paths going through a tie.
#'   - `net_betweenness()` measures the betweenness centralization for a network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name measure_central_between
#' @family centrality
#' @family measures
#' @inheritParams measure_central_degree
#' @param cutoff The maximum path length to consider when calculating betweenness.
#'   If negative or NULL (the default), there's no limit to the path lengths considered.
NULL

#' @rdname measure_central_between
#' @section Betweenness centrality: 
#'   Betweenness centrality is based on the number of shortest paths between
#'   other nodes that a node lies upon:
#'   \deqn{C_B(i) = \sum_{j,k:j \neq k, j \neq i, k \neq i} \frac{g_{jik}}{g_{jk}}}
#' @references
#' ## On betweenness centrality
#' Freeman, Linton. 1977. 
#' "A set of measures of centrality based on betweenness". 
#' _Sociometry_, 40(1): 35–41. 
#' \doi{10.2307/3033543}
#' @examples
#' node_betweenness(ison_southern_women)
#' @return A numeric vector giving the betweenness centrality measure of each node.
#' @export 
node_betweenness <- function(.data, normalized = TRUE, 
                             cutoff = NULL){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    betw_scores <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                       directed = manynet::is_directed(graph))
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- ifelse(set_size > other_set_size, 
                  betw_scores/(2*(set_size-1)*(other_set_size-1)), 
                  betw_scores/(1/2*other_set_size*(other_set_size-1)+1/2*(set_size-1)*(set_size-2)+(set_size-1)*(other_set_size-1)))
  } else {
    if (is.null(cutoff)) {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = manynet::is_directed(graph), weights = weights, 
                                 normalized = normalized)
    } else {
      out <- igraph::betweenness(graph = graph, v = igraph::V(graph), 
                                 directed = manynet::is_directed(graph), 
                                 cutoff = cutoff, 
                                 weights = weights)
    }
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_between 
#' @section Induced centrality: 
#'   Induced centrality or vitality centrality concerns the change in 
#'   total betweenness centrality between networks with and without a given node:
#'   \deqn{C_I(i) = C_B(G) - C_B(G\ i)}
#' @references
#' ## On induced centrality
#' Everett, Martin and Steve Borgatti. 2010.
#' "Induced, endogenous and exogenous centrality"
#' _Social Networks_, 32: 339-344.
#' \doi{10.1016/j.socnet.2010.06.004}
#' @examples
#' node_induced(ison_adolescents)
#' @export 
node_induced <- function(.data, normalized = TRUE, 
                         cutoff = NULL){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  endog <- sum(node_betweenness(.data, normalized = normalized, cutoff = cutoff),
               na.rm = TRUE)
  exog <- vapply(seq.int(manynet::net_nodes(.data)),
                 function(x) sum(node_betweenness(manynet::delete_nodes(.data, x),
                                              normalized = normalized, cutoff = cutoff),
                                 na.rm = TRUE),
                FUN.VALUE = numeric(1))
  out <- endog - exog
  make_node_measure(out, .data)
}

#' @rdname measure_central_between 
#' @section Flow betweenness centrality: 
#'   Flow betweenness centrality concerns the total maximum flow, \eqn{f},
#'   between other nodes \eqn{j,k} in a network \eqn{G} that a given node mediates:
#'   \deqn{C_F(i) = \sum_{j,k:j\neq k, j\neq i, k\neq i} f(j,k,G) - f(j,k,G\ i)}
#'   When normalized (by default) this sum of differences is divided by the
#'   sum of flows \eqn{f(i,j,G)}.
#' @references
#' ## On flow centrality
#' Freeman, Lin, Stephen Borgatti, and Douglas White. 1991. 
#' "Centrality in Valued Graphs: A Measure of Betweenness Based on Network Flow". 
#' _Social Networks_, 13(2), 141-154.
#' 
#' Koschutzki, D., K.A. Lehmann, L. Peeters, S. Richter, D. Tenfelde-Podehl, and O. Zlotowski. 2005. 
#' "Centrality Indices". 
#' In U. Brandes and T. Erlebach (eds.), _Network Analysis: Methodological Foundations_. 
#' Berlin: Springer.
#' @export 
node_flow <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  thisRequires("sna")
  out <- sna::flowbet(as_network(.data),
                      gmode = ifelse(is_directed(.data), "digraph", "graph"),
                      diag = is_complex(.data),
                      cmode = ifelse(normalized, "normflow", "rawflow"))
  make_node_measure(out, .data)
}

#' @rdname measure_central_between 
#' @section Stress centrality: 
#'   Stress centrality is the number of all shortest paths or geodesics, \eqn{g}, 
#'   between other nodes that a given node mediates:
#'   \deqn{C_S(i) = \sum_{j,k:j \neq k, j \neq i, k \neq i} g_{jik}}
#'   High stress nodes lie on a large number of shortest paths between other
#'   nodes, and thus associated with bridging or spanning boundaries.
#' @references
#' ## On stress centrality
#'   Shimbel, A. 1953.
#'   "Structural Parameters of Communication Networks".
#'   _Bulletin of Mathematical Biophysics_, 15:501-507.
#'   \doi{10.1007/BF02476438}
#' @export 
node_stress <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  thisRequires("sna")
  out <- sna::stresscent(as_network(.data),
                      gmode = ifelse(is_directed(.data), "digraph", "graph"),
                      diag = is_complex(.data),
                      rescale = normalized)
  make_node_measure(out, .data)
}

#' @rdname measure_central_between
#' @importFrom igraph edge_betweenness
#' @examples
#' (tb <- tie_betweenness(ison_adolescents))
#' plot(tb)
#' ison_adolescents %>% mutate_ties(weight = tb) %>% 
#'      graphr()
#' @export
tie_betweenness <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  .data <- manynet::as_igraph(.data)
  eddies <- manynet::as_edgelist(.data)
  eddies <- paste(eddies[["from"]], eddies[["to"]], sep = "-")
  out <- igraph::edge_betweenness(.data)
  names(out) <- eddies
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname measure_central_between
#' @examples
#' net_betweenness(ison_southern_women, direction = "in")
#' @export
net_betweenness <- function(.data, normalized = TRUE,
                                direction = c("all", "out", "in")) {
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  direction <- match.arg(direction)
  graph <- manynet::as_igraph(.data)
  
  if (manynet::is_twomode(.data)) {
    becent <- node_betweenness(graph, normalized = FALSE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (direction == "all") {
      if (!normalized) {
        out$nodes1 <- sum(max(becent[!mode]) - becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1) + (mode1 - 1))
        out$nodes2 <- sum(max(becent[mode]) - becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1) + (mode2 - 1))
        if (mode1 > mode2) {
          out$nodes1 <- sum(max(becent[!mode]) - becent) / (2 * (mode1 - 1) * (mode2 - 1) * (mode1 + mode2 - 1) - (mode2 - 1) * (mode1 + mode2 - 2) - 1/2 * (mode1 - mode2) * (mode1 + 3*mode2 - 3))
        }
        if (mode2 > mode1) {
          out$nodes2 <- sum(max(becent[mode]) - becent) / (2 * (mode2 - 1) * (mode1 - 1) * (mode2 + mode1 - 1) - (mode1 - 1) * (mode2 + mode1 - 2) - 1/2 * (mode2 - mode1) * (mode2 + 3*mode1 - 3))
        }
      } else if (normalized) {
        out$nodes1 <- sum(max(becent[!mode]) - becent) / ((1/2 * mode2 * (mode2 - 1) + 1/2 * (mode1 - 1)*(mode1 - 2) + (mode1 - 1) * (mode2 - 2))*(mode1 + mode2 - 1) + (mode1 - 1))
        out$nodes2 <- sum(max(becent[mode]) - becent) / ((1/2 * mode1 * (mode1 - 1) + 1/2 * (mode2 - 1)*(mode2 - 2) + (mode2 - 1) * (mode1 - 2))*(mode2 + mode1 - 1) + (mode2 - 1))
        if (mode1 > mode2) {
          becent <- node_betweenness(graph, normalized = TRUE)
          out$nodes1 <- sum(max(becent[!mode]) - becent) / ((mode1 + mode2 - 1) - (((mode2 - 1)*(mode1 + mode2 - 2) + 1/2*(mode1 - mode2)*(mode1 + (3*mode2) - 3)) / (1/2*(mode1*(mode1 - 1)) + 1/2*(mode2 - 1) * (mode2 - 2) + (mode1 - 1) * (mode2 - 1))))
        }
        if (mode2 > mode1) {
          becent <- node_betweenness(graph, normalized = TRUE)
          out$nodes2 <- sum(max(becent[mode]) - becent) / ((mode1 + mode2 - 1)*((mode1 - 1)*(mode1 + mode2 - 2) / 2*(mode1 - 1)*(mode2 - 1)))
        }
      }
    } else if (direction == "in") {
      out$nodes1 <- sum(max(becent[!mode]) - becent[!mode])/((mode1 - 1)*(1/2*mode2*(mode2 - 1) + 1/2*(mode1 - 1)*(mode1 - 2) + (mode1 - 1)*(mode2 - 1)))
      out$nodes2 <- sum(max(becent[mode]) - becent[mode])/((mode2 - 1)*(1/2*mode1*(mode1 - 1) + 1/2 * (mode2 - 1) * (mode2 - 2) + (mode2 - 1) * (mode1 - 1)))
      if (mode1 > mode2) {
        out$nodes1 <- sum(max(becent[!mode]) - becent[!mode]) / (2 * (mode1 - 1)^2 * (mode2 - 1))
      }
      if (mode2 > mode1) {
        out$nodes2 <- sum(max(becent[mode]) - becent[mode]) / (2 * (mode2 - 1)^2 * (mode1 - 1))
      }
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_betw(graph = graph)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}

# Closeness-like centralities ####

#' Measures of closeness-like centrality and centralisation
#' @description
#'   These functions calculate common closeness-related centrality measures 
#'   that rely on path-length for one- and two-mode networks:
#'   
#'   - `node_closeness()` measures the closeness centrality of nodes in a 
#'   network.
#'   - `node_reach()` measures nodes' reach centrality,
#'   or how many nodes they can reach within _k_ steps.
#'   - `node_harmonic()` measures nodes' harmonic centrality or valued 
#'   centrality, which is thought to behave better than reach centrality 
#'   for disconnected networks.
#'   - `node_information()` measures nodes' information centrality or 
#'   current-flow closeness centrality.
#'   - `node_eccentricity()` measures nodes' eccentricity or maximum distance
#'   from another node in the network.
#'   - `node_distance()` measures nodes' geodesic distance from or to a 
#'   given node.
#'   - `tie_closeness()` measures the closeness of each tie to other ties 
#'   in the network.
#'   - `net_closeness()` measures a network's closeness centralization.
#'   - `net_reach()` measures a network's reach centralization.
#'   - `net_harmonic()` measures a network's harmonic centralization.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures by default,
#'   including for two-mode networks.
#' @name measure_central_close
#' @family centrality
#' @family measures
#' @inheritParams measure_central_degree
NULL

#' @rdname measure_central_close
#' @param cutoff Maximum path length to use during calculations.
#' @section Closeness centrality: 
#'   Closeness centrality or status centrality is defined as the reciprocal of 
#'   the farness or distance, \eqn{d}, 
#'   from a node to all other nodes in the network:
#'   \deqn{C_C(i) = \frac{1}{\sum_j d(i,j)}}
#'   When (more commonly) normalised, the numerator is instead \eqn{N-1}.
#' @references
#' ## On closeness centrality
#' Bavelas, Alex. 1950. 
#' "Communication Patterns in Task‐Oriented Groups". 
#' _The Journal of the Acoustical Society of America_, 22(6): 725–730.
#' \doi{10.1121/1.1906679}
#' 
#' Harary, Frank. 1959. 
#' "Status and Contrastatus". 
#' _Sociometry_, 22(1): 23–43. 
#' \doi{10.2307/2785610}
#' @examples
#' node_closeness(ison_southern_women)
#' @export
node_closeness <- function(.data, normalized = TRUE, 
                           direction = "out", cutoff = NULL){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (manynet::is_twomode(graph) & normalized){
    # farness <- rowSums(igraph::distances(graph = graph))
    closeness <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction)
    other_set_size <- ifelse(igraph::V(graph)$type, sum(!igraph::V(graph)$type), sum(igraph::V(graph)$type))
    set_size <- ifelse(igraph::V(graph)$type, sum(igraph::V(graph)$type), sum(!igraph::V(graph)$type))
    out <- closeness/(1/(other_set_size+2*set_size-2))
    } else {
      cutoff <- if (is.null(cutoff)) -1 else cutoff
      out <- igraph::closeness(graph = graph, vids = igraph::V(graph), mode = direction, 
                               cutoff = cutoff, weights = weights, normalized = normalized)
    }
  out <- make_node_measure(out, .data)
  out
} 

#' @rdname measure_central_close 
#' @section Harmonic centrality:
#'   Harmonic centrality or valued centrality reverses the sum and reciprocal 
#'   operations compared to closeness centrality:
#'   \deqn{C_H(i) = \sum_{i, i \neq j} \frac{1}{d(i,j)}}
#'   where \eqn{\frac{1}{d(i,j)} = 0} where there is no path between \eqn{i} and
#'   \eqn{j}. Normalization is by \eqn{N-1}. 
#'   Since the harmonic mean performs better than the arithmetic mean on
#'   unconnected networks, i.e. networks with infinite distances,
#'   harmonic centrality is to be preferred in these cases.
#' @references
#'   ## On harmonic centrality
#'   Marchiori, Massimo, and Vito Latora. 2000. 
#'   "Harmony in the small-world".
#'   _Physica A_ 285: 539-546.
#'   \doi{10.1016/S0378-4371(00)00311-3}
#'   
#'   Dekker, Anthony. 2005.
#'   "Conceptual distance in social network analysis".
#'   _Journal of Social Structure_ 6(3).
#' @export
node_harmonic <- function(.data, normalized = TRUE, cutoff = -1){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- igraph::harmonic_centrality(as_igraph(.data), # weighted if present
                                     normalized = normalized, cutoff = cutoff)
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_close 
#' @section Reach centrality: 
#'   In some cases, longer path lengths are irrelevant and 'closeness' should
#'   be defined as how many others are in a local neighbourhood.
#'   How many steps out this neighbourhood should be defined as is given by 
#'   the 'cutoff' parameter. 
#'   This is usually termed \eqn{k} or \eqn{m} in equations,
#'   which is why this is sometimes called (\eqn{m}- or) 
#'   \eqn{k}-step reach centrality:
#'   \deqn{C_R(i) = \sum_j d(i,j) \leq k}
#'   The maximum reach score is \eqn{N-1}, achieved when the node can reach all
#'   other nodes in the network in \eqn{k} steps or less,
#'   but the normalised version, \eqn{\frac{C_R}{N-1}}, is more common.
#'   Note that if \eqn{k = 1} (i.e. cutoff = 1), then this returns the node's degree.
#'   At higher cutoff reach centrality returns the size of the node's component.
#' @references
#' ## On reach centrality
#' Borgatti, Stephen P., Martin G. Everett, and J.C. Johnson. 2013. 
#' _Analyzing social networks_. 
#' London: SAGE Publications Limited.
#' @examples
#' node_reach(ison_adolescents)
#' @export
node_reach <- function(.data, normalized = TRUE, cutoff = 2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(is_weighted(.data)){
    tore <- as_matrix(.data)/mean(as_matrix(.data))
    out <- 1/tore
  } else out <- igraph::distances(as_igraph(.data))
  diag(out) <- 0
  out <- rowSums(out <= cutoff)
  if(normalized) out <- out/(net_nodes(.data)-1)
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_close 
#' @section Information centrality: 
#'   Information centrality, also known as current-flow centrality, 
#'   is a hybrid measure relating to both path-length and walk-based measures. 
#'   The information centrality of a node is the harmonic average of the 
#'   “bandwidth” or inverse path-length for all paths originating from the node.
#'   
#'   As described in the `{sna}` package, 
#'   information centrality works on an undirected but potentially weighted 
#'   network excluding isolates (which take scores of zero).
#'   It is defined as:
#'   \deqn{C_I = \frac{1}{T + \frac{\sum T - 2 \sum C_1}{|N|}}}
#'   where \eqn{C = B^-1} with \eqn{B} is a pseudo-adjacency matrix replacing
#'   the diagonal of \eqn{1-A} with \eqn{1+k},
#'   and \eqn{T} is the trace of \eqn{C} and \eqn{S_R} an arbitrary row sum 
#'   (all rows in \eqn{C} have the same sum). 
#'   
#'   Nodes with higher information centrality have a large number of short paths
#'   to many others in the network, and are thus considered to have greater
#'   control of the flow of information.
#' @references
#' ## On information centrality
#' Stephenson, Karen, and Marvin Zelen. 1989.
#' "Rethinking centrality: Methods and examples". 
#' _Social Networks_ 11(1):1-37. 
#' \doi{10.1016/0378-8733(89)90016-6}
#' 
#' Brandes, Ulrik, and Daniel Fleischer. 2005. 
#' "Centrality Measures Based on Current Flow". 
#' _Proc. 22nd Symp. Theoretical Aspects of Computer Science_ LNCS 3404: 533-544. 
#' \doi{10.1007/978-3-540-31856-9_44}
#' @export
node_information <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  thisRequires("sna")
  out <- sna::infocent(manynet::as_network(.data),
                       gmode = ifelse(manynet::is_directed(.data), "digraph", "graph"),
                       diag = manynet::is_complex(.data),
                       rescale = normalized)
  make_node_measure(out, .data)
}
  
#' @rdname measure_central_close
#' @section Eccentricity centrality: 
#'   Eccentricity centrality, graph centrality, or the Koenig number,
#'   is the (if normalized, inverse of) the distance to the furthest node:
#'   \deqn{C_E(i) = \frac{1}{max_{j \in N} d(i,j)}}
#'   where the distance from \eqn{i} to \eqn{j} is \eqn{\infty} if unconnected.
#'   As such it is only well defined for connected networks.
#' @references
#' ## On eccentricity centrality
#'   Hage, Per, and Frank Harary. 1995.
#'   "Eccentricity and centrality in networks".
#'   _Social Networks_, 17(1): 57-63.
#'   \doi{10.1016/0378-8733(94)00248-9}
#' @export
node_eccentricity <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!is_connected(.data)) 
    mnet_unavailable("Eccentricity centrality is only available for connected networks.")
  disties <- igraph::distances(as_igraph(.data))
  out <- apply(disties, 1, max)
  if(normalized) out <- 1/out
  make_node_measure(out, .data)
}

#   - `node_eccentricity()` measures nodes' eccentricity or Koenig number,
#   a measure of farness based on number of links needed to reach 
#   most distant node in the network.
# #' @rdname measure_holes 
# #' @importFrom igraph eccentricity
# #' @export
# cnode_eccentricity <- function(.data){
#  if(missing(.data)) {expect_nodes(); .data <- .G()}
#  out <- igraph::eccentricity(manynet::as_igraph(.data),
#                              mode = "out")
#  make_node_measure(out, .data)
# }

#' @rdname measure_central_close 
#' @param from,to Index or name of a node to calculate distances from or to.
#' @export
node_distance <- function(.data, from, to, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(missing(from) && missing(to)) cli::cli_abort("Either 'from' or 'to' must be specified.")
  if(!missing(from)) out <- igraph::distances(as_igraph(.data), v = from) else 
    if(!missing(to)) out <- igraph::distances(as_igraph(.data), to = to)
  if(normalized) out <- out/max(out)
  make_node_measure(out, .data)
}

#' @rdname measure_central_close 
#' @section Closeness vitality centrality: 
#'   The closeness vitality of a node is the change in the sum of all distances
#'   in a network, also known as the Wiener Index, when that node is removed.
#'   Note that the closeness vitality may be negative infinity if
#'   removing that node would disconnect the network.
#' @references
#'   Koschuetzki, Dirk, Katharina Lehmann, Leon Peeters, Stefan Richter,
#'   Dagmar Tenfelde-Podehl, and Oliver Zlotowski. 2005.
#'   "Centrality Indices", in
#'   Brandes, Ulrik, and Thomas Erlebach (eds.). 
#'   _Network Analysis: Methodological Foundations_. 
#'   Springer: Berlin, pp. 16-61.
#' @export
node_vitality <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  .data <- as_igraph(.data)
  out <- vapply(mnet_progress_nodes(.data), function(x){
    sum(igraph::distances(.data)) - sum(igraph::distances(delete_nodes(.data, x)))
  }, FUN.VALUE = numeric(1))
  if(normalized) out <- out/max(out)
  make_node_measure(out, .data)
}

#' @rdname measure_central_close 
#' @examples
#' (ec <- tie_closeness(ison_adolescents))
#' plot(ec)
#' ison_adolescents %>% mutate_ties(weight = ec) %>% 
#'    graphr()
#' @export
tie_closeness <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  edge_adj <- manynet::to_ties(.data)
  out <- node_closeness(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname measure_central_close 
#' @examples
#' net_closeness(ison_southern_women, direction = "in")
#' @export
net_closeness <- function(.data, normalized = TRUE,
                              direction = c("all", "out", "in")){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  direction <- match.arg(direction)
  graph <- manynet::as_igraph(.data)
  
  if (manynet::is_twomode(.data)) {
    clcent <- node_closeness(graph, normalized = TRUE)
    mode <- igraph::V(graph)$type
    mode1 <- length(mode) - sum(mode)
    mode2 <- sum(mode)
    out <- list()
    if (direction == "in") {
      out$nodes1 <- sum(max(clcent[!mode]) - clcent[!mode])/(((mode1 - 2)*(mode1 - 1))/(2 * mode1 - 3))
      out$nodes2 <- sum(max(clcent[mode]) - clcent[mode])/(((mode2 - 2)*(mode2 - 1))/(2 * mode2 - 3))
      if (mode1 > mode2) { #28.43
        lhs <- ((mode2 - 1)*(mode1 - 2) / (2 * mode1 - 3))
        rhs <- ((mode2 - 1)*(mode1 - mode2) / (mode1 + mode2 - 2))
        out$nodes1 <- sum(max(clcent[!mode]) - clcent[!mode])/( lhs +  rhs) # 0.2135
      }
      if (mode2 > mode1) {
        lhs <- ((mode1 - 1)*(mode2 - 2) / (2 * mode2 - 3))
        rhs <- ((mode1 - 1)*(mode2 - mode1) / (mode2 + mode1 - 2))
        out$nodes2 <- sum(max(clcent[mode]) - clcent[mode])/( lhs +  rhs)
      }
    } else {
      term1 <- 2*(mode1 - 1) * (mode2 + mode1 - 4)/(3*mode2 + 4*mode1 - 8)
      term2 <- 2*(mode1 - 1) * (mode1 - 2)/(2*mode2 + 3*mode1 - 6)
      term3 <- 2*(mode1 - 1) * (mode2 - mode1 + 1)/(2*mode2 + 3*mode1 - 4)
      out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3)
      term1 <- 2*(mode2 - 1) * (mode1 + mode2 - 4)/(3*mode1 + 4*mode2 - 8)
      term2 <- 2*(mode2 - 1) * (mode2 - 2)/(2*mode1 + 3*mode2 - 6)
      term3 <- 2*(mode2 - 1) * (mode1 - mode2 + 1)/(2*mode1 + 3*mode2 - 4)
      out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3)
      
      if (mode1 > mode2) {
        term1 <- 2*(mode2 - 1) * (mode2 + mode1 - 2) / (3 * mode2 + 4 * mode1 - 8)
        term2 <- 2*(mode1 - mode2) * (2 * mode2 - 1) / (5 * mode2 + 2 * mode1 - 6)
        term3 <- 2*(mode2 - 1) * (mode1 - 2) / (2 * mode2 + 3 * mode1 - 6)
        term4 <- 2 * (mode2 - 1) / (mode1 + 4 * mode2 - 4)
        out$nodes1 <- sum(max(clcent[!mode]) - clcent) / sum(term1, term2, term3, term4)
      }
      if (mode2 > mode1) {
        term1 <- 2*(mode1 - 1) * (mode1 + mode2 - 2) / (3 * mode1 + 4 * mode2 - 8)
        term2 <- 2*(mode2 - mode1) * (2 * mode1 - 1) / (5 * mode1 + 2 * mode2 - 6)
        term3 <- 2*(mode1 - 1) * (mode2 - 2) / (2 * mode1 + 3 * mode2 - 6)
        term4 <- 2 * (mode1 - 1) / (mode2 + 4 * mode1 - 4)
        out$nodes2 <- sum(max(clcent[mode]) - clcent) / sum(term1, term2, term3, term4)
      }
    }
    out <- c("Mode 1" = out$nodes1, "Mode 2" = out$nodes2)
  } else {
    out <- igraph::centr_clo(graph = graph,
                             mode = direction,
                             normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}

#' @rdname measure_central_close 
#' @export
net_reach <- function(.data, normalized = TRUE, cutoff = 2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  reaches <- node_reach(.data, normalized = FALSE, cutoff = cutoff)
  out <- sum(max(reaches) - reaches)
  if(normalized) out <- out / sum(manynet::net_nodes(.data) - reaches)
  make_network_measure(out, .data)
}

#' @rdname measure_central_close
#' @export
net_harmonic <- function(.data, normalized = TRUE, cutoff = 2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  harm <- node_harmonic(.data, normalized = FALSE, cutoff = cutoff)
  out <- sum(max(harm) - harm)
  if(normalized) out <- out / sum(manynet::net_nodes(.data) - harm)
  make_network_measure(out, .data)
}

# Eigenvector-like centralities ####

#' Measures of eigenvector-like centrality and centralisation
#' @description
#'   These functions calculate common eigenvector-related centrality 
#'   measures, or walk-based eigenmeasures, for one- and two-mode networks:
#'   
#'   - `node_eigenvector()` measures the eigenvector centrality of nodes 
#'   in a network.
#'   - `node_power()` measures the Bonacich, beta, or power centrality of 
#'   nodes in a network.
#'   - `node_alpha()` measures the alpha or Katz centrality of nodes in a 
#'   network.
#'   - `node_pagerank()` measures the pagerank centrality of nodes in a network.
#'   - `node_hub()` measures how well nodes in a network serve as hubs pointing 
#'   to many authorities.
#'   - `node_authority()` measures how well nodes in a network serve as 
#'   authorities from many hubs.
#'   - `tie_eigenvector()` measures the eigenvector centrality of ties in a 
#'   network.
#'   - `net_eigenvector()` measures the eigenvector centralization for a 
#'   network.
#'   
#'   All measures attempt to use as much information as they are offered,
#'   including whether the networks are directed, weighted, or multimodal.
#'   If this would produce unintended results, 
#'   first transform the salient properties using e.g. [to_undirected()] functions.
#'   All centrality and centralization measures return normalized measures 
#'   by default, including for two-mode networks.
#' @name measure_central_eigen
#' @family centrality
#' @family measures
#' @inheritParams measure_central_degree
NULL

#' @rdname measure_central_eigen
#' @section Eigenvector centrality:
#'   Eigenvector centrality operates as a measure of a node's influence in a network.
#'   The idea is that being connected to well-connected others results in a higher score.
#'   Each node's eigenvector centrality can be defined as:
#'   \deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j}
#'   where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
#'   and \eqn{\lambda} is a constant representing the principal eigenvalue.
#'   Rather than performing this iteration,
#'   most routines solve the eigenvector equation \eqn{Ax = \lambda x}.
#'   Note that since `{igraph}` v2.1.1,
#'   the values will always be rescaled so that the maximum is 1.
#' @param scale Logical scalar, whether to rescale the vector so the maximum score is 1. 
#' @details
#'   We use `{igraph}` routines behind the scenes here for consistency and because they are often faster.
#'   For example, `igraph::eigencentrality()` is approximately 25% faster than `sna::evcent()`.
#' @references 
#'   ## On eigenvector centrality
#'   Bonacich, Phillip. 1991. 
#'   “Simultaneous Group and Individual Centralities.” 
#'   _Social Networks_ 13(2):155–68. 
#'   \doi{10.1016/0378-8733(91)90018-O}
#' @examples
#' node_eigenvector(ison_southern_women)
#' @return A numeric vector giving the eigenvector centrality measure of each node.
#' @export 
node_eigenvector <- function(.data, normalized = TRUE, scale = TRUE){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  if(!normalized) mnet_info("This function always returns a normalized value now.")
  if(!scale) mnet_info("This function always returns a scaled value now.")
  
  if(!manynet::is_connected(.data)) 
    cli::cli_alert_warning("Unconnected networks will only allow nodes from one component to have non-zero eigenvector scores.")
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::eigen_centrality(graph = graph, 
                                    directed = manynet::is_directed(graph),
                                    options = igraph::arpack_defaults())$vector
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::eigen_centrality(graph = eigen1, 
                                       directed = manynet::is_directed(eigen1),
                                       options = igraph::arpack_defaults())$vector
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::eigen_centrality(graph = eigen2, 
                                       directed = manynet::is_directed(eigen2),
                                       options = igraph::arpack_defaults())$vector
    out <- c(eigen1, eigen2)
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_eigen
#' @param exponent Decay rate or attentuation factor for 
#'   the Bonacich power centrality score.
#'   Can be positive or negative.
#' @section Power or beta (or Bonacich) centrality:
#'   Power centrality includes an exponent that weights contributions to a node's
#'   centrality based on how far away those other nodes are.
#'   \deqn{c_b(i) = \sum A(i,j) (\alpha = \beta c(j))}
#'   Where \eqn{\beta} is positive, this means being connected to central people
#'   increases centrality.
#'   Where \eqn{\beta} is negative, this means being connected to central people
#'   decreases centrality 
#'   (and being connected to more peripheral actors increases centrality).
#'   When \eqn{\beta = 0}, this is the outdegree.
#'   \eqn{\alpha} is calculated to make sure the root mean square equals 
#'   the network size.
#' @references 
#' ## On power centrality
#'   Bonacich, Phillip. 1987. 
#'   “Power and Centrality: A Family of Measures.” 
#'   _The American Journal of Sociology_, 92(5): 1170–82.
#' \doi{10.1086/228631}.
#' @importFrom igraph power_centrality
#' @examples
#' node_power(ison_southern_women, exponent = 0.5)
#' @return A numeric vector giving each node's power centrality measure.
#' @export 
node_power <- function(.data, normalized = TRUE, scale = FALSE, exponent = 1){
  
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  weights <- `if`(manynet::is_weighted(.data), 
                  manynet::tie_weights(.data), NA)
  graph <- manynet::as_igraph(.data)
  
  # Do the calculations
  if (!manynet::is_twomode(graph)){
    out <- igraph::power_centrality(graph = graph, 
                                    exponent = exponent,
                                    rescale = scale)
    if (normalized) out <- out / sqrt(1/2)
  } else {
    eigen1 <- manynet::to_mode1(graph)
    eigen1 <- igraph::power_centrality(graph = eigen1, 
                                       exponent = exponent,
                                       rescale = scale)
    eigen2 <- manynet::to_mode2(graph)
    eigen2 <- igraph::power_centrality(graph = eigen2, 
                                       exponent = exponent,
                                       rescale = scale)
    out <- c(eigen1, eigen2)
    if (normalized) out <- out / sqrt(1/2)
  }
  out <- make_node_measure(out, .data)
  out
}

#' @rdname measure_central_eigen 
#' @param alpha A constant that trades off the importance of external influence against the importance of connection.
#'   When \eqn{\alpha = 0}, only the external influence matters.
#'   As \eqn{\alpha} gets larger, only the connectivity matters and we reduce to eigenvector centrality.
#'   By default \eqn{\alpha = 0.85}.
#' @section Alpha centrality:
#'   Alpha or Katz (or Katz-Bonacich) centrality operates better than eigenvector centrality
#'   for directed networks.
#'   Eigenvector centrality will return 0s for all nodes not in the main strongly-connected component.
#'   Each node's alpha centrality can be defined as:
#'   \deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j + e_i}
#'   where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
#'   \eqn{\lambda} is a constant representing the principal eigenvalue,
#'   and \eqn{e_i} is some external influence used to ensure that even nodes beyond the main
#'   strongly connected component begin with some basic influence.
#'   Note that many equations replace \eqn{\frac{1}{\lambda}} with \eqn{\alpha},
#'   hence the name.
#'
#'   For example, if \eqn{\alpha = 0.5}, then each direct connection (or alter) would be worth \eqn{(0.5)^1 = 0.5},
#'   each secondary connection (or tertius) would be worth \eqn{(0.5)^2 = 0.25},
#'   each tertiary connection would be worth \eqn{(0.5)^3 = 0.125}, and so on.
#'
#'   Rather than performing this iteration though,
#'   most routines solve the equation \eqn{x = (I - \frac{1}{\lambda} A^T)^{-1} e}.
#' @importFrom igraph alpha_centrality
#' @references 
#' ## On alpha centrality
#'   Katz, Leo 1953. 
#'   "A new status index derived from sociometric analysis". 
#'   _Psychometrika_. 18(1): 39–43.
#' 
#'   Bonacich, P. and Lloyd, P. 2001. 
#'   “Eigenvector-like measures of centrality for asymmetric relations” 
#'   _Social Networks_. 23(3):191-201.
#' @export 
node_alpha <- function(.data, alpha = 0.85){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::alpha_centrality(manynet::as_igraph(.data), 
                                             alpha = alpha),
                    .data)
}

#' @rdname measure_central_eigen 
#' @references 
#' ## On pagerank centrality
#'   Brin, Sergey and Page, Larry. 1998.
#'   "The anatomy of a large-scale hypertextual web search engine".
#'   _Proceedings of the 7th World-Wide Web Conference_. Brisbane, Australia.
#' @export 
node_pagerank <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::page_rank(manynet::as_igraph(.data)),
                    .data)
}
  
#' @rdname measure_central_eigen 
#' @references 
#' ## On hub and authority centrality
#'   Kleinberg, Jon. 1999.
#'   "Authoritative sources in a hyperlinked environment". 
#'   _Journal of the ACM_ 46(5): 604–632.
#'   \doi{10.1145/324133.324140}
#' @export 
node_authority <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::authority_score(manynet::as_igraph(.data))$vector,
                    .data)
}

#' @rdname measure_central_eigen 
#' @export 
node_hub <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::hub_score(manynet::as_igraph(.data))$vector,
                    .data)
}

#' @rdname measure_central_eigen
#' @examples 
#' tie_eigenvector(ison_adolescents)
#' @export
tie_eigenvector <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  edge_adj <- manynet::to_ties(.data)
  out <- node_eigenvector(edge_adj, normalized = normalized)
  class(out) <- "numeric"
  out <- make_tie_measure(out, .data)
  out
}

#' @rdname measure_central_eigen 
#' @examples
#' net_eigenvector(ison_southern_women)
#' @export
net_eigenvector <- function(.data, normalized = TRUE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if (manynet::is_twomode(.data)) {
    out <- c(igraph::centr_eigen(manynet::as_igraph(manynet::to_mode1(.data)), 
                                 normalized = normalized)$centralization,
             igraph::centr_eigen(manynet::as_igraph(manynet::to_mode2(.data)), 
                                 normalized = normalized)$centralization)
  } else {
    out <- igraph::centr_eigen(manynet::as_igraph(.data), 
                               normalized = normalized)$centralization
  }
  out <- make_network_measure(out, .data, call = deparse(sys.call()))
  out
}


