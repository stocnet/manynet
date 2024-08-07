#' Measures of network closure
#'
#' @description
#'   These functions offer methods for summarising the closure in configurations 
#'   in one-, two-, and three-mode networks:
#'   
#'   - `net_reciprocity()` measures reciprocity in a (usually directed) network.
#'   - `node_reciprocity()` measures nodes' reciprocity.
#'   - `net_transitivity()` measures transitivity in a network.
#'   - `node_transitivity()` measures nodes' transitivity.
#'   - `net_equivalency()` measures equivalence or reinforcement 
#'   in a (usually two-mode) network.
#'   - `net_congruency()` measures congruency across two two-mode networks.
#'   
#' @details 
#' For one-mode networks, shallow wrappers of igraph versions exist via 
#' `net_reciprocity` and `net_transitivity`.
#' 
#' For two-mode networks, `net_equivalency` calculates the proportion of three-paths in the network
#' that are closed by fourth tie to establish a "shared four-cycle" structure.
#' 
#' For three-mode networks, `net_congruency` calculates the proportion of three-paths 
#' spanning two two-mode networks that are closed by a fourth tie to establish a 
#' "congruent four-cycle" structure.
#' @inheritParams is
#' @param object2 Optionally, a second (two-mode) matrix, igraph, or tidygraph
#' @param method For reciprocity, either `default` or `ratio`.
#'   See `?igraph::reciprocity`
#' @name measure_closure
#' @family measures
#' @references 
#' Robins, Garry L, and Malcolm Alexander. 2004. 
#' Small worlds among interlocking directors: Network structure and distance in bipartite graphs. 
#' \emph{Computational & Mathematical Organization Theory} 10(1): 69–94.
#' \doi{10.1023/B:CMOT.0000032580.12184.c0}.
#' 
#' Knoke, David, Mario Diani, James Hollway, and Dimitris C Christopoulos. 2021. 
#' \emph{Multimodal Political Networks}. 
#' Cambridge University Press. Cambridge University Press.
#' \doi{10.1017/9781108985000}
NULL

#' @rdname measure_closure 
#' @importFrom igraph reciprocity
#' @examples
#' net_reciprocity(ison_southern_women)
#' @export
net_reciprocity <- function(.data, method = "default") {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_network_measure(igraph::reciprocity(manynet::as_igraph(.data), mode = method), 
                       .data)
}

#' @rdname measure_closure 
#' @examples
#' node_reciprocity(to_unweighted(ison_networkers))
#' @export
node_reciprocity <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- manynet::as_matrix(.data)
  make_node_measure(rowSums(out * t(out))/rowSums(out), 
                    .data)
}

#' @rdname measure_closure 
#' @importFrom igraph transitivity
#' @examples
#' net_transitivity(ison_adolescents)
#' @export
net_transitivity <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_network_measure(igraph::transitivity(manynet::as_igraph(.data)), 
                       .data)
}

#' @rdname measure_closure 
#' @examples
#' node_transitivity(ison_adolescents)
#' @export
node_transitivity <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  make_node_measure(igraph::transitivity(manynet::as_igraph(.data), 
                                         type = "local"), 
                    .data)
}

#' @rdname measure_closure
#' @section Equivalency: 
#'   The `net_equivalency()` function calculates the Robins and Alexander (2004) 
#'   clustering coefficient for two-mode networks.
#'   Note that for weighted two-mode networks, the result is divided by the average tie weight.
#' @examples
#' net_equivalency(ison_southern_women)
#' @export
net_equivalency <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if (manynet::is_twomode(.data)) {
    mat <- manynet::as_matrix(.data)
    c <- ncol(mat)
    indegrees <- colSums(mat)
    twopaths <- crossprod(mat)
    diag(twopaths) <- 0
    output <- sum(twopaths * (twopaths - 1)) /
      (sum(twopaths * (twopaths - 1)) +
         sum(twopaths *
             (matrix(indegrees, c, c) - twopaths)))
    if (is.nan(output)) output <- 1
    if(manynet::is_weighted(.data)) output <- output / mean(mat[mat>0])
  } else stop("This function expects a two-mode network")
  make_network_measure(output, .data)
}

#' @rdname measure_closure 
#' @export
net_congruency <- function(.data, object2){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(missing(.data) | missing(object2)) stop("This function expects two two-mode networks")
  if(!manynet::is_twomode(.data) | !manynet::is_twomode(object2)) stop("This function expects two two-mode networks")
  if(manynet::net_dims(.data)[2] != manynet::net_dims(object2)[1]) 
    stop(paste("This function expects the number of nodes",
    "in the second mode of the first network", "to be the same as the number of nodes",
    "in the first mode of the second network."))
  mat1 <- manynet::as_matrix(.data)
  mat2 <- manynet::as_matrix(object2)
  connects <- ncol(mat1)
  twopaths1 <- crossprod(mat1)
  indegrees <- diag(twopaths1)
  diag(twopaths1) <- 0
  twopaths2 <- tcrossprod(mat2)
  outdegrees <- diag(twopaths2)
  diag(twopaths2) <- 0
  twopaths <- twopaths1 + twopaths2
  degrees <- indegrees + outdegrees
  output <- sum(twopaths * (twopaths - 1)) /
    (sum(twopaths * (twopaths - 1)) +
       sum(twopaths *
             (matrix(degrees, connects, connects) - twopaths)))
  if (is.nan(output)) output <- 1
  make_network_measure(output, .data)
}
