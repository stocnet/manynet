#' Graph theoretic dimensions of hierarchy
#' 
#' @description
#'   These functions, together with `net_reciprocity()`, are used jointly to
#'   measure how hierarchical a network is:
#'   
#'   - `net_connectedness()` measures the proportion of dyads in the network
#'   that are reachable to one another, 
#'   or the degree to which network is a single component.
#'   - `net_efficiency()` measures the Krackhardt efficiency score.
#'   - `net_upperbound()` measures the Krackhardt (least) upper bound score.
#' 
#' @inheritParams mark_is
#' @name measure_hierarchy
#' @family measures
#' @references
#' ## On hierarchy
#' Krackhardt, David. 1994. 
#' Graph theoretical dimensions of informal organizations. 
#' In Carley and Prietula (eds) _Computational Organizational Theory_, 
#' Hillsdale, NJ: Lawrence Erlbaum Associates. Pp. 89-111. 
#' 
#' Everett, Martin, and David Krackhardt. 2012. 
#' “A second look at Krackhardt's graph theoretical dimensions of informal organizations.”
#' _Social Networks_, 34: 159-163.
#' \doi{10.1016/j.socnet.2011.10.006}
#' @examples 
#' net_connectedness(ison_networkers)
#' 1 - net_reciprocity(ison_networkers)
#' net_efficiency(ison_networkers)
#' net_upperbound(ison_networkers)
NULL

#' @rdname measure_hierarchy 
#' @export
net_by_hierarchy <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  out <- data.frame(Connectedness = net_connectedness(.data),
                    InvReciprocity = 1 - net_reciprocity(.data),
                    Efficiency = net_efficiency(.data),
                    LeastUpperBound = net_upperbound(.data))
  make_network_motif(out, .data)
}

#' @rdname measure_hierarchy 
#' @export
net_connectedness <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  dists <- igraph::distances(as_igraph(.data))
  make_network_measure(1 - sum(dists==Inf)/sum(dists!=0),
                       .data, 
                       call = deparse(sys.call()))
}

#' @rdname measure_hierarchy 
#' @export
net_efficiency <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  degs <- node_indegree(.data, normalized = FALSE)
  out <- (net_nodes(.data)-1)/sum(degs)
  make_network_measure(out, .data, 
                       call = deparse(sys.call()))
}

#' @rdname measure_hierarchy 
#' @export
net_upperbound <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  dists <- igraph::distances(.data, mode = "in")
  dists[is.infinite(dists)] <- 0
  dists <- dists[order(rowSums(dists)), order(rowSums(dists))]
  if (max(colSums(dists > 0)) / (net_nodes(.data)-1) == 1){
    out <- 1
  } else {
    out <- apply(utils::combn(2:nrow(dists), 2), 2, 
                 function(x){
                   ubs <- dists[x,]>0
                   any(ubs[1,]*ubs[2,]==1)
                 })
    out <- sum(out)/length(out)
  }
  make_network_measure(out, .data, 
                       call = deparse(sys.call()))
}