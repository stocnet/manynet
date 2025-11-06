#' Measures of network diversity
#' 
#' @description
#'   These functions offer ways to measure the heterogeneity of an attribute
#'   across a network, within groups of a network, or the distribution of ties
#'   across this attribute:
#'   
#'   - `net_richness()` measures the number of unique categories 
#'   in a network attribute.
#'   - `node_richness()` measures the number of unique categories 
#'   of an attribute to which each node is connected.
#'   - `net_diversity()` measures the heterogeneity of ties across a network.
#'   - `node_diversity()` measures the heterogeneity of each node's
#'   local neighbourhood.
#'   - `net_heterophily()` measures how embedded nodes in the network
#'   are within groups of nodes with the same attribute.
#'   - `node_heterophily()` measures each node's embeddedness within groups
#'   of nodes with the same attribute.
#'   - `net_assortativity()` measures the degree assortativity in a network.
#'   - `net_spatial()` measures the spatial association/autocorrelation 
#'   (global Moran's I) in a network.
#'   
#' @inheritParams mark_is
#' @param attribute Name of a nodal attribute or membership vector
#'   to use as categories for the diversity measure.
#' @param clusters A nodal cluster membership vector or name of a vertex attribute.
#' @name measure_heterogeneity
#' @family measures
NULL

#' @rdname measure_heterogeneity 
#' @examples
#' net_richness(ison_networkers)
#' @export
net_richness <- function(.data, attribute){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  make_network_measure(length(unique(manynet::node_attribute(.data, attribute))),
                       .data, call = deparse(sys.call()))
}

#' @rdname measure_heterogeneity 
#' @examples
#' node_richness(ison_networkers, "Discipline")
#' @export
node_richness <- function(.data, attribute){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  out <- vapply(manynet::to_egos(.data, min_dist = 1), 
         function(x) length(unique(manynet::node_attribute(x, attribute))),
         FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @rdname measure_heterogeneity 
#' @section net_diversity:
#'    Blau's index (1977) uses a formula known also in other disciplines
#'    by other names 
#'    (Gini-Simpson Index, Gini impurity, Gini's diversity index, 
#'    Gibbs-Martin index, and probability of interspecific encounter (PIE)): 
#'    \deqn{1 - \sum\limits_{i = 1}^k {p_i^2 }}, 
#'    where \eqn{p_i} is the proportion of group members in \eqn{i}th category 
#'    and \eqn{k} is the number of categories for an attribute of interest. 
#'    This index can be interpreted as the probability that two members 
#'    randomly selected from a group would be from different categories. 
#'    This index finds its minimum value (0) when there is no variety, 
#'    i.e. when all individuals are classified in the same category. 
#'    The maximum value depends on the number of categories and 
#'    whether nodes can be evenly distributed across categories. 
#' @references 
#' ## On diversity
#'   Blau, Peter M. 1977. 
#'   _Inequality and heterogeneity_. 
#'   New York: Free Press.
#'   
#'   Page, Scott E. 2010. 
#'   _Diversity and Complexity_. 
#'   Princeton: Princeton University Press. 
#'   \doi{10.1515/9781400835140}
#' @examples
#' marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
#' net_diversity(marvel_friends, "Gender")
#' net_diversity(marvel_friends, "Attractive")
#' net_diversity(marvel_friends, "Gender", "Rich")
#' @export
net_diversity <- function(.data, attribute, clusters = NULL){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  blau <- function(features) { 1 - sum((table(features)/length(features))^2) }
  attr <- manynet::node_attribute(.data, attribute)
  make_network_measure(out, .data, call = deparse(sys.call()))
}

#' @rdname measure_heterogeneity 
#' @examples 
#' node_diversity(marvel_friends, "Gender")
#' node_diversity(marvel_friends, "Attractive")
#' @export
node_diversity <- function(.data, attribute){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  out <- vapply(igraph::ego(manynet::as_igraph(.data)),
                function(x) net_diversity(
                  igraph::induced_subgraph(manynet::as_igraph(.data), x),
                  attribute),
                FUN.VALUE = numeric(1))
  make_node_measure(out, .data)
}

#' @rdname measure_heterogeneity 
#' @section net_heterophily:
#'   Given a partition of a network into a number of mutually exclusive groups then 
#'   The E-I index is the number of ties between (or _external_) nodes 
#'   grouped in some mutually exclusive categories
#'   minus the number of ties within (or _internal_) these groups
#'   divided by the total number of ties. 
#'   This value can range from 1 to -1,
#'   where 1 indicates ties only between categories/groups and -1 ties only within categories/groups.
#' @references 
#' ## On heterophily
#'   Krackhardt, David, and Robert N. Stern. 1988. 
#'   Informal networks and organizational crises: an experimental simulation. 
#'   _Social Psychology Quarterly_ 51(2): 123-140.
#'   \doi{10.2307/2786835}
#'   
#'   McPherson, Miller, Lynn Smith-Lovin, and James M. Cook. 2001.
#'   "Birds of a Feather: Homophily in Social Networks".
#'   _Annual Review of Sociology_, 27(1): 415-444.
#'   \doi{10.1146/annurev.soc.27.1.415}
#' @examples 
#' net_heterophily(marvel_friends, "Gender")
#' net_heterophily(marvel_friends, "Attractive")
#' @export
net_heterophily <- function(.data, attribute){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- sum(m * same, na.rm = TRUE)
  nExternal <- sum(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / sum(m, na.rm = TRUE)
  make_network_measure(ei, .data, call = deparse(sys.call()))
}

#' @rdname measure_heterogeneity 
#' @examples 
#' node_heterophily(marvel_friends, "Gender")
#' node_heterophily(marvel_friends, "Attractive")
#' @export
node_heterophily <- function(.data, attribute){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  if(anyNA(attribute)){
    m[is.na(attribute),] <- NA
    m[,is.na(attribute)] <- NA
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- rowSums(m * same, na.rm = TRUE)
  nInternal[is.na(attribute)] <- NA
  nExternal <- rowSums(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / rowSums(m, na.rm = TRUE)
  make_node_measure(ei, .data)
}

#' @rdname measure_heterogeneity 
#' @importFrom igraph assortativity_degree
#' @references
#' ## On assortativity
#' Newman, Mark E.J. 2002.
#' "Assortative Mxing in Networks".
#' _Physical Review Letters_, 89(20): 208701.
#' \doi{10.1103/physrevlett.89.208701} 
#' @examples 
#' net_assortativity(ison_networkers)
#' @export
net_assortativity <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  make_network_measure(igraph::assortativity_degree(manynet::as_igraph(.data), 
                               directed = manynet::is_directed(.data)),
                     .data, call = deparse(sys.call()))
}

#' @rdname measure_heterogeneity 
#' @references
#' ## On spatial autocorrelation
#'   Moran, Patrick Alfred Pierce. 1950.
#'   "Notes on Continuous Stochastic Phenomena".
#'   _Biometrika_ 37(1): 17-23.
#'   \doi{10.2307/2332142}
#' @examples 
#' net_spatial(ison_lawfirm, "age")
#' @export
net_spatial <- function(.data, attribute){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  N <- manynet::net_nodes(.data)
  x <- manynet::node_attribute(.data, attribute)
  stopifnot(is.numeric(x))
  x_bar <- mean(x, na.rm = TRUE)
  w <- manynet::as_matrix(.data)
  W <- sum(w, na.rm = TRUE)
  I <- (N/W) * 
    (sum(w * matrix(x - x_bar, N, N) * matrix(x - x_bar, N, N, byrow = TRUE)) / 
    sum((x - x_bar)^2))
  make_network_measure(I, .data, 
                       call = deparse(sys.call()))
}
