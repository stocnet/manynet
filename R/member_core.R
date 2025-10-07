#' Core-periphery clustering algorithms
#' @description
#'   These functions identify nodes belonging to (some level of) the core of a network:
#'   
#'   - `node_is_universal()` identifies whether nodes are adjacent to all other
#'   nodes in the network.
#'   - `node_is_core()` identifies whether nodes belong to the core of the 
#'   network, as opposed to the periphery.
#'   - `node_in_core()` categorizes nodes into two or more core/periphery
#'   categories based on their coreness.
#'   - `node_coreness()` returns a continuous measure of how closely each node
#'   resembles a typical core node.
#'   - `node_kcoreness()` assigns nodes to their level of k-coreness.
#' 
#' @inheritParams mark_is
#' @param method Which method to use to identify cores and periphery.
#'   By default this is "degree", 
#'   which relies on the heuristic that high degree nodes are more likely to be in the core.
#'   An alternative is "eigenvector", which instead begins with high eigenvector nodes.
#'   Other methods, such as a genetic algorithm, CONCOR, and Rombach-Porter,
#'   can be added if there is interest.
#' @name mark_core
#' @family memberships
NULL

#' @rdname mark_core
#' @section Universal/dominating node: 
#'   A universal node is adjacent to all other nodes in the network.
#'   It is also sometimes called the dominating vertex because it represents
#'   a one-element dominating set.
#'   A network with a universal node is called a cone, and its universal node
#'   is called the apex of the cone.
#'   A classic example of a cone is a star graph,
#'   but friendship, wheel, and threshold graphs are also cones.
#' @examples
#' node_is_universal(create_star(11))
#' @export
node_is_universal <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  net <- to_undirected(to_unweighted(.data))
  make_node_mark(node_deg(net)==(net_nodes(net)-1), .data)
}

#' @rdname mark_core
#' @section Core-periphery: 
#'   This function is used to identify which nodes should belong to the core,
#'   and which to the periphery.
#'   It seeks to minimize the following quantity:
#'   \deqn{Z(S_1) = \sum_{(i<j)\in S_1} \textbf{I}_{\{A_{ij}=0\}} + \sum_{(i<j)\notin S_1} \textbf{I}_{\{A_{ij}=1\}}}
#'   where nodes \eqn{\{i,j,...,n\}} are ordered in descending degree,
#'   \eqn{A} is the adjacency matrix,
#'   and the indicator function is 1 if the predicate is true or 0 otherwise.
#'   Note that minimising this quantity maximises density in the core block
#'   and minimises density in the periphery block;
#'   it ignores ties between these blocks.
#' @references
#' ## On core-periphery partitioning
#' Borgatti, Stephen P., & Everett, Martin G. 1999. 
#' "Models of core /periphery structures". 
#' _Social Networks_, 21, 375–395. 
#' \doi{10.1016/S0378-8733(99)00019-2}
#' 
#' Lip, Sean Z. W. 2011. 
#' “A Fast Algorithm for the Discrete Core/Periphery Bipartitioning Problem.”
#' \doi{10.48550/arXiv.1102.5511}
#' @examples 
#' node_is_core(ison_adolescents)
#' #ison_adolescents %>% 
#' #   mutate(corep = node_is_core()) %>% 
#' #   graphr(node_color = "corep")
#' @export
node_is_core <- function(.data, method = c("degree", "eigenvector")){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  method <- match.arg(method)
  if(is_directed(.data)) warning("Asymmetric core-periphery not yet implemented.")
  if(method == "degree"){
    degi <- node_degree(.data, normalized = FALSE, 
                        alpha = ifelse(is_weighted(.data), 1, 0))
  } else if (method == "eigenvector") {
    degi <- node_eigenvector(.data, normalized = FALSE)
  } else snet_abort("This function expects either 'degree' or 'eigenvector' method to be specified.")
  nord <- order(degi, decreasing = TRUE)
  zbest <- net_nodes(.data)*3
  kbest <- 0
  z <- 1/2*sum(degi)
  for(k in 1:(net_nodes(.data)-1)){
    z <- z + k - 1 - degi[nord][k]
    if(z < zbest){
      zbest <- z
      kbest <- k
    }
  }
  out <- ifelse(seq_len(net_nodes(.data)) %in% nord[seq_len(kbest)],
         1,2)
  make_node_mark(out==1, .data)
}

#' @rdname mark_core
#' @references
#' ## On k-coreness
#' Seidman, Stephen B. 1983. 
#' "Network structure and minimum degree". 
#' _Social Networks_, 5(3), 269-287.
#' \doi{10.1016/0378-8733(83)90028-X}
#' 
#' Batagelj, Vladimir, and Matjaz Zaversnik. 2003. 
#' "An O(m) algorithm for cores decomposition of networks". 
#' _arXiv preprint_ cs/0310049.
#' \doi{10.48550/arXiv.cs/0310049}
#' @examples
#' node_kcoreness(ison_adolescents)
#' @export
node_kcoreness <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!manynet::is_graph(.data)) .data <- manynet::as_igraph(.data)
  out <- igraph::coreness(.data)
  make_node_measure(out, .data)
}

#' @rdname mark_core
#' @examples
#' node_coreness(ison_adolescents)
#' @export
node_coreness <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  A <- as_matrix(.data)
  n <- nrow(A)
  obj_fun <- function(c) {
    ideal <- outer(c, c)
    val <- suppressWarnings(cor(as.vector(A), as.vector(ideal)))
    if (!is.finite(val)) return(1e6)  # Penalize non-finite values
    return(-val)  # Negative for maximization
  }
  # Initial guess: all nodes have coreness 0.5
  init <- rep(0.5, n)
  result <- stats::optim(init, obj_fun, method = "L-BFGS-B", 
                         lower = 0, upper = 1)
  make_node_measure(result$par, .data)
}

#' @rdname mark_core
#' @param groups Number of categories to create. Must be at least 2 and at most
#'   the number of nodes in the network. Default is 3.
#' @param cluster_by Method to use to create the categories.
#'   One of "bins" (equal-width bins), "quantiles" (quantile-based bins),
#'   or "kmeans" (k-means clustering). Default is "bins".
#' @section Core-periphery categories:
#'   This function categorizes nodes based on their coreness into a specified
#'   number of groups. The groups are labeled as "Core", "Semi-core",
#'   "Semi-periphery", and "Periphery" depending on the number of groups
#'   specified.
#'   The categorization can be done using different methods: equal-width bins,
#'   quantile-based bins, or k-means clustering.
#' @references
#' ## On core-periphery categorization
#' Wallerstein, Immanuel. 1974.
#' "Dependence in an Interdependent World: The Limited Possibilities of Transformation Within the Capitalist World Economy."
#' _African Studies Review_, 17(1), 1-26.
#' \doi{https://doi.org/10.2307/523574}
#' @examples
#' node_in_core(ison_adolescents)
#' @export
node_in_core <- function(.data, groups = 3,
                         cluster_by = c("bins","quantiles","kmeans")) {
  if (groups < 2) snet_abort("Number of categories must be at least 2")
  if (groups > net_nodes(.data)) snet_abort("There cannot be more categories than nodes.")
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  contin <- node_coreness(.data)
  cluster_by <- match.arg(cluster_by)
  out <- switch(cluster_by,
                bins = cut(as.numeric(contin), breaks = groups, labels = FALSE),
                quantiles = as.numeric(cut(as.numeric(contin),
                                           breaks = stats::quantile(as.numeric(contin),
                                                                    probs = seq(0, 1, length.out = groups + 1)),
                                           include.lowest = TRUE, labels = FALSE)),
                kmeans = stats::kmeans(as.numeric(contin), centers = groups)$cluster
  )
  
  if (groups == 2) core_labels <- c("Core", "Periphery")
  if (groups == 3) core_labels <- c("Core", "Semi-periphery", "Periphery")
  if (groups == 4) core_labels <- c("Core", "Semi-core", "Semi-periphery", "Periphery")
  if (groups >= 5){
    n_middle <- groups - 2
    middle <- character(n_middle)
    
    for (i in seq_len(n_middle)) {
      if (i %% 2 == 1) {
        middle[i] <- paste0("Semi-periphery-", (i + 1) %/% 2)
      } else {
        middle[i] <- paste0("Semi-core-", i %/% 2)
      }
    }
    middle <- middle[order(middle)]
    
    core_labels <- c("Core", middle, "Periphery")
    if(groups == 5) core_labels[2] <- "Semi-core"
  } 
  out <- rev(core_labels)[out]
  make_node_member(out, .data)
}