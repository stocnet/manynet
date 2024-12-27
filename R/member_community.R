# Non-hierarchical community partitioning ####

#' Non-hierarchical community partitioning algorithms
#' 
#' @description
#'   These functions offer algorithms for partitioning
#'   networks into sets of communities:
#' 
#'   - `node_in_community()` runs either optimal or, for larger networks, 
#'   finds the algorithm that maximises modularity and returns that membership
#'   vector.
#'   - `node_in_optimal()` is a problem-solving algorithm that seeks to maximise 
#'   modularity over all possible partitions.
#'   - `node_in_partition()` is a greedy, iterative, deterministic
#'   partitioning algorithm that results in two equally-sized communities.
#'   - `node_in_infomap()` is an algorithm based on the information in random walks.
#'   - `node_in_spinglass()` is a greedy, iterative, probabilistic algorithm, 
#'   based on analogy to model from statistical physics.
#'   - `node_in_fluid()` is a propogation-based partitioning algorithm,
#'   based on analogy to model from fluid dynamics.
#'   - `node_in_louvain()` is an agglomerative multilevel algorithm that seeks to maximise 
#'   modularity over all possible partitions.
#'   - `node_in_leiden()` is an agglomerative multilevel algorithm that seeks to maximise 
#'   the Constant Potts Model over all possible partitions.
#'  
#'   The different algorithms offer various advantages in terms of computation time,
#'   availability on different types of networks, ability to maximise modularity,
#'   and their logic or domain of inspiration.
#'   
#' @inheritParams mark_is
#' @name member_community_non
#' @family memberships
NULL

#' @rdname member_community_non 
#' @section Community:
#'   This function runs through all available community detection algorithms 
#'   for a given network type, finds the algorithm that returns the
#'   largest modularity score, and returns the corresponding membership
#'   partition.
#'   Where feasible (a small enough network), the optimal problem solving
#'   technique is used to ensure the maximal modularity partition.
#' @export
node_in_community <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(net_nodes(.data)<100){
    # don't use node_in_betweenness because slow and poorer quality to optimal
    mnet_success("{.fn node_in_optimal} available and", 
                 "will return the highest modularity partition.")
    node_in_optimal(.data)
  } else {
    mnet_info("Excluding {.fn node_in_optimal} because network rather large.")
    poss_algs <- c("node_in_infomap",
                   "node_in_spinglass",
                   "node_in_fluid",
                   "node_in_louvain",
                   "node_in_leiden",
                   "node_in_greedy",
                   "node_in_eigen",
                   "node_in_walktrap")
    if(!manynet::is_connected(.data)){
      notforconnected <- c("node_in_spinglass", 
                           "node_in_fluid")
      mnet_info("Excluding {.fn {notforconnected}} because network unconnected.")
      poss_algs <- setdiff(poss_algs, notforconnected)
    }
    if(manynet::is_directed(.data)){
      notfordirected <- c("node_in_louvain", 
                          "node_in_leiden",
                          "node_in_eigen")
      mnet_info("Excluding {.fn {notfordirected}} because network directed.")
      poss_algs <- setdiff(poss_algs, notfordirected)
    }
    mnet_info("Considering each of {.fn {poss_algs}}.")
    candidates <- lapply(mnet_progress_along(poss_algs), function(comm){
      memb <- get(poss_algs[comm])(.data)
      mod <- net_modularity(.data, memb)
      list(memb, mod)
    })
    mods <- unlist(sapply(candidates, "[", 2))
    maxmod <- which.max(mods)
    mnet_success("{.fn {poss_algs[maxmod]}} returns the highest modularity ({round(mods[maxmod],3)}).")
    candidates[[maxmod]][[1]]
  }
}

#' @rdname member_community_non 
#' @section Optimal:
#'   The general idea is to calculate the modularity of all possible partitions,
#'   and choose the community structure that maximises this modularity measure.
#'   Note that this is an NP-complete problem with exponential time complexity.
#'   The guidance in the igraph package is networks of <50-200 nodes is probably fine.
#' @references
#' ## On optimal community detection
#' Brandes, Ulrik, Daniel Delling, Marco Gaertler, Robert Gorke, Martin Hoefer, Zoran Nikoloski, Dorothea Wagner. 2008.
#' "On Modularity Clustering", 
#' _IEEE Transactions on Knowledge and Data Engineering_ 20(2):172-188.
#' @examples
#' node_in_optimal(ison_adolescents)
#' @export
node_in_optimal <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(net_nodes(.data)>100) 
    cli::cli_alert_danger(paste("This algorithm may take some time", 
    "or even run out of memory on such a large network."))
  out <- igraph::cluster_optimal(manynet::as_igraph(.data)
  )$membership
  make_node_member(out, .data)
}

#' @rdname member_community_non 
#' @references
#' ## On partitioning community detection
#' Kernighan, Brian W., and Shen Lin. 1970.
#' "An efficient heuristic procedure for partitioning graphs."
#' _The Bell System Technical Journal_ 49(2): 291-307.
#' \doi{10.1002/j.1538-7305.1970.tb01770.x}
#' @examples
#' node_in_partition(ison_adolescents)
#' node_in_partition(ison_southern_women)
#' @export
node_in_partition <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  # assign groups arbitrarily
  n <- net_nodes(.data)
  group_size <- ifelse(n %% 2 == 0, n/2, (n+1)/2)
  
  # count internal and external costs of each node
  g <- as_matrix(to_multilevel(.data))
  g1 <- g[1:group_size, 1:group_size]
  g2 <- g[(group_size+1):n, (group_size+1):n]
  intergroup <- g[1:group_size, (group_size+1):n]
  
  g2.intcosts <- rowSums(g2)
  g2.extcosts <- colSums(intergroup)
  
  g1.intcosts <- rowSums(g1)
  g1.extcosts <- rowSums(intergroup)
  
  # count edge costs of each nodes
  g1.net <- g1.extcosts - g1.intcosts
  g2.net <- g2.extcosts - g2.intcosts
  
  g1.net <- sort(g1.net, decreasing = TRUE)
  g2.net <- sort(g2.net, decreasing = TRUE)
  
  # swap pairs of nodes (one from each group) that give a positive sum of net tie costs
  if(length(g1.net)!=length(g2.net)) {
    g2.net <- c(g2.net,0)
  } else {g2.net}
  
  sums <- as.integer(unname(g1.net + g2.net))
  # positions in sequence of names at which sum >= 0
  index <- which(sums >= 0 %in% sums)
  g1.newnames <- g1.names <- names(g1.net)
  g2.newnames <- g2.names <- names(g2.net)
  # make swaps based on positions in sequence
  for (i in index) {
    g1.newnames[i] <- g2.names[i]
    g2.newnames[i] <- g1.names[i]
  }
  
  # extract names of vertices in each group after swaps
  out <- ifelse(manynet::node_names(.data) %in% g1.newnames, 1, 2)
  make_node_member(out, .data)
}

#' @rdname member_community_non 
#' @section Infomap:
#'   Motivated by information theoretic principles, this algorithm tries to build 
#'   a grouping that provides the shortest description length for a random walk,
#'   where the description length is measured by the expected number of bits per node required to encode the path.
#' @param times Integer indicating number of simulations/walks used.
#'   By default, `times=50`.
#' @references
#' ## On infomap community detection
#' Rosvall, M, and C. T. Bergstrom. 2008.
#' "Maps of information flow reveal community structure in complex networks", 
#' _PNAS_ 105:1118.
#' \doi{10.1073/pnas.0706851105}
#' 
#' Rosvall, M., D. Axelsson, and C. T. Bergstrom. 2009.
#' "The map equation", 
#' _Eur. Phys. J. Special Topics_ 178: 13. 
#' \doi{10.1140/epjst/e2010-01179-1}
#' @examples
#' node_in_infomap(ison_adolescents)
#' @export
node_in_infomap <- function(.data, times = 50){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- igraph::cluster_infomap(manynet::as_igraph(.data), 
                                 nb.trials = times
  )$membership
  make_node_member(out, .data)
}

#' @rdname member_community_non 
#' @param max_k Integer constant, the number of spins to use as an upper limit
#'   of communities to be found. Some sets can be empty at the end.
#' @param resolution The Reichardt-Bornholdt “gamma” resolution parameter for modularity.
#'   By default 1, making existing and non-existing ties equally important.
#'   Smaller values make existing ties more important,
#'   and larger values make missing ties more important.
#' @section Spin-glass:
#'   This is motivated by analogy to the Potts model in statistical physics.
#'   Each node can be in one of _k_ "spin states",
#'   and ties (particle interactions) provide information about which pairs of nodes 
#'   want similar or different spin states.
#'   The final community definitions are represented by the nodes' spin states
#'   after a number of updates.
#'   A different implementation than the default is used in the case of signed networks,
#'   such that nodes connected by negative ties will be more likely found in separate communities.
#' @references
#' ## On spinglass community detection
#' Reichardt, Jorg, and Stefan Bornholdt. 2006.
#' "Statistical Mechanics of Community Detection"
#' _Physical Review E_, 74(1): 016110–14.
#' \doi{10.1073/pnas.0605965104}
#' 
#' Traag, Vincent A., and Jeroen Bruggeman. 2009.
#' "Community detection in networks with positive and negative links".
#' _Physical Review E_, 80(3): 036115.
#' \doi{10.1103/PhysRevE.80.036115}
#' @examples
#' node_in_spinglass(ison_adolescents)
#' @export
node_in_spinglass <- function(.data, max_k = 200, resolution = 1){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!igraph::is_connected(.data)) # note manynet::is_connected will return false
    mnet_unavailable("This algorithm only works for connected networks.",
                     "We suggest using `to_giant()`", 
                     "to select the largest component.") else {
      out <- igraph::cluster_spinglass(manynet::as_igraph(.data), 
                                       spins = max_k, gamma = resolution,
                                       implementation = ifelse(manynet::is_signed(.data), "neg", "orig")
      )$membership
      make_node_member(out, .data)
    }
}

#' @rdname member_community_non 
#' @section Fluid:
#'   The general idea is to observe how a discrete number of fluids interact, expand and contract, 
#'   in a non-homogenous environment, i.e. the network structure.
#'   Unlike the `{igraph}` implementation that this function wraps,
#'   this function iterates over all possible numbers of communities and returns the membership
#'   associated with the highest modularity.
#' @references
#' ## On fluid community detection
#' Parés Ferran, Dario Garcia Gasulla, Armand Vilalta, Jonatan Moreno, Eduard Ayguade, Jesus Labarta, Ulises Cortes, and Toyotaro Suzumura. 2018. 
#' "Fluid Communities: A Competitive, Scalable and Diverse Community Detection Algorithm". 
#' In: _Complex Networks & Their Applications VI_
#' Springer, 689: 229.
#' \doi{10.1007/978-3-319-72150-7_19}
#' @examples
#' node_in_fluid(ison_adolescents)
#' @export
node_in_fluid <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  .data <- as_igraph(.data)
  if (!igraph::is_connected(.data)) {
    mnet_unavailable("This algorithm only works for connected networks.",
                     "We suggest using `to_giant()`", 
                     "to select the largest component.")
  } else {
    if(is_complex(.data)){
      mnet_info("This algorithm only works for simple networks.", 
                      "Converting to simplex.")
      .data <- to_simplex(.data)
    }
    if(is_directed(.data)){
      mnet_info("This algorithm only works for undirected networks.", 
                      "Converting to undirected")
      .data <- to_undirected(.data)
    }
    mods <- vapply(seq_nodes(.data), function(x)
      igraph::modularity(.data, membership = igraph::membership(
        igraph::cluster_fluid_communities(.data, x))),
      FUN.VALUE = numeric(1))
    out <- igraph::membership(igraph::cluster_fluid_communities(
      .data, no.of.communities = which.max(mods)))
    make_node_member(out, .data)
  }
}

#' @rdname member_community_non 
#' @section Louvain:
#'   The general idea is to take a hierarchical approach to optimising the modularity criterion.
#'   Nodes begin in their own communities and are re-assigned in a local, greedy way:
#'   each node is moved to the community where it achieves the highest contribution to modularity.
#'   When no further modularity-increasing reassignments are possible, 
#'   the resulting communities are considered nodes (like a reduced graph),
#'   and the process continues.
#' @references
#' ## On Louvain community detection
#' Blondel, Vincent, Jean-Loup Guillaume, Renaud Lambiotte, Etienne Lefebvre. 2008.
#' "Fast unfolding of communities in large networks",
#' _J. Stat. Mech._ P10008.
#' @examples
#' node_in_louvain(ison_adolescents)
#' @export
node_in_louvain <- function(.data, resolution = 1){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(is_directed(.data)){
    mnet_info("This algorithm only works for undirected networks.", 
              "Converting to undirected")
    .data <- to_undirected(.data)
  }
  out <- igraph::cluster_louvain(manynet::as_igraph(.data), 
                                resolution = resolution
  )$membership
  make_node_member(out, .data)
}

#' @rdname member_community_non 
#' @section Leiden:
#'   The general idea is to optimise the Constant Potts Model, 
#'   which does not suffer from the resolution limit, instead of modularity.
#'   As outlined in the `{igraph}` package, 
#'   the Constant Potts Model object function is:
#'   
#'   \deqn{\frac{1}{2m} \sum_{ij}(A_{ij}-\gamma n_i n_j)\delta(\sigma_i, \sigma_j)}
#'   
#'   where _m_ is the total tie weight, 
#'   \eqn{A_{ij}} is the tie weight between _i_ and _j_,
#'   \eqn{\gamma} is the so-called resolution parameter,
#'   \eqn{n_i} is the node weight of node _i_,
#'   and \eqn{\delta(\sigma_i, \sigma_j) = 1} if and only if
#'   _i_ and _j_ are in the same communities and 0 otherwise.
#'   Compared to the Louvain method, the Leiden algorithm additionally
#'   tries to avoid unconnected communities.
#' @references
#' ## On Leiden community detection
#' Traag, Vincent A., Ludo Waltman, and Nees Jan van Eck. 2019. 
#' "From Louvain to Leiden: guaranteeing well-connected communities", 
#' _Scientific Reports_, 9(1):5233. 
#' \doi{10.1038/s41598-019-41695-z}
#' @examples
#' node_in_leiden(ison_adolescents)
#' @export
node_in_leiden <- function(.data, resolution = 1){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(is_directed(.data)){
    mnet_info("This algorithm only works for undirected networks.", 
              "Converting to undirected")
    .data <- to_undirected(.data)
  }
  if(is_weighted(.data)){ # Traag resolution default
    n <- net_nodes(.data)
    resolution <- sum(tie_weights(.data))/(n*(n - 1)/2)
  }
  out <- igraph::cluster_leiden(as_igraph(.data), 
                                resolution_parameter = resolution
  )$membership
  make_node_member(out, .data)
}

# Hierarchical community partitioning ####

#' Hierarchical community partitioning algorithms
#' 
#' @description
#'   These functions offer algorithms for hierarchically clustering
#'   networks into communities. Since all of the following are hierarchical,
#'   their dendrograms can be plotted:
#' 
#'   - `node_in_betweenness()` is a hierarchical, decomposition algorithm
#'   where edges are removed in decreasing order of the number of
#'   shortest paths passing through the edge.
#'   - `node_in_greedy()` is a hierarchical, agglomerative algorithm, 
#'   that tries to optimize modularity in a greedy manner.
#'   - `node_in_eigen()` is a top-down, hierarchical algorithm.
#'   - `node_in_walktrap()` is a hierarchical, agglomerative algorithm based on random walks.
#'  
#'   The different algorithms offer various advantages in terms of computation time,
#'   availability on different types of networks, ability to maximise modularity,
#'   and their logic or domain of inspiration.
#'   
#' @inheritParams member_community_non
#' @name member_community_hier
#' @family memberships
NULL

#' @rdname member_community_hier 
#' @section Edge-betweenness:
#'   This is motivated by the idea that edges connecting different groups 
#'   are more likely to lie on multiple shortest paths when they are the 
#'   only option to go from one group to another. 
#'   This method yields good results but is very slow because of 
#'   the computational complexity of edge-betweenness calculations and 
#'   the betweenness scores have to be re-calculated after every edge removal. 
#'   Networks of ~700 nodes and ~3500 ties are around the upper size limit 
#'   that are feasible with this approach. 
#' @references
#' ## On edge-betweenness community detection
#' Newman, Mark, and Michelle Girvan. 2004.
#' "Finding and evaluating community structure in networks." 
#' _Physical Review E_ 69: 026113.
#' \doi{10.1103/PhysRevE.69.026113}
#' @examples
#' node_in_betweenness(ison_adolescents)
#' if(require("ggdendro", quietly = TRUE)){
#' plot(node_in_betweenness(ison_adolescents))
#' }
#' @export
node_in_betweenness <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(net_nodes(.data)>100) 
    cli::cli_alert_danger(paste("This algorithm may take some time", 
                                "or even run out of memory on such a large network."))
  clust <- suppressWarnings(igraph::cluster_edge_betweenness(
    manynet::as_igraph(.data)))
  out <- clust$membership
  out <- make_node_member(out, .data)
  attr(out, "hc") <- stats::as.hclust(clust, 
                                      use.modularity = igraph::is_connected(.data))
  attr(out, "k") <- max(clust$membership)
  out
}

#' @rdname member_community_hier 
#' @section Fast-greedy:
#'   Initially, each node is assigned a separate community.
#'   Communities are then merged iteratively such that each merge
#'   yields the largest increase in the current value of modularity,
#'   until no further increases to the modularity are possible.
#'   The method is fast and recommended as a first approximation 
#'   because it has no parameters to tune. 
#'   However, it is known to suffer from a resolution limit.
#' @references
#' ## On fast-greedy community detection
#' Clauset, Aaron, Mark E.J. Newman, and Cristopher Moore. 2004.
#' "Finding community structure in very large networks."
#' _Physical Review E_, 70: 066111.
#' \doi{10.1103/PhysRevE.70.066111}
#' @examples
#' node_in_greedy(ison_adolescents)
#' @export
node_in_greedy <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  clust <- igraph::cluster_fast_greedy(to_undirected(as_igraph(.data)))
  out <- clust$membership
  make_node_member(out, .data)
  out <- make_node_member(out, .data)
  attr(out, "hc") <- stats::as.hclust(clust, 
                                      use.modularity = igraph::is_connected(.data))
  attr(out, "k") <- max(clust$membership)
  out
}

#' @rdname member_community_hier 
#' @section Leading eigenvector:
#'   In each step, the network is bifurcated such that modularity increases most.
#'   The splits are determined according to the leading eigenvector of the modularity matrix.
#'   A stopping condition prevents tightly connected groups from being split further.
#'   Note that due to the eigenvector calculations involved,
#'   this algorithm will perform poorly on degenerate networks,
#'   but will likely obtain a higher modularity than fast-greedy (at some cost of speed).
#' @references
#' ## On leading eigenvector community detection
#' Newman, Mark E.J. 2006.
#' "Finding community structure using the eigenvectors of matrices"
#' _Physical Review E_ 74:036104.
#' \doi{10.1103/PhysRevE.74.036104}
#' @examples
#' node_in_eigen(ison_adolescents)
#' @export
node_in_eigen <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(is_directed(.data)){
    mnet_info("This algorithm only works for undirected networks.", 
              "Converting to undirected")
    .data <- to_undirected(.data)
  }
  clust <- igraph::cluster_leading_eigen(as_igraph(.data))
  out <- clust$membership
  make_node_member(out, .data)
  out <- make_node_member(out, .data)
  attr(out, "hc") <- stats::as.hclust(clust)
  attr(out, "k") <- max(clust$membership)
  out
}

#' @rdname member_community_hier 
#' @section Walktrap:
#'   The general idea is that random walks on a network are more likely to stay 
#'   within the same community because few edges lead outside a community.
#'   By repeating random walks of 4 steps many times,
#'   information about the hierarchical merging of communities is collected.
#' @param times Integer indicating number of simulations/walks used.
#'   By default, `times=50`.
#' @references
#' ## On walktrap community detection
#' Pons, Pascal, and Matthieu Latapy. 2005.
#' "Computing communities in large networks using random walks".
#' 1-20.
#' \doi{10.48550/arXiv.physics/0512106}
#' @examples
#' node_in_walktrap(ison_adolescents)
#' @export
node_in_walktrap <- function(.data, times = 50){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  clust <- igraph::cluster_walktrap(manynet::as_igraph(.data))
  out <- clust$membership
  make_node_member(out, .data)
  out <- make_node_member(out, .data)
  attr(out, "hc") <- stats::as.hclust(clust, 
                                      use.modularity = igraph::is_connected(.data))
  attr(out, "k") <- max(clust$membership)
  out
}

# #' @rdname member_community_hier 
# #' @section Ensemble:
# #'   Ensemble-based community detection runs community detection
# #'   algorithms over multilayer or multiplex networks.
# #' @references
# #' ## On ensemble-based community detection
# #' Tagarelli, Andrea, Alessia Amelio, and Francesco Gullo. 2017.
# #' "Ensemble-based Community Detection in Multilayer Networks".
# #' _Data Mining and Knowledge Discovery_, 31: 1506-1543.
# #' \doi{10.1007/s10618-017-0528-8}
# #' @examples
# #' node_in_ensemble(ison_adolescents)
# #' @export
# node_in_ensemble <- function(.data, linkage_constraint = TRUE){
#   if(missing(.data)) {expect_nodes(); .data <- .G()}
#   clust <- igraph::cluster_walktrap(manynet::as_igraph(.data))
#   out <- clust$membership
#   make_node_member(out, .data)
#   out <- make_node_member(out, .data)
#   attr(out, "hc") <- stats::as.hclust(clust, 
#                                       use.modularity = igraph::is_connected(.data))
#   attr(out, "k") <- max(clust$membership)
#   out
# }
