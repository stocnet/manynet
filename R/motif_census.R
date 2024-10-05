# Node censuses ####

#' Motifs at the nodal level
#' 
#' @description
#'   These functions include ways to take a census of the positions of nodes
#'   in a network: 
#'   
#'   - `node_by_tie()` returns a census of the ties in a network.
#'   For directed networks, out-ties and in-ties are bound together.
#'   For multiplex networks, the various types of ties are bound together.
#'   - `node_by_triad()` returns a census of the triad configurations
#'   nodes are embedded in.
#'   - `node_by_quad()` returns a census of nodes' positions
#'   in motifs of four nodes.
#'   - `node_by_path()` returns the shortest path lengths
#'   of each node to every other node in the network.
#'   
#' @name motif_node
#' @family motifs
#' @inheritParams mark_is
#' @importFrom igraph vcount make_ego_graph delete_vertices triad_census
NULL

#' @rdname motif_node 
#' @examples
#' task_eg <- to_named(to_uniplex(ison_algebra, "tasks"))
#' (tie_cen <- node_by_tie(task_eg))
#' @export
node_by_tie <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  object <- as_igraph(.data)
  # edge_names <- net_tie_attributes(object)
  if (is_directed(object)) {
    if (is_multiplex(.data)) {
      mat <- do.call(rbind, lapply(unique(tie_attribute(object, "type")), 
                                   function(x){
                                     rc <- manynet::as_matrix(manynet::to_uniplex(object, x))
                                     rbind(rc, t(rc))
                                   }))
    } else if (is_longitudinal(object)){
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "wave")), 
                                   function(x){
                                     rc <- manynet::as_matrix(manynet::to_waves(object)[[x]])
                                     rbind(rc, t(rc))
                                   }))
      
      } else {
        rc <- manynet::as_matrix(object)
        mat <- rbind(rc, t(rc))
      }
  } else {
    if (manynet::is_multiplex(.data)) {
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "type")), 
                                   function(x){
                                     manynet::as_matrix(manynet::to_uniplex(object, x))
                                   }))
    } else if (manynet::is_longitudinal(object)){
      mat <- do.call(rbind, lapply(unique(manynet::tie_attribute(object, "wave")), 
                                   function(x){
                                     manynet::as_matrix(manynet::to_waves(object)[[x]])
                                   }))
    } else if (manynet::is_twomode(.data)) {
      mat <- manynet::as_matrix(manynet::to_multilevel(object))
    } else {
      mat <- manynet::as_matrix(object)
    }
  }
  if(manynet::is_labelled(object) & manynet::is_directed(object))
    if(manynet::is_multiplex(.data)){
      rownames(mat) <- apply(expand.grid(c(paste0("from", manynet::node_names(object)),
                                           paste0("to", manynet::node_names(object))),
                                           unique(manynet::tie_attribute(object, "type"))), 
                             1, paste, collapse = "_")
    } else if (manynet::is_longitudinal(object)){
      rownames(mat) <- apply(expand.grid(c(paste0("from", manynet::node_names(object)),
                                           paste0("to", manynet::node_names(object))),
                                         unique(manynet::tie_attribute(object, "wave"))), 
                             1, paste, collapse = "_wave")
    } else {
      rownames(mat) <- rep(c(paste0("from", manynet::node_names(object)),
                             paste0("to", manynet::node_names(object))))
    }
  make_node_motif(t(mat), object)
}

#' @rdname motif_node 
#' @references
#' ## On the dyad census
#' Holland, Paul W., and Samuel Leinhardt. 1970. 
#' "A Method for Detecting Structure in Sociometric Data". 
#' _American Journal of Sociology_, 76: 492-513.
#' \doi{10.1016/B978-0-12-442450-0.50028-6}
#' @examples 
#' node_by_dyad(ison_networkers)
#' @export
node_by_dyad <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(is_weighted(.data)){
    .data <- to_unweighted(.data)
    mnet_info("Ignoring tie weights.")
  }
  mat <- as_matrix(.data)
  out <- t(vapply(seq_nodes(.data), function(x){
    vec <- mat[x,] + mat[,x]
    c(sum(vec==2), sum(vec==1), sum(vec==0))
  }, FUN.VALUE = numeric(3)))
  colnames(out) <- c("Mutual", "Asymmetric", "Null")
  if (!is_directed(.data)) out <- out[,c(1, 3)]
  make_node_motif(out, .data)
}

#' @rdname motif_node 
#' @references 
#' ## On the triad census
#' Davis, James A., and Samuel Leinhardt. 1967. 
#' “\href{https://files.eric.ed.gov/fulltext/ED024086.pdf}{The Structure of Positive Interpersonal Relations in Small Groups}.” 55.
#' @examples 
#' (triad_cen <- node_by_triad(task_eg))
#' @export
node_by_triad <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  out <- t(sapply(seq.int(manynet::net_nodes(.data)), 
                  function(x) net_by_triad(.data) - net_by_triad(manynet::delete_nodes(.data, x))))
  make_node_motif(out, .data)
}

# #' @rdname motif_node
# #' @section Quad census:
# #'   The quad census uses the `{oaqc}` package to do
# #'   the heavy lifting of counting the number of each orbits.
# #'   See `vignette('oaqc')`.
# #'   However, our function relabels some of the motifs
# #'   to avoid conflicts and improve some consistency with
# #'   other census-labelling practices.
# #'   The letter-number pairing of these labels indicate
# #'   the number and configuration of ties.
# #'   For now, we offer a rough translation:
# #' 
# #' | migraph | Ortmann and Brandes      
# #' | ------------- |------------- |
# #' | E4  | co-K4
# #' | I40, I41  | co-diamond
# #' | H4  | co-C4
# #' | L42, L41, L40 | co-paw
# #' | D42, D40 | co-claw
# #' | U42, U41 | P4
# #' | Y43, Y41 | claw
# #' | P43, P42, P41 | paw
# #' | 04 | C4
# #' | Z42, Z43 | diamond
# #' | X4 | K4
# #' 
# #' See also [this list of graph classes](https://www.graphclasses.org/smallgraphs.html#nodes4).
# #' @importFrom tidygraph %E>%
# #' @references 
# #'  Ortmann, Mark, and Ulrik Brandes. 2017. 
# #'  “Efficient Orbit-Aware Triad and Quad Census in Directed and Undirected Graphs.” 
# #'  \emph{Applied Network Science} 2(1):13. 
# #'  \doi{10.1007/s41109-017-0027-2}.
# #' @examples 
# #' node_by_quad(ison_southern_women)
# #' @export
# node_by_quad <- function(.data){
#   if(missing(.data)) {expect_nodes(); .data <- .G()}
#   thisRequires("oaqc")
#   graph <- .data %>% manynet::as_tidygraph() %E>% 
#     as.data.frame()
#   if(ncol(graph)>2) graph <- graph[,1:2]
#   out <- oaqc::oaqc(graph)[[1]]
#   out <- out[-1,]
#   rownames(out) <- manynet::node_names(.data)
#   colnames(out) <- c("E4", # co-K4
#                      "I41","I40", # co-diamond
#                      "H4", # co-C4
#                      "L42","L41","L40", # co-paw
#                      "D42","D40", # co-claw
#                      "U42","U41", # P4
#                      "Y43","Y41", # claw
#                      "P43","P42","P41", # paw
#                      "04", # C4
#                      "Z42","Z43", # diamond
#                      "X4") # K4
#   if(manynet::is_twomode(.data)) out <- out[,-c(8,9,14,15,16,18,19,20)]
#   make_node_motif(out, .data)
# }

#' @rdname motif_node
#' @examples 
#' node_by_quad(ison_southern_women)
#' @export
node_by_quad <- function(.data){
  cmbs <- utils::combn(1:net_nodes(.data), 4)
  mat <- as_matrix(to_onemode(.data))
  dd <- apply(cmbs, 2, function(x) c(sum(mat[x,x]), 
                                     max(rowSums(mat[x,x]))))
  
  types <- rep(NA, ncol(cmbs))
  types[dd[1,] == 0] <- "E4"
  types[dd[1,] == 2] <- "I4"
  types[dd[1,] == 4 & dd[2,] == 1] <- "H4"
  types[dd[1,] == 4 & dd[2,] == 2] <- "L4"
  types[dd[1,] == 6 & dd[2,] == 2] <- "D4"
  types[dd[1,] == 6 & dd[2,] == 1] <- "U4"
  types[dd[1,] == 6 & dd[2,] == 3] <- "Y4"
  types[dd[1,] == 8 & dd[2,] == 3] <- "P4"
  types[dd[1,] == 8 & dd[2,] == 2] <- "C4"
  types[dd[1,] == 10] <- "Z4"
  types[dd[1,] == 12] <- "X4"
  
  appears <- sapply(seq.int(net_nodes(.data)), 
         function(x) types[which(cmbs == x, arr.ind = TRUE)[,2]])
  out <- apply(appears, 2, table)

  if(is.list(out)){
    out <- as.matrix(dplyr::bind_rows(out))
  } else out <- as.matrix(as.data.frame(t(out)))
  out.order <- c("E4","I4","H4","L4","D4","U4","Y4","P4","C4","Z4","X4")
  out <- out[,match(out.order, colnames(out))]
  colnames(out) <- out.order
  out[is.na(out)] <- 0

  make_node_motif(out, .data)
}

# https://stackoverflow.com/questions/26828301/faster-version-of-combn#26828486
# comb2.int <- function(n, choose = 2){
#   # e.g. n=3 => (1,2), (1,3), (2,3)
#   x <- rep(1:n,(n:1)-1)
#   i <- seq_along(x)+1
#   o <- c(0,cumsum((n-2):1))
#   y <- i-o[x]
#   return(cbind(x,y))
# }
  
# #' @export
# node_igraph_census <- function(.data, normalized = FALSE){
#     out <- igraph::motifs(manynet::as_igraph(.data), 4)
#     if(manynet::is_labelled(.data))
#       rownames(out) <- manynet::node_names(.data)
#     colnames(out) <- c("co-K4",
#                        "co-diamond",
#                        "co-C4",
#                        "co-paw",
#                        "co-claw",
#                        "P4",
#                        "claw",
#                        "paw",
#                        "C4",
#                        "diamond",
#                        "K4")
#     make_node_motif(out, .data)
# }

#' @rdname motif_node 
#' @importFrom igraph distances
#' @references 
#' ## On paths
#' Dijkstra, Edsger W. 1959. 
#' "A note on two problems in connexion with graphs". 
#' _Numerische Mathematik_ 1, 269-71.
#' \doi{10.1007/BF01386390}.
#' 
#' Opsahl, Tore, Filip Agneessens, and John Skvoretz. 2010.
#' "Node centrality in weighted networks: Generalizing degree and shortest paths". 
#' _Social Networks_ 32(3): 245-51.
#' \doi{10.1016/j.socnet.2010.03.006}.
#' @examples 
#' node_by_path(ison_adolescents)
#' node_by_path(ison_southern_women)
#' @export
node_by_path <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(manynet::is_weighted(.data)){
    tore <- manynet::as_matrix(.data)/mean(manynet::as_matrix(.data))
    out <- 1/tore
  } else out <- igraph::distances(manynet::as_igraph(.data))
  diag(out) <- 0
  make_node_motif(out, .data)
}

# Network censuses ####

#' Motifs at the network level
#' 
#' @description
#'   These functions include ways to take a census of the positions of nodes
#'   in a network: 
#'   
#'   - `net_by_dyad()` returns a census of dyad motifs in a network.
#'   - `net_by_triad()` returns a census of triad motifs in a network.
#'   - `net_by_mixed()` returns a census of triad motifs that span
#'   a one-mode and a two-mode network.
#'   
#' @name motif_net
#' @family motifs
#' @inheritParams motif_node
#' @param object2 A second, two-mode migraph-consistent object.
NULL

#' @rdname motif_net
#' @references
#' ## On the dyad census
#' Holland, Paul W., and Samuel Leinhardt. 1970. 
#' "A Method for Detecting Structure in Sociometric Data". 
#' _American Journal of Sociology_, 76: 492-513.
#' \doi{10.1016/B978-0-12-442450-0.50028-6}
#' 
#' Wasserman, Stanley, and Katherine Faust. 1994. 
#' "Social Network Analysis: Methods and Applications". 
#' Cambridge: Cambridge University Press.
#' @examples 
#' net_by_dyad(manynet::ison_algebra)
#' @export
net_by_dyad <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if (manynet::is_twomode(.data)) {
    mnet_unavailable("A twomode or multilevel option for a dyad census is not yet implemented.")
  } else {
    out <- suppressWarnings(igraph::dyad_census(manynet::as_igraph(.data)))
    out <- unlist(out)
    names(out) <- c("Mutual", "Asymmetric", "Null")
    if (!manynet::is_directed(.data)) out <- out[c(1, 3)]
    make_network_motif(out, .data)
  }
}

#' @rdname motif_net 
#' @references 
#' ## On the triad census
#' Davis, James A., and Samuel Leinhardt. 1967. 
#' “\href{https://files.eric.ed.gov/fulltext/ED024086.pdf}{The Structure of Positive Interpersonal Relations in Small Groups}.” 55.
#' @examples 
#' net_by_triad(manynet::ison_adolescents)
#' @export
net_by_triad <- function(.data) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if (manynet::is_twomode(.data)) {
    cli::cli_abort("A twomode or multilevel option for a triad census is not yet implemented.")
  } else {
    out <- suppressWarnings(igraph::triad_census(as_igraph(.data)))
    names(out) <- c("003", "012", "102", "021D",
                    "021U", "021C", "111D", "111U",
                    "030T", "030C", "201", "120D",
                    "120U", "120C", "210", "300")
    if (!manynet::is_directed(.data)) out <- out[c(1, 2, 3, 11, 15, 16)]
    make_network_motif(out, .data)
  }
}

#' @rdname motif_net
#' @examples 
#' net_by_quad(ison_southern_women)
#' @export
net_by_quad <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  cmbs <- utils::combn(1:net_nodes(.data), 4)
  mat <- as_matrix(to_onemode(.data))
  dens <- apply(cmbs, 2, function(x) sum(mat[x,x]))
  
  E4 <- sum(dens == 0)
  I4 <- sum(dens == 1)
  
  if(any(dens==2)){
    if(sum(dens==2)>1){
      twosies <- apply(cmbs[,dens==2], 2, function(x) max(rowSums(mat[x,x])))
    } else twosies <- max(rowSums(mat[cmbs[,dens==2], cmbs[,dens==2]]))
    H4 <- sum(twosies==1)
    L4 <- sum(twosies==2)
  } else H4 <- L4 <- 0
  
  if(any(dens==3)){
    if(sum(dens==3)>1){
      threesies <- apply(cmbs[,dens==3], 2, function(x) max(rowSums(mat[x,x])))
    } else threesies <- max(rowSums(mat[cmbs[,dens==3], cmbs[,dens==3]]))
    D4 <- sum(threesies==2)
    U4 <- sum(threesies==1)
    Y4 <- sum(threesies==3)
  } else D4 <- U4 <- Y4 <- 0
  
  if(any(dens==4)){
    if(sum(dens==4)>1){
      foursies <- apply(cmbs[,dens==4], 2, function(x) max(rowSums(mat[x,x])))
    } else foursies <- max(rowSums(mat[cmbs[,dens==4], cmbs[,dens==4]]))
    P4 <- sum(foursies==3)
    C4 <- sum(foursies==2)
  } else P4 <- C4 <- 0
  
  Z4 <- sum(dens == 5)
  X4 <- sum(dens == 6)
  
  out <- c(E4 = E4, I4 = I4, H4 = H4, L4 = L4, D4 = D4, U4 = U4, Y4 = Y4, 
           P4 = P4, C4 = C4, Z4 = Z4, X4 = X4)
  
  make_network_motif(out, .data)
}

#' @rdname motif_net 
#' @source Alejandro Espinosa 'netmem'
#' @references 
#' ## On the mixed census
#' Hollway, James, Alessandro Lomi, Francesca Pallotti, and Christoph Stadtfeld. 2017.
#' “Multilevel Social Spaces: The Network Dynamics of Organizational Fields.” 
#' _Network Science_ 5(2): 187–212.
#' \doi{10.1017/nws.2017.8}
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships, "positive")
#' (mixed_cen <- net_by_mixed(marvel_friends, ison_marvel_teams))
#' @export
net_by_mixed <- function (.data, object2) {
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(manynet::is_twomode(.data))
    cli::cli_abort("First object should be a one-mode network")
  if(!manynet::is_twomode(object2))
    cli::cli_abort("Second object should be a two-mode network")
  if(manynet::net_dims(.data)[1] != manynet::net_dims(object2)[1])
    cli::cli_abort("Non-conformable arrays")
  m1 <- manynet::as_matrix(.data)
  m2 <- manynet::as_matrix(object2)
  cp <- function(m) (-m + 1)
  onemode.reciprocal <- m1 * t(m1)
  onemode.forward <- m1 * cp(t(m1))
  onemode.backward <- cp(m1) * t(m1)
  onemode.null <- cp(m1) * cp(t(m1))
  diag(onemode.forward) <- 0
  diag(onemode.backward) <- 0
  diag(onemode.null) <- 0
  bipartite.twopath <- m2 %*% t(m2)
  bipartite.null <- cp(m2) %*% cp(t(m2))
  bipartite.onestep1 <- m2 %*% cp(t(m2))
  bipartite.onestep2 <- cp(m2) %*% t(m2)
  diag(bipartite.twopath) <- 0
  diag(bipartite.null) <- 0
  diag(bipartite.onestep1) <- 0
  diag(bipartite.onestep2) <- 0
  res <- c("22" = sum(onemode.reciprocal * bipartite.twopath) / 2,
           "21" = sum(onemode.forward * bipartite.twopath) / 2 + sum(onemode.backward * bipartite.twopath) / 2,
           "20" = sum(onemode.null * bipartite.twopath) / 2,
           "12" = sum(onemode.reciprocal * bipartite.onestep1) / 2 + sum(onemode.reciprocal * bipartite.onestep2) / 2,
           "11D" = sum(onemode.forward * bipartite.onestep1) / 2 + sum(onemode.backward * bipartite.onestep2) / 2,
           "11U" = sum(onemode.forward * bipartite.onestep2) / 2 + sum(onemode.backward * bipartite.onestep1) / 2,
           "10" = sum(onemode.null * bipartite.onestep2) / 2 + sum(onemode.null * bipartite.onestep1) / 2,
           "02" = sum(onemode.reciprocal * bipartite.null) / 2,
           "01" = sum(onemode.forward * bipartite.null) / 2 + sum(onemode.backward * bipartite.null) / 2,
           "00" = sum(onemode.null * bipartite.null) / 2)  
  make_network_motif(res, .data)
}

# Brokerage ####

#' Motifs of brokerage
#' 
#' @description
#'   These functions include ways to take a census of the brokerage positions of nodes
#'   in a network: 
#'   
#'   - `node_by_brokerage()` returns the Gould-Fernandez brokerage
#'   roles played by nodes in a network.
#'   - `net_by_brokerage()` returns the Gould-Fernandez brokerage
#'   roles in a network.
#'   - `node_brokering_activity()` measures nodes' brokerage activity.
#'   - `node_brokering_exclusivity()` measures nodes' brokerage exclusivity. 
#'   
#' @name motif_brokerage
#' @family motifs
#' @inheritParams motif_node
#' @param membership A vector of partition membership as integers.
#' @param standardized Whether the score should be standardized
#'   into a _z_-score indicating how many standard deviations above
#'   or below the average the score lies.
NULL

#' @rdname motif_brokerage 
#' @references 
#' ## On brokerage motifs
#' Gould, Roger V., and Roberto M. Fernandez. 1989. 
#' “Structures of Mediation: A Formal Approach to Brokerage in Transaction Networks.” 
#' _Sociological Methodology_, 19: 89-126.
#' \doi{10.2307/270949}
#' 
#' Jasny, Lorien, and Mark Lubell. 2015. 
#' “Two-Mode Brokerage in Policy Networks.” 
#' _Social Networks_ 41:36–47. 
#' \doi{10.1016/j.socnet.2014.11.005}
#' @examples 
#' # node_by_brokerage(ison_networkers, "Discipline")
#' @export
node_by_brokerage <- function(.data, membership, standardized = FALSE){
  thisRequires("sna")
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!manynet::is_twomode(.data)){
    out <- sna::brokerage(manynet::as_network(.data),
                          manynet::node_attribute(.data, membership))
    out <- if(standardized) out$z.nli else out$raw.nli
    colnames(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                       "Representative", "Liaison", "Total")
  } else {
    out <- suppressWarnings(sna::brokerage(manynet::as_network(manynet::to_mode1(.data)),
                          manynet::node_attribute(.data, membership)))
    out <- if(standardized) out$z.nli else out$raw.nli
    out <- out[,-4]
    colnames(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                       "Liaison", "Total")
  }
  make_node_motif(out, .data)
}

#' @rdname motif_brokerage 
#' @examples 
#' # net_by_brokerage(ison_networkers, "Discipline")
#' @export
net_by_brokerage <- function(.data, membership, standardized = FALSE){
  thisRequires("sna")
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(!manynet::is_twomode(.data)){
    out <- sna::brokerage(manynet::as_network(.data),
                        manynet::node_attribute(.data, membership))
  out <- if(standardized) out$z.gli else out$raw.gli
  names(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                     "Representative", "Liaison", "Total")
  } else {
    out <- suppressWarnings(sna::brokerage(manynet::as_network(manynet::to_mode1(.data)),
                          manynet::node_attribute(.data, membership)))
    out <- if(standardized) out$z.gli else out$raw.gli
    names(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                    "Representative", "Liaison", "Total")
  }
    make_network_motif(out, .data)
}

#' @rdname motif_brokerage 
#' @references
#' ## On brokerage activity and exclusivity
#'   Hamilton, Matthew, Jacob Hileman, and Orjan Bodin. 2020.
#'   "Evaluating heterogeneous brokerage: New conceptual and methodological approaches
#'   and their application to multi-level environmental governance networks"
#'   _Social Networks_ 61: 1-10.
#'   \doi{10.1016/j.socnet.2019.08.002}
#' @export
node_brokering_activity <- function(.data, membership){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  from <- to.y <- to_memb <- from_memb <- NULL
  twopaths <- .to_twopaths(.data)
  if(!missing(membership)){
    twopaths$from_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                                 match(twopaths$from, manynet::node_names(.data)),
                                                                 twopaths$from)]
    twopaths$to_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                               match(twopaths$to.y, manynet::node_names(.data)),
                                                               twopaths$to.y)]
    twopaths <- dplyr::filter(twopaths, from_memb != to_memb)
  }
  # tabulate brokerage
  out <- c(table(twopaths$to))
  # correct ordering for named data
  if(manynet::is_labelled(.data)) out <- out[match(manynet::node_names(.data), names(out))]
  # missings should be none
  out[is.na(out)] <- 0
  make_node_measure(out, .data)
}

#' @rdname motif_brokerage
#' @examples
#' node_brokering_exclusivity(ison_networkers, "Discipline")
#' @export
node_brokering_exclusivity <- function(.data, membership){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  from <- to.y <- to_memb <- from_memb <- NULL
  twopaths <- .to_twopaths(.data)
  if(!missing(membership)){
    twopaths$from_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                                 match(twopaths$from, manynet::node_names(.data)),
                                                                 twopaths$from)]
    twopaths$to_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                               match(twopaths$to.y, manynet::node_names(.data)),
                                                               twopaths$to.y)]
    twopaths <- dplyr::filter(twopaths, from_memb != to_memb)
  }
  # get only exclusive paths
  out <- twopaths %>% dplyr::group_by(from, to.y) %>% dplyr::filter(dplyr::n()==1)
  # tabulate brokerage
  out <- c(table(out$to))
  # correct ordering for named data
  if(manynet::is_labelled(.data)) out <- out[match(manynet::node_names(.data), names(out))]
  # missings should be none
  out[is.na(out)] <- 0
  make_node_measure(out, .data)
}

#' Memberships of brokerage
#' 
#' @description
#'   These functions include ways to take a census of the brokerage positions of nodes
#'   in a network: 
#'   
#'   - `node_in_brokerage()` returns nodes membership as a powerhouse,
#'   connector, linchpin, or sideliner according to Hamilton et al. (2020).
#'   
#' @name member_brokerage
#' @family memberships
#' @inheritParams motif_brokerage
NULL

#' @rdname member_brokerage 
#' @export
node_in_brokering <- function(.data, membership){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  activ <- node_brokering_activity(.data, membership)
  exclusiv <- node_brokering_exclusivity(.data, membership)
  activ <- activ - mean(activ)
  exclusiv <- exclusiv - mean(exclusiv)
  out <- dplyr::case_when(activ > 0 & exclusiv > 0 ~ "Powerhouse",
                          activ > 0 & exclusiv < 0 ~ "Connectors",
                          activ < 0 & exclusiv > 0 ~ "Linchpins",
                          activ < 0 & exclusiv < 0 ~ "Sideliners")
  make_node_member(out, .data)
}

.to_twopaths <- function(.data){
  to <- from <- to.y <- NULL
  if(!manynet::is_directed(.data)){
    el <- manynet::as_edgelist(manynet::to_reciprocated(.data)) 
  } else el <- manynet::as_edgelist(.data)
  twopaths <- dplyr::full_join(el, el, 
                               by = dplyr::join_by(to == from), 
                               relationship = "many-to-many")
  # remove non two-paths
  twopaths <- dplyr::filter(twopaths, !(is.na(from) | is.na(to.y)))
  # remove reciprocated paths
  twopaths <- dplyr::filter(twopaths, from != to.y)
  # remove triads
  twopaths <- dplyr::filter(twopaths, !paste(from, to.y) %in% paste(from, to))
  twopaths
}


# Diffusion ####

#' Motifs of diffusion
#' 
#' @description
#'   - `node_by_exposure()` produces a motif matrix of nodes' exposure to 
#'   infection/adoption by time step
#' 
#' @family motifs
#' @inheritParams motif_node
#' @inheritParams measure_diffusion_net
#' @name motif_diffusion
#' 
NULL

#' @rdname motif_diffusion
#' @export
node_by_exposure <- function(diff_model){
  t <- NULL
  .data <- as_tidygraph(diff_model)
  times <- diff_model$t
  out <- sapply(times, function(x){
   inf <- node_is_infected(diff_model, time = x)
   if(sum(inf)==1) as_matrix(.data)[inf,] else
     colSums(as_matrix(.data)[inf,])
  })
  colnames(out) <- paste0("t",times)
  make_node_motif(out, .data)
}

