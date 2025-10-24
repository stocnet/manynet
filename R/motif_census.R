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
#'   - `node_by_tetrad()` returns a census of nodes' positions
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(is_weighted(.data)){
    .data <- to_unweighted(.data)
    snet_info("Ignoring tie weights.")
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
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

#' @rdname motif_node
#' @section Tetrad census:
#'   The nodal tetrad census counts the number of four-node configurations
#'   that each node is embedded in.
#'   The function returns a matrix with a special naming convention:
#'   - E4 (aka co-K4): This is an empty set of four nodes; no ties
#'   - I4 (aka co-diamond): This is a set of four nodes with just one tie
#'   - H4 (aka co-C4): This set of four nodes includes two non-adjacent ties
#'   - L4 (aka co-paw): This set of four nodes includes two adjacent ties
#'   - D4 (aka co-claw): This set of four nodes includes three adjacent ties,
#'   in the form of a triangle with one isolate
#'   - U4 (aka P4, four-actor line): This set of four nodes includes three ties 
#'   arranged in a line
#'   - Y4 (aka claw): This set of four nodes includes three ties all adjacent
#'   to a single node
#'   - P4 (aka paw, kite): This set of four nodes includes four ties arranged
#'   as a triangle with an extra tie hanging off of one of the nodes
#'   - C4 (aka bifan): This is a symmetric box or 4-cycle or set of shared choices
#'   - Z4 (aka diamond): This resembles C4 but with an extra tie cutting across the box
#'   - X4 (aka K4): This resembles C4 but with two extra ties cutting across the box;
#'   a realisation of all possible ties
#'   
#'   Graphs of these motifs can be shown using 
#'   `plot(node_by_tetrad(ison_southern_women))`.
#' @references
#' ## On the tetrad census
#'  Ortmann, Mark, and Ulrik Brandes. 2017. 
#'  “Efficient Orbit-Aware Triad and Quad Census in Directed and Undirected Graphs.” 
#'  \emph{Applied Network Science} 2(1):13. 
#'  \doi{10.1007/s41109-017-0027-2}.
#'  
#'  McMillan, Cassie, and Diane Felmlee. 2020.
#'  "Beyond Dyads and Triads: A Comparison of Tetrads in Twenty Social Networks".
#'  _Social Psychology Quarterly_ 83(4): 383-404.
#'  \doi{10.1177/0190272520944151}
#' @examples 
#' node_by_tetrad(ison_southern_women)
#' @export
node_by_tetrad <- function(.data){
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
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
#'   These functions include ways to take a census of the graphlets
#'   in a network: 
#'   
#'   - `net_by_dyad()` returns a census of dyad motifs in a network.
#'   - `net_by_triad()` returns a census of triad motifs in a network.
#'   - `net_by_tetrad()` returns a census of tetrad motifs in a network.
#'   - `net_by_mixed()` returns a census of triad motifs that span
#'   a one-mode and a two-mode network.
#'   
#'   See also \href{https://www.graphclasses.org/smallgraphs.html}{graph classes}.
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if (manynet::is_twomode(.data)) {
    snet_unavailable("A twomode or multilevel option for a dyad census is not yet implemented.")
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if (manynet::is_twomode(.data)) {
    snet_abort("A twomode or multilevel option for a triad census is not yet implemented.")
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
#' @section Tetrad census:
#'   The tetrad census counts the number of four-node configurations in the network.
#'   The function returns a matrix with a special naming convention:
#'   - E4 (aka co-K4): This is an empty set of four nodes; no ties
#'   - I4 (aka co-diamond): This is a set of four nodes with just one tie
#'   - H4 (aka co-C4): This set of four nodes includes two non-adjacent ties
#'   - L4 (aka co-paw): This set of four nodes includes two adjacent ties
#'   - D4 (aka co-claw): This set of four nodes includes three adjacent ties,
#'   in the form of a triangle with one isolate
#'   - U4 (aka P4, four-actor line): This set of four nodes includes three ties 
#'   arranged in a line
#'   - Y4 (aka claw): This set of four nodes includes three ties all adjacent
#'   to a single node
#'   - P4 (aka paw, kite): This set of four nodes includes four ties arranged
#'   as a triangle with an extra tie hanging off of one of the nodes
#'   - C4 (aka bifan): This is a symmetric box or 4-cycle or set of shared choices
#'   - Z4 (aka diamond): This resembles C4 but with an extra tie cutting across the box
#'   - X4 (aka K4): This resembles C4 but with two extra ties cutting across the box;
#'   a realisation of all possible ties
#'   
#'   Graphs of these motifs can be shown using 
#'   `plot(net_by_tetrad(ison_southern_women))`.
#' @references
#' ## On the tetrad census
#'  Ortmann, Mark, and Ulrik Brandes. 2017. 
#'  “Efficient Orbit-Aware Triad and Quad Census in Directed and Undirected Graphs.” 
#'  \emph{Applied Network Science} 2(1):13. 
#'  \doi{10.1007/s41109-017-0027-2}.
#'  
#'  McMillan, Cassie, and Diane Felmlee. 2020.
#'  "Beyond Dyads and Triads: A Comparison of Tetrads in Twenty Social Networks".
#'  _Social Psychology Quarterly_ 83(4): 383-404.
#'  \doi{10.1177/0190272520944151}
#' @examples 
#' net_by_tetrad(ison_southern_women)
#' @export
net_by_tetrad <- function(.data){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
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
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(manynet::is_twomode(.data))
    snet_abort("First object should be a one-mode network")
  if(!manynet::is_twomode(object2))
    snet_abort("Second object should be a two-mode network")
  if(manynet::net_dims(.data)[1] != manynet::net_dims(object2)[1])
    snet_abort("Non-conformable arrays")
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

# Diffusion ####

#' Motifs of diffusion
#' 
#' @description
#'   - `net_by_hazard()` measures the hazard rate or instantaneous probability that
#'   nodes will adopt/become infected at that time.
#'   - `node_by_exposure()` produces a motif matrix of nodes' exposure to 
#'   infection/adoption by time step.
#' 
#' @family motifs
#' @inheritParams motif_node
#' @inheritParams measure_diffusion_net
#' @name motif_diffusion
#' 
NULL

#' @rdname motif_diffusion
#' @examples
#' node_by_exposure(play_diffusion(create_tree(12)))
#' @export
node_by_exposure <- function(.data){
  if(inherits(.data, "diff_model")){
    diff_model <- as_tidygraph(.data)
    times <- diff_model$t
    out <- sapply(times, function(x){
      inf <- node_is_infected(diff_model, time = x)
      if(sum(inf)==1) as_matrix(.data)[inf,] else
        colSums(as_matrix(.data)[inf,])
    })
  } else {
    times <- as_diffusion(.data)$time
    out <- sapply(times, function(x){
      inf <- node_is_infected(.data, time = x)
      if(sum(inf)==1) as_matrix(.data)[inf,] else
        colSums(as_matrix(.data)[inf,])
    })
  }
  colnames(out) <- paste0("t",times)
  make_node_motif(out, .data)
}

#' @rdname motif_diffusion
#' @section Hazard rate: 
#' The hazard rate is the instantaneous probability of adoption/infection at each time point (Allison 1984).
#' In survival analysis, hazard rate is formally defined as:
#'
#' \deqn{%
#' \lambda(t)=\lim_{h\to +0}\frac{F(t+h)-F(t)}{h}\frac{1}{1-F(t)} %
#' }{%
#' \lambda(t-1)= lim (t -> +0) [F(t+h)-F(t)]/h * 1/[1-F(t)] %
#' }
#'
#' By approximating \eqn{h=1}, we can rewrite the equation as
#'
#' \deqn{%
#' \lambda(t)=\frac{F(t+1)-F(t)}{1-F(t)} %
#' }{%
#' \lambda(t-1)= [F(t+1)-F(t)]/[1-F(t)] %
#' }
#'
#' If we estimate \eqn{F(t)}, 
#' the probability of not having adopted the innovation in time \eqn{t}, 
#' from the proportion of adopters in that time, 
#' such that \eqn{F(t) \sim q_t/n}{F(t) ~ q(t)/n}, we now have (ultimately for \eqn{t>1}):
#'
#' \deqn{%
#' \lambda(t)=\frac{q_{t+1}/n-q_t/n}{1-q_t/n} = \frac{q_{t+1} - q_t}{n - q_t} = \frac{q_t - q_{t-1}}{n - q_{t-1}} %
#' }{%
#' \lambda(t-1)= [q(t+1)/n-q(t)/n]/[1-q(t)/n] = [q(t+1) - q(t)]/[n - q(t)] = [q(t) - q(t-1)]/[n - q(t-1)] %
#' }
#' 
#' where \eqn{q_i}{q(i)} is the number of adopters in time \eqn{t}, 
#' and \eqn{n} is the number of vertices in the graph.
#'
#' The shape of the hazard rate indicates the pattern of new adopters over time.
#' Rapid diffusion with convex cumulative adoption curves will have 
#' hazard functions that peak early and decay over time. 
#' Slow concave cumulative adoption curves will have 
#' hazard functions that are low early and rise over time.
#' Smooth hazard curves indicate constant adoption whereas 
#' those that oscillate indicate variability in adoption behavior over time.
#' @source `{netdiffuseR}`
#' @references
#' ## On hazard rates
#' Allison, Paul D. 1984. 
#' _Event history analysis: Regression for longitudinal event data_. 
#' London: Sage Publications.
#' \doi{10.4135/9781412984195}
#'
#' Wooldridge, Jeffrey M. 2010. 
#' _Econometric Analysis of Cross Section and Panel Data_ (2nd ed.). 
#' Cambridge: MIT Press.
#' @examples
#' # To calculate the hazard rates at each time point
#'   smeg <- generate_smallworld(15, 0.025)
#' net_by_hazard(play_diffusion(smeg, transmissibility = 0.3))
#' @export
net_by_hazard <- function(.data){
  diff_model <- as_diffusion(.data)
  out <- (diff_model$I - dplyr::lag(diff_model$I)) / 
    (diff_model$n - dplyr::lag(diff_model$I))
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  names(out) <- paste0("t", diff_model$time)
  make_network_motif(out, net)
}


