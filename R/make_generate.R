# Conditional ####

#' Making unconditional and conditional random networks
#' 
#' @description These functions are similar to the `create_*` functions,
#'   but include some element of randomisation. 
#'   They are particularly useful for creating a distribution of networks 
#'   for exploring or testing network properties.
#'   
#'   - `generate_random()` generates a random network with ties appearing at some probability.
#'   - `generate_configuration()` generates a random network consistent with a
#'   given degree distribution.
#'   - `generate_man()` generates a random network conditional on the dyad census
#'   of Mutual, Asymmetric, and Null dyads, respectively.
#'   - `generate_utilities()` generates a random utility matrix.
#'
#'   These functions can create either one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#' @name make_random
#' @family makes
#' @inheritParams make_create
#' @inheritParams mark_is
#' @param directed Whether to generate network as directed. By default FALSE.
#' @return By default a `tbl_graph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, or `as_network()`.
#'   
#'   By default, all networks are created as undirected.
#'   This can be overruled with the argument `directed = TRUE`.
#'   This will return a directed network in which the arcs are
#'   out-facing or equivalent.
#'   This direction can be swapped using `to_redirected()`.
#'   In two-mode networks, `generate_random(directed = TRUE)` points every
#'   tie from the first mode to the second, and the other functions ignore
#'   the directed argument.
NULL

#' @rdname make_random 
#' @param p Proportion of possible ties in the network that are realised or,
#'   if integer greater than 1, the number of ties in the network.
#' @param with_attr Logical whether any attributes of the object
#'   should be retained. 
#'   By default TRUE. 
#' @references 
#' ## On random networks
#' Erdos, Paul, and Alfred Renyi. 1959. 
#' "\href{https://www.renyi.hu/~p_erdos/1959-11.pdf}{On Random Graphs I}" 
#' _Publicationes Mathematicae_. 6: 290–297.
#' @examples
#' generate_random(12, 0.4)
#' # generate_random(c(6, 6), 0.4)
#' @export
generate_random <- function(n, p = 0.5, directed = FALSE, with_attr = TRUE) {
  if(is_manynet(n)){
    m <- net_ties(n)
    directed <- is_directed(n)
    if(is_twomode(n)){
      if (utils::packageVersion("igraph") >= "2.2.0") {
        g <- igraph::sample_bipartite_gnm(mode_nodes(n)[1],
                                          mode_nodes(n)[2],
                                          m = m,
                                          directed = directed,
                                          mode = "out")
      } else {
        g <- igraph::sample_bipartite(mode_nodes(n)[1], 
                                      mode_nodes(n)[2],
                                      m = m, type = "gnm",
                                      directed = directed,
                                      mode = "out")
      }
    } else {
      g <- igraph::sample_gnm(net_nodes(n), 
                              m = m,
                              directed = directed)
    }
    if(with_attr) g <- bind_node_attributes(g, n)
  } else if (length(n) == 1) {
    if(p > 1){
      if(as.integer(p)!=p) snet_abort("`p` must be an integer if above 1.")
      g <- igraph::sample_gnm(n, m = p, directed = directed)
    } else {
      g <- igraph::sample_gnp(n, p = p, directed = directed)
    }
  } else if (length(n) == 2) {
    if(p > 1){
      if(as.integer(p)!=p) snet_abort("`p` must be an integer if above 1.")
      if (utils::packageVersion("igraph") >= "2.2.0") {
        g <- igraph::sample_bipartite_gnm(n[1], n[2],
                                          m = p,
                                          directed = directed,
                                          mode = "out")
      } else {
        g <- igraph::sample_bipartite(n[1], n[2],
                                      m = p,
                                      type = "gnm",
                                      directed = directed,
                                      mode = "out")
    }
  } else {
    if (utils::packageVersion("igraph") >= "2.2.0") {
      g <- igraph::sample_bipartite_gnp(n[1], n[2],
                                        p = p,
                                        directed = directed,
                                        mode = "out")
    } else {
      g <- igraph::sample_bipartite(n[1], n[2],
                                    p = p,
                                    type = "gnp",
                                    directed = directed,
                                    mode = "out")
    }
    }
    
  } else {
    snet_abort("`n` must be of length=1 for a one-mode network or length=2 for a two-mode network.")
  }
  g
}

#' @rdname make_random 
#' @references
#' ## On configuration models
#' Bollobas, Bela. 1980.
#' "A Probabilistic Proof of an Asymptotic Formula for the Number of Labelled Regular Graphs".
#' _European Journal of Combinatorics_ 1: 311-316.
#' @importFrom igraph sample_degseq
#' @export
generate_configuration <- function(.data){
  method1 <- "configuration"
  method2 <- "fast.heur.simple"
  if(is_twomode(.data)){
    # 'type' is how an igraph marks the modes; a stocnet marks them in 'mode',
    # so the mark is asked for rather than the attribute that holds it.
    modes <- node_is_mode(.data)
    degs <- .node_deg(.data)
    outs <- ifelse(!modes,c(degs),rep(0,length(degs)))
    ins <- ifelse(modes,c(degs),rep(0,length(degs)))
    out <- igraph::sample_degseq(outs, ins, method = method2)
    out <- as_tidygraph(out) |> add_node_attribute("type", modes)
  } else {
    if(is_complex(.data) || is_multiplex(.data) && is_directed(.data)) 
      out <- igraph::sample_degseq(.node_deg(.data, direction = "out"), 
                                   .node_deg(.data, direction = "in"),
                                   method = method1)
    if(is_complex(.data) || is_multiplex(.data) && !is_directed(.data)) 
      out <- igraph::sample_degseq(.node_deg(.data), method = method1)
    if(!(is_complex(.data) || is_multiplex(.data)) && is_directed(.data)) 
      out <- igraph::sample_degseq(.node_deg(.data, direction = "out"), 
                                   .node_deg(.data, direction = "in"), 
                                   method = method2)
    if(!(is_complex(.data) || is_multiplex(.data)) && !is_directed(.data)) 
      out <- igraph::sample_degseq(.node_deg(.data), 
                                   method = method2)
  }
  as_tidygraph(out)
}

.node_deg <- function(.data, direction = "all"){
  if(is_twomode(.data)){
    out <- igraph::degree(as_igraph(.data), mode = ifelse(direction == "out", "out", "in"))
  } else {
    out <- igraph::degree(as_igraph(.data), mode = ifelse(direction == "out", "out", 
                                                  ifelse(direction == "in", "in", "all")))
  }
  out
}

#' @rdname make_random 
#' @param man Vector of Mutual, Asymmetric, and Null dyads, respectively.
#'   These are treated as proportions, e.g. `c(0.25, 0.5, 0.25)`;
#'   counts such as `c(10,0,20)` are read as relative weights and normalised,
#'   so the dyad census is reproduced in expectation rather than exactly.
#'   Is inferred from `n` if it is an existing network object,
#'   and otherwise defaults to `c(0.25, 0.5, 0.25)`,
#'   which is the dyad distribution of a random (Erdős-Rényi) digraph
#'   in which each arc is present with probability 0.5.
#'
#'   For two-mode networks, `man` is conditioned on the dyads between
#'   the modes.
#'   Since ties in two-mode networks are undirected,
#'   both mutual and asymmetric dyads are realised as a tie,
#'   so only their sum is consequential there.
#' @references
#' ## On dyad-census conditioned networks
#' Holland, Paul W., and Samuel Leinhardt. 1976.
#' “Local Structure in Social Networks.”
#' In D. Heise (Ed.), _Sociological Methodology_, pp 1-45.
#' San Francisco: Jossey-Bass.
#' @examples
#' generate_man(6)
#' generate_man(c(4, 6))
#' @export
generate_man <- function(n, man = NULL){
  thisRequires("sna")
  if(!is.null(man)){
    if(length(man)!=3)
      snet_abort(paste("`man` should be a numeric vector of length 3,",
                       "giving the Mutual, Asymmetric, and Null dyads,",
                       "but a vector of length", length(man), "was given."))
    dcen <- man
  } else if (is_manynet(n)){
    dcen <- .net_by_dyad(n)
    if(length(dcen)==2) dcen <- c(dcen[1],0,dcen[2])
  } else dcen <- c(0.25, 0.5, 0.25)
  n <- infer_n(n)
  if(length(n)==2) .rgbman(n[1], n[2], dcen) else
    as_tidygraph(sna::rguman(1, n, dcen[1], dcen[2], dcen[3]))
}

# Two-mode counterpart to `sna::rguman()`, which is defined only for
# square (one-mode) networks. Each of the dyads between the modes is
# tied with the probability that it is not null, since mutual and
# asymmetric dyads are indistinguishable where ties are undirected.
.rgbman <- function(n1, n2, dcen) {
  p <- (dcen[1] + dcen[2]) / sum(dcen)
  out <- matrix(stats::rbinom(n1*n2, 1, p), n1, n2)
  # `twomode` is declared since a square matrix would otherwise be
  # coerced into a one-mode network
  as_tidygraph(as_igraph(out, twomode = TRUE))
}

.net_by_dyad <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if (manynet::is_twomode(.data)) {
    # `igraph::dyad_census()` counts every pair of nodes, including those
    # within the modes, which are not dyads in a two-mode network
    mat <- manynet::as_matrix(.data)
    ties <- sum(mat != 0)
    return(c(Mutual = ties, Asymmetric = 0, Null = length(mat) - ties))
  }
  out <- suppressWarnings(igraph::dyad_census(manynet::as_igraph(.data)))
  out <- unlist(out)
  names(out) <- c("Mutual", "Asymmetric", "Null")
  if (!manynet::is_directed(.data)) out <- out[c(1, 3)]
  out
}

#' @rdname make_random 
#' @param steps Number of simulation steps to run.
#'   By default 1: a single, one-shot simulation.
#'   If more than 1, further iterations will update the utilities
#'   depending on the values of the volatility and threshold parameters.
#' @param volatility How much change there is between steps.
#'   Only if volatility is more than 1 do further simulation steps make sense.
#'   This is passed on to `stats::rnorm` as the `sd` or standard deviation
#'   parameter.
#' @param threshold This parameter can be used to mute or disregard stepwise
#'   changes in utility that are minor.
#'   The default 0 will recognise all changes in utility, 
#'   but raising the threshold will mute any changes less than this threshold.
#' @export
generate_utilities <- function(n, steps = 1, volatility = 0, threshold = 0){
  
  utilities <- matrix(stats::rnorm(n*n, 0, 1), n, n) 
  diag(utilities) <- 0
  utilities <- utilities / rowSums(utilities)
  
  if(steps > 1 && volatility > 0){
    iter <- 1
    while (iter < steps){
      utility_update <- matrix(stats::rnorm(n*n, 0, volatility), n, n)
      diag(utility_update) <- 0
      utility_update[abs(utility_update) < threshold] <- 0
      utilities <- utilities + utility_update
      iter <- iter + 1
    }
  }
  as_igraph(utilities)
}

# Growth ####

#' Making networks that grow
#' 
#' @description These functions are similar to the `create_*` functions,
#'   but include some element of randomisation. 
#'   They are particularly useful for creating a distribution of networks 
#'   for exploring or testing network properties.
#'   The networks here grow: nodes arrive one at a time,
#'   and each new node ties to nodes that arrived before it.
#'   The functions differ in how a new node chooses whom to tie to.
#'   
#'   - `generate_scalefree()` generates a scale-free structure via preferential attachment at some probability.
#'   - `generate_fire()` generates a forest fire model.
#'   - `generate_citations()` generates a citations model.
#'
#'   These functions can create either one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#' @name make_growth
#' @family makes
#' @inheritParams make_create
#' @inheritParams make_random
#' @inheritParams mark_is
#' @param directed Whether to generate network as directed. By default FALSE.
#' @return By default a `tbl_graph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, or `as_network()`.
#'   
#'   By default, all networks are created as undirected.
#'   This can be overruled with the argument `directed = TRUE`.
#'   This will return a directed network in which the arcs are
#'   out-facing or equivalent.
#'   This direction can be swapped using `to_redirected()`.
#'   In two-mode networks, the directed argument is ignored.
NULL

#' @rdname make_growth 
#' @param p Power of the preferential attachment, default is 1.
#' @importFrom igraph sample_pa
#' @references 
#' ## On scale-free networks
#' Barabasi, Albert-Laszlo, and Reka Albert. 1999. 
#' “Emergence of Scaling in Random Networks.” 
#' _Science_ 286(5439):509–12. 
#' \doi{10.1126/science.286.5439.509}
#' @examples
#' generate_scalefree(12, 0.25)
#' generate_scalefree(12, 1.25)
#' @export
generate_scalefree <- function(n, p = 1, directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n) > 1){
    g <- matrix(0, n[1], n[2])
    for(i in seq_len(nrow(g))){
      if(i==1) g[i,1] <- 1
      else g[i, sample.int(ncol(g), size = 1,
                           prob = (colSums(g)^p + 1))] <- 1
    }
    g <- as_igraph(g, twomode = TRUE)
  } else {
    g <- igraph::sample_pa(n, power = p, directed = directed)
  }
  g
}

#' @rdname make_growth 
#' @param contacts Number of contacts or ambassadors chosen from among existing
#'   nodes in the network.
#'   By default 1.
#'   See `igraph::sample_forestfire()`.
#' @details
#'   In a one-mode network, each burn step is a single hop, so each tie the
#'   fire creates closes a triangle.
#'   A tie in a two-mode network crosses modes, so the shortest closure there
#'   is the four-cycle rather than the triangle.
#'   In a two-mode network each burn step is therefore a two-path hop across
#'   the other mode, and each tie the fire creates closes a four-cycle.
#'   Both modes grow over the course of the simulation.
#' @param their_out Probability of tieing to a contact's outgoing ties.
#'   By default 0.
#'   In a two-mode network, this is instead the probability of burning across
#'   each two-path, and so of closing a four-cycle.
#' @param their_in Probability of tieing to a contact's incoming ties.
#'   By default 1.
#'   This is a factor on `their_out` rather than a probability in its own
#'   right, so `their_out = 0` gives a tree whatever `their_in` is set to.
#'   In a two-mode network, `their_out * their_in` is instead the probability
#'   that a newly burned node re-ignites and spreads the fire further.
#' @importFrom igraph sample_forestfire
#' @references
#' ## On the forest-fire model
#' Leskovec, Jure, Jon Kleinberg, and Christos Faloutsos. 2007. 
#' "Graph evolution: Densification and shrinking diameters". 
#' _ACM transactions on Knowledge Discovery from Data_, 1(1): 2-es.
#' \doi{10.1145/1217299.1217301}
#' @examples
#' generate_fire(10)
#' generate_fire(c(10, 6))
#' @export
generate_fire <- function(n, contacts = 1, their_out = 0, their_in = 1, directed = FALSE){
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n)==2){
    out <- .fire_twomode(n, contacts, their_out, their_in)
  } else {
    out <- igraph::sample_forestfire(n, 
                                     fw.prob = their_out, bw.factor = their_in,
                                     ambs = contacts, directed = directed)
  }
  as_tidygraph(out)
}

#' @rdname make_growth 
#' @param ties Number of ties to add per new node.
#'   By default a uniform random sample from 1 to 4 new ties.
#'   In a two-mode network, each new node of the first mode ties to this many
#'   nodes of the second mode, chosen by how recently each was last tied to.
#' @param agebins Number of aging bins.
#'   By default either \eqn{\frac{n}{10}} or 1,
#'   whichever is the larger.
#'   See `igraphr::sample_last_cit()` for more.
#' @importFrom igraph sample_last_cit
#' @examples
#' generate_citations(10)
#' generate_citations(c(10, 6))
#' @export
generate_citations <- function(n, ties = sample(1:4,1), agebins = max(1, n/10), directed = FALSE){
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  stopifnot(is.scalar(ties))
  if(length(n)>1){
    out <- .citations_twomode(n, ties, agebins)
  } else {
    out <- igraph::sample_last_cit(n, edges = ties, agebins = agebins,
                                   directed = directed)
  }
  as_tidygraph(out)
}


# Groups ####

#' Making networks with groups
#' 
#' @description These functions are similar to the `create_*` functions,
#'   but include some element of randomisation. 
#'   They are particularly useful for creating a distribution of networks 
#'   for exploring or testing network properties.
#'   The networks here start from a planted structure,
#'   to which some random noise is then added.
#'   
#'   - `generate_smallworld()` generates a small-world structure via ring rewiring at some probability.
#'   - `generate_islands()` generates an islands model.
#'   - `generate_communities()` generates communities of unequal size
#'   among nodes of unequal degree.
#'
#'   `generate_islands()` and `generate_communities()` plant discrete groups,
#'   and record the group of each node in the node attribute `community`.
#'   `generate_smallworld()` plants local clusters around a ring instead,
#'   so its nodes do not belong to discrete groups.
#'
#'   `generate_smallworld()` and `generate_islands()` can create either 
#'   one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#'   `generate_communities()` creates only one-mode, undirected networks.
#' @name make_groups
#' @family makes
#' @inheritParams make_create
#' @inheritParams make_random
#' @inheritParams mark_is
#' @param directed Whether to generate network as directed. By default FALSE.
#' @return By default a `tbl_graph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, or `as_network()`.
#'   
#'   By default, all networks are created as undirected.
#'   This can be overruled with the argument `directed = TRUE`.
#'   This will return a directed network in which the arcs are
#'   out-facing or equivalent.
#'   This direction can be swapped using `to_redirected()`.
#'   In two-mode networks, the directed argument is ignored.
NULL

#' @rdname make_groups
#' @param p For `generate_smallworld()`, the probability that each tie of the
#'   ring is rewired, by default 0.05.
#'   For `generate_islands()`, the probability of a tie between two nodes of
#'   the same island, by default 0.5.
#' @references
#' ## On small-world networks
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#' “Collective Dynamics of ‘Small-World’ Networks.” 
#' _Nature_ 393(6684):440–42.
#' \doi{10.1038/30918}.
#' @importFrom igraph sample_smallworld
#' @examples
#' generate_smallworld(12, 0.025)
#' generate_smallworld(12, 0.25)
#' @export
generate_smallworld <- function(n, p = 0.05, directed = FALSE, width = 2) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n) > 1){
    g <- create_ring(n, width = width, directed = directed)
    g <- igraph::rewire(g, igraph::each_edge(p = p))
  } else {
    g <- igraph::sample_smallworld(dim = 1, size = n, 
                                   nei = width, p = p)
    if(directed) g <- to_acyclic(g)
  }
  g
}

#' @rdname make_groups 
#' @param islands Number of islands to create.
#'   By default 2.
#'   See `igraph::sample_islands()` for more.
#'   In a two-mode network, each mode is cut into this many blocks,
#'   and a node of each mode that share a block are tied with probability `p`.
#' @param bridges Number of bridges between each pair of islands.
#'   By default 1.
#' @importFrom igraph sample_islands
#' @examples
#' generate_islands(10)
#' generate_islands(c(10, 6))
#' @export
generate_islands <- function(n, islands = 2, p = 0.5, bridges = 1, 
                             directed = FALSE){
  directed <- infer_directed(n, directed)
  if(is_manynet(n)){
    # both `igraph::sample_islands()` and `.islands_twomode()` add `bridges`
    # ties for each pair of islands, so the count of the pairs is what the
    # aimed tie count subtracts
    extra_ties <- choose(islands, 2) * bridges
    aimed_ties <- net_ties(n) - extra_ties
    if(is_twomode(n)){
      # a two-mode island has m1 * m2 possible ties, not m * (m-1) / 2
      dims <- infer_dims(n)
      m1 <- mean(c(table(cut(seq.int(dims[1]), islands, labels = FALSE))))
      m2 <- mean(c(table(cut(seq.int(dims[2]), islands, labels = FALSE))))
      p <- (aimed_ties/islands) / (m1*m2)
    } else {
      m <- net_nodes(n)
      m <- mean(c(table(cut(seq.int(m), islands, labels = FALSE))))
      p <-  (aimed_ties/islands) / ifelse(directed, m*(m-1), (m*(m-1))/2)
    }
    if(p > 1) p <- 1
    if(p < 0) p <- 0
  } 
  n <- infer_n(n)
  if(length(n)==2){
    out <- .islands_twomode(n, islands, p, bridges)
  } else {
    out <- igraph::sample_islands(islands.n = islands,
                                  islands.size = ceiling(n/islands),
                                  islands.pin = p,
                                  n.inter = bridges)
    # the island of each node is recorded before any surplus nodes are
    # deleted, since the deletion does not take the same number from each
    out <- igraph::set_vertex_attr(out, "community",
                                   value = rep(seq_len(islands),
                                               each = ceiling(n/islands)))
    if(net_nodes(out) != n) out <- delete_nodes(out,
          order(.node_constraint(out), decreasing = TRUE)[1:(net_nodes(out)-n)])
    if(directed) out <- to_directed(out)
  }
  as_tidygraph(out)
}

.node_constraint <- function(.data) {
  .data <- manynet::expect_nodes(.data)
  if (manynet::is_twomode(.data)) {
    get_constraint_scores <- function(mat) {
      inst <- colnames(mat)
      rowp <- mat * matrix(1 / rowSums(mat), nrow(mat), ncol(mat))
      colp <- mat * matrix(1 / colSums(mat), nrow(mat), ncol(mat), byrow = T)
      res <- vector()
      for (i in inst) {
        ci <- 0
        membs <- names(which(mat[, i] > 0))
        for (a in membs) {
          pia <- colp[a, i]
          oth <- membs[membs != a]
          pbj <- 0
          if (length(oth) == 1) {
            for (j in inst[mat[oth, ] > 0 & inst != i]) {
              pbj <- sum(pbj, sum(colp[oth, i] * rowp[oth, j] * colp[a, j]))
            }
          } else {
            for (j in inst[colSums(mat[oth, ]) > 0 & inst != i]) {
              pbj <- sum(pbj, sum(colp[oth, i] * rowp[oth, j] * colp[a, j]))
            }
          }
          cia <- (pia + pbj)^2
          ci <- sum(ci, cia)
        }
        res <- c(res, ci)
      }
      names(res) <- inst
      res
    }
    inst.res <- get_constraint_scores(manynet::as_matrix(.data))
    actr.res <- get_constraint_scores(t(manynet::as_matrix(.data)))
    res <- c(actr.res, inst.res)
  } else {
    res <- igraph::constraint(manynet::as_igraph(.data), 
                              nodes = igraph::V(.data), 
                              weights = NULL)
  }
  res
}

#' @rdname make_groups
#' @param degree The mean degree of the nodes.
#'   By default 10, or less in a network too small for that.
#' @param max_degree The maximum degree of the nodes.
#'   By default three times `degree`, or `n - 1` if that is smaller.
#' @param mixing The share of each node's ties that go to nodes in other
#'   communities, from 0 to 1.
#'   By default 0.1.
#'   A low share gives communities that are easy to tell apart.
#'   A share above 0.5 gives communities with more ties out than in.
#' @param community A vector of the minimum and the maximum community size.
#'   By default `c(degree + 1, max_degree + 1)`,
#'   so that a node of any degree can find all the ties it keeps within its
#'   community.
#' @param degree_exp The exponent of the power law from which the degrees
#'   are drawn.
#'   By default 2.
#' @param community_exp The exponent of the power law from which the community
#'   sizes are drawn.
#'   By default 1.
#' @details
#'   `generate_communities()` follows the benchmark of Lancichinetti,
#'   Fortunato, and Radicchi (2008).
#'   Degrees and community sizes are each drawn from a power law,
#'   so that there are a few large hubs and a few large communities,
#'   as in many observed networks.
#'   Each node then keeps a share `1 - mixing` of its ties within its
#'   community and sends the remaining share to nodes in other communities.
#'   Ties within and between communities are drawn at random from all the
#'   simple networks with those degrees.
#'   Where a degree sequence cannot be realised as a simple network,
#'   or a node has more ties to keep than its community has other members,
#'   ties are dropped or sent elsewhere, so the realised degrees and mixing
#'   can differ a little from those asked for.
#'   They differ most in small networks and where there are few communities.
#'
#'   `generate_islands()` is a simpler model of the same kind:
#'   its islands are all the same size, the ties in each island are random,
#'   and each pair of islands is joined by a fixed number of bridges.
#' @references
#' ## On communities
#' Lancichinetti, Andrea, Santo Fortunato, and Filippo Radicchi. 2008.
#' “Benchmark Graphs for Testing Community Detection Algorithms.”
#' _Physical Review E_ 78(4):046110.
#' \doi{10.1103/PhysRevE.78.046110}
#' @importFrom igraph sample_degseq is_graphical
#' @examples
#' generate_communities(50)
#' generate_communities(50, mixing = 0.4)
#' @export
generate_communities <- function(n, degree = NULL, max_degree = NULL,
                                 mixing = 0.1, community = NULL,
                                 degree_exp = 2, community_exp = 1) {
  n <- infer_n(n)
  if (length(n) > 1)
    snet_abort("`generate_communities()` creates only one-mode networks.")
  if (n < 3) snet_abort("At least 3 nodes required to form communities.")
  if (!is.numeric(mixing) || length(mixing) != 1 || is.na(mixing) ||
      mixing < 0 || mixing > 1)
    snet_abort("`mixing` must be a single number from 0 to 1.")
  if (is.null(degree)) degree <- min(10, max(1, floor((n - 1) / 2)))
  if (is.null(max_degree)) max_degree <- min(n - 1, ceiling(3 * degree))
  if (!is.numeric(degree) || length(degree) != 1 || is.na(degree) ||
      degree < 1)
    snet_abort("`degree` must be a single number of at least 1.")
  if (!is.numeric(max_degree) || length(max_degree) != 1 ||
      is.na(max_degree) || max_degree != round(max_degree) ||
      max_degree < degree || max_degree > n - 1)
    snet_abort(paste("`max_degree` must be a single whole number that is at",
                     "least `degree` and less than the number of nodes."))
  if (is.null(community))
    community <- c(ceiling(degree) + 1, max_degree + 1)
  if (!is.numeric(community) || length(community) != 2 || anyNA(community) ||
      any(community != round(community)) || community[1] < 2 ||
      community[1] > community[2])
    snet_abort(paste("`community` must be a vector of two whole numbers,",
                     "the minimum (at least 2) and the maximum community size."))
  community <- pmin(community, n)
  
  degs <- .powerlaw_degrees(n, degree, max_degree, degree_exp)
  sizes <- .powerlaw_sizes(n, community[1], community[2], community_exp)
  # The ties a node sends out of its community are rounded up or down at
  # random, so that their share is `mixing` on average whatever the degree.
  outs <- degs * mixing
  outs <- floor(outs) + (stats::runif(n) < outs - floor(outs))
  memb <- .assign_communities(degs - outs, sizes)
  # Nodes are listed community by community, as in `generate_islands()`.
  degs <- degs[order(memb)]
  outs <- outs[order(memb)]
  memb <- sort(memb)
  # A node can tie to no more nodes outside its community than there are,
  # nor to more nodes inside it than its other members.
  outs <- pmin(outs, n - sizes[memb])
  ins <- pmin(degs - outs, sizes[memb] - 1)
  outs <- pmin(degs - ins, n - sizes[memb])
  # A community cannot send out more ties than all the others send out to
  # meet them, which is most likely where there are only a few communities.
  # The ties it sends out beyond that are kept within it where there is room.
  sent <- c(tapply(outs, memb, sum))
  over <- which(sent > sum(sent) - sent)
  if (length(over) == 1) {
    members <- which(memb == over)
    stubs <- rep(members, outs[members])
    kept <- tabulate(stubs[sample.int(length(stubs), 
                                      2 * sent[over] - sum(sent))], n)
    outs <- outs - kept
    ins <- pmin(ins + kept, sizes[memb] - 1)
  }
  
  within <- lapply(seq_along(sizes), function(k) {
    members <- which(memb == k)
    el <- .sample_simple(ins[members])
    cbind(members[el[, 1]], members[el[, 2]])
  })
  between <- .separate_ties(.sample_simple(outs), memb)
  ties <- rbind(do.call(rbind, within), between)
  out <- igraph::make_empty_graph(n, directed = FALSE)
  # a tie that could not be moved out of a community may repeat one within it
  out <- igraph::simplify(igraph::add_edges(out, t(ties)))
  out <- igraph::set_vertex_attr(out, "community", value = memb)
  as_tidygraph(out) |> 
    add_info(name = "Communities network")
}

# Draws `n` whole numbers from `lo` to `hi` with probability proportional to
# the number raised to the power of minus `exponent`.
.rpowerlaw <- function(n, lo, hi, exponent) {
  ks <- seq.int(lo, hi)
  ks[sample.int(length(ks), n, replace = TRUE, prob = ks^-exponent)]
}

# Draws degrees from a power law that is truncated above at `max_degree`.
# The mean of a truncated power law rises with its minimum, so the minimum is
# what sets the mean. No whole minimum gives exactly `degree`, so each node
# draws from one of the two minimums either side of it, in the proportion
# that makes the mean `degree` in expectation.
.powerlaw_degrees <- function(n, degree, max_degree, exponent) {
  ks <- seq_len(max_degree)
  w <- ks^-exponent
  means <- rev(cumsum(rev(ks * w))) / rev(cumsum(rev(w)))
  lo <- max(c(1, which(means <= degree)))
  if (lo == max_degree) return(rep(max_degree, n))
  share <- (means[lo + 1] - degree) / (means[lo + 1] - means[lo])
  lower <- stats::runif(n) < min(max(share, 0), 1)
  out <- integer(n)
  out[lower] <- .rpowerlaw(sum(lower), lo, max_degree, exponent)
  out[!lower] <- .rpowerlaw(sum(!lower), lo + 1, max_degree, exponent)
  out
}

# Draws community sizes from a power law until they hold all `n` nodes.
# The nodes left over when the next community no longer fits form a community
# of their own if there are at least `lo` of them, and are otherwise spread
# over the communities that are not yet of size `hi`.
.powerlaw_sizes <- function(n, lo, hi, exponent) {
  if (n < 2 * lo) return(n)
  for (attempt in seq_len(100)) {
    sizes <- .rpowerlaw(ceiling(n / lo), lo, hi, exponent)
    sizes <- sizes[cumsum(sizes) <= n]
    left <- n - sum(sizes)
    if (left >= lo) return(c(sizes, left))
    room <- hi - sizes
    if (sum(room) < left) next
    slots <- rep(seq_along(sizes), room)
    return(sizes + tabulate(slots[sample.int(length(slots), left)],
                            length(sizes)))
  }
  snet_abort(paste("Could not divide", n, "nodes into communities of", lo, 
                   "to", hi, "nodes. Please widen `community`."))
}

# Assigns each node to a community that has more members than the node has
# ties to keep inside it. Nodes with the most such ties choose first, since
# they have the fewest communities to choose from, and each chooses among the
# communities with room in proportion to that room.
# A node that no community with room is large enough for joins the largest one
# with room, and the ties it cannot keep there are sent out instead.
.assign_communities <- function(ins, sizes) {
  room <- sizes
  memb <- integer(length(ins))
  for (i in order(ins, decreasing = TRUE)) {
    fits <- which(room > 0 & sizes > ins[i])
    if (length(fits) == 0) {
      fits <- which(room > 0)
      fits <- fits[sizes[fits] == max(sizes[fits])]
    }
    pick <- fits[sample.int(length(fits), 1, prob = room[fits])]
    memb[i] <- pick
    room[pick] <- room[pick] - 1
  }
  memb
}

# Returns the ties of a random simple network with the degrees given.
# Degrees that no simple network has are first lowered, from the highest
# degrees down, until one does. Only degrees above zero are lowered, 
# and no ties is a network every node can have, so this always ends.
.sample_simple <- function(degs) {
  if (sum(degs) %% 2 == 1) {
    top <- which.max(degs)
    degs[top] <- degs[top] - 1
  }
  while (!igraph::is_graphical(degs)) {
    top <- order(degs, decreasing = TRUE)[1:2]
    top <- top[!is.na(top) & degs[top] > 0]
    if (length(top) < 2) degs[] <- 0 else degs[top] <- degs[top] - 1
  }
  if (sum(degs) == 0) return(matrix(integer(0), 0, 2))
  igraph::as_edgelist(igraph::sample_degseq(degs, 
                                            method = "edge.switching.simple"), 
                      names = FALSE)
}

# Moves ties that fell within a community to between communities.
# Each such tie swaps an end with another tie chosen at random,
# where neither new tie is within a community or already there.
# The swap keeps every degree. Ties still within a community after many
# passes are returned as they are, since there are then too few ties
# elsewhere to swap with, as where one community sends out more ties than
# all the others together.
.separate_ties <- function(el, memb) {
  n <- length(memb)
  key <- function(a, b) pmin(a, b) * (n + 1) + pmax(a, b)
  keys <- key(el[, 1], el[, 2])
  inside <- which(memb[el[, 1]] == memb[el[, 2]])
  found <- length(inside)
  passes <- 0
  while (length(inside) > 0 && passes < 50) {
    for (i in inside) {
      j <- sample.int(nrow(el), 1)
      a <- el[i, 1]
      b <- el[i, 2]
      other <- if (stats::runif(1) < 0.5) el[j, 1:2] else el[j, 2:1]
      if (memb[a] == memb[other[2]] || memb[other[1]] == memb[b] ||
          key(a, other[2]) %in% keys || key(other[1], b) %in% keys) next
      el[i, ] <- c(a, other[2])
      el[j, ] <- c(other[1], b)
      keys[c(i, j)] <- key(el[c(i, j), 1], el[c(i, j), 2])
    }
    inside <- which(memb[el[, 1]] == memb[el[, 2]])
    passes <- passes + 1
  }
  if (length(inside) > 10 && length(inside) > nrow(el) / 10)
    snet_warn(paste("{length(inside)} of {nrow(el)} ties between communities",
                    "could only be placed within a community,",
                    "so there is less mixing than asked for.",
                    "Consider a lower `mixing` or smaller communities."))
  el
}

# Two-mode helpers ####

# Returns the arrival order of nodes as a vector of mode indices,
# with exactly `n[1]` entries of 1 and `n[2]` entries of 2,
# interleaved in the ratio n[1]:n[2] so that both modes grow together.
.interleave_modes <- function(n) {
  t1 <- (seq_len(n[1]) - 0.5) / n[1]
  t2 <- (seq_len(n[2]) - 0.5) / n[2]
  c(rep(1L, n[1]), rep(2L, n[2]))[order(c(t1, t2))]
}

# A two-mode forest fire.
# In one mode a burn step is a single hop, so each new tie closes a triangle.
# A tie in a two-mode network crosses modes, so the shortest closure is the
# four-cycle. One burn step is therefore a 2-path hop across the other mode:
# from a burned node `e`, to a partner `a` of `e`, to another node `f` of `a`.
# Tieing the new node `v` to `f` closes the four-cycle v-e-a-f-v.
# As in one mode, `their_out` is the burn probability and `their_in` is a
# factor on it, so the defaults give the same minimal fire in both cases.
# `their_out` is the probability of burning across each two-path,
# and so of closing a four-cycle.
# `their_out * their_in` is the probability that a newly burned node
# re-ignites, which spreads the fire beyond the immediate closure.
.fire_twomode <- function(n, contacts, their_out, their_in) {
  g <- matrix(0, n[1], n[2])
  arrivals <- .interleave_modes(n)
  act <- c(0, 0)
  for (k in seq_along(arrivals)) {
    m <- arrivals[k]
    act[m] <- act[m] + 1
    v <- act[m]
    if (act[1] == 0 || act[2] == 0) next
    sub <- g[seq_len(act[1]), seq_len(act[2]), drop = FALSE]
    if (sum(sub) == 0) { # seed the first tie
      if (m == 1) g[v, 1] <- 1 else g[1, v] <- 1
      next
    }
    # ambassadors are drawn from the opposite mode, among those already tied
    cand <- if (m == 1) which(colSums(sub) > 0) else which(rowSums(sub) > 0)
    burnt <- cand[sample.int(length(cand), min(contacts, length(cand)))]
    queue <- burnt
    while (length(queue) > 0) {
      e <- queue[1]
      queue <- queue[-1]
      # the partners of `e`, which are in the same mode as `v`
      mem <- if (m == 1) which(sub[, e] > 0) else which(sub[e, ] > 0)
      if (length(mem) == 0) next
      # the nodes those partners reach, which are in the opposite mode to `v`
      reach <- if (m == 1) which(colSums(sub[mem, , drop = FALSE]) > 0) else
        which(rowSums(sub[, mem, drop = FALSE]) > 0)
      reach <- setdiff(reach, burnt)
      if (length(reach) == 0) next
      lit <- reach[stats::runif(length(reach)) < their_out]
      if (length(lit) == 0) next
      burnt <- c(burnt, lit)
      queue <- c(queue, lit[stats::runif(length(lit)) < their_out * their_in])
    }
    if (m == 1) g[v, burnt] <- 1 else g[burnt, v] <- 1
  }
  as_igraph(g, twomode = TRUE)
}

# A two-mode islands model, that is, a bipartite blockmodel with a planted
# diagonal. Each mode is cut into `islands` blocks. A node of the first mode
# and a node of the second mode that share a block are tied with probability
# `p`. Each pair of blocks is then joined by `bridges` further ties.
.islands_twomode <- function(n, islands, p, bridges) {
  b1 <- cut(seq_len(n[1]), islands, labels = FALSE)
  b2 <- cut(seq_len(n[2]), islands, labels = FALSE)
  g <- matrix(0, n[1], n[2])
  same <- outer(b1, b2, "==")
  g[same] <- stats::rbinom(sum(same), 1, p)
  if (bridges > 0 && islands > 1) {
    for (i in seq_len(islands - 1)) for (j in seq(i + 1, islands)) {
      cells <- which(outer(b1 == i, b2 == j, "&") |
                       outer(b1 == j, b2 == i, "&"))
      if (length(cells) == 0) next
      g[cells[sample.int(length(cells), min(bridges, length(cells)))]] <- 1
    }
  }
  igraph::set_vertex_attr(as_igraph(g, twomode = TRUE), "community",
                          value = c(b1, b2))
}

# A two-mode citation model.
# `igraph::sample_last_cit()` is a recency model: a new node cites old nodes
# with a probability that depends on how long ago each was last cited.
# Here the mechanism is kept but the target crosses the mode divide.
# A new node of the first mode ties to `ties` nodes of the second mode,
# each chosen by how recently that node was last tied to.
# Because both modes grow, new second-mode nodes keep entering in the freshest
# bin, so the concentration of ties turns over instead of locking in.
.citations_twomode <- function(n, ties, agebins) {
  agebins <- max(1, round(agebins))
  pref <- seq_len(agebins + 1)^-3
  g <- matrix(0, n[1], n[2])
  arrivals <- .interleave_modes(n)
  last_used <- rep(NA_integer_, n[2])
  act <- c(0, 0)
  for (k in seq_along(arrivals)) {
    m <- arrivals[k]
    act[m] <- act[m] + 1
    if (m == 2) {
      last_used[act[2]] <- k # a new node enters in the freshest bin
      next
    }
    if (act[2] == 0) next
    cols <- seq_len(act[2])
    prob <- pref[pmin(k - last_used[cols], agebins) + 1]
    chosen <- cols[sample.int(length(cols), min(ties, length(cols)),
                              prob = prob)]
    g[act[1], chosen] <- 1
    last_used[chosen] <- k
  }
  as_igraph(g, twomode = TRUE)
}
