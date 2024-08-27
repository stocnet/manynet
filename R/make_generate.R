#' Making networks with a stochastic element
#' 
#' @description These functions are similar to the `create_*` functions,
#'   but include some element of randomisation. 
#'   They are particularly useful for creating a distribution of networks 
#'   for exploring or testing network properties.
#'   
#'   - `generate_random()` generates a random network with ties appearing at some probability.
#'   - `generate_configuration()` generates a random network consistent with a
#'   given degree distribution.
#'   - `generate_smallworld()` generates a small-world structure via ring rewiring at some probability.
#'   - `generate_scalefree()` generates a scale-free structure via preferential attachment at some probability.
#'   - `generate_permutation()` generates a permutation of the network
#'   using a Fisher-Yates shuffle on both the rows and columns (for a one-mode network)
#'   or on each of the rows and columns (for a two-mode network).
#'   - `generate_utilities()` generates a random utility matrix.
#'   - `generate_fire()` generates a forest fire model.
#'   - `generate_islands()` generates an islands model.
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
#' @name make_generate
#' @family makes
#' @inheritParams make_create
#' @inheritParams is
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

#' @rdname make_generate 
#' @param p Proportion of possible ties in the network that are realised or,
#'   if integer greater than 1, the number of ties in the network.
#' @references 
#' Erdos, Paul, and Alfred Renyi. (1959). 
#' "\href{https://www.renyi.hu/~p_erdos/1959-11.pdf}{On Random Graphs I}" 
#' _Publicationes Mathematicae_. 6: 290–297.
#' @importFrom igraph sample_bipartite sample_gnp sample_gnm
#' @examples
#' graphr(generate_random(12, 0.4))
#' # graphr(generate_random(c(6, 6), 0.4))
#' @export
generate_random <- function(n, p = 0.5, directed = FALSE, with_attr = TRUE) {
  if(is_manynet(n)){
    m <- net_ties(n)
    directed <- is_directed(n)
    if(is_twomode(n)){
      g <- igraph::sample_bipartite(net_dims(n)[1], 
                                    net_dims(n)[2],
                                    m = m, type = "gnm",
                                    directed = directed,
                                    mode = "out")
    } else {
      g <- igraph::sample_gnm(net_nodes(n), 
                                    m = m,
                                    directed = directed)
    }
    if(with_attr) g <- bind_node_attributes(g, n)
  } else if (length(n) == 1) {
    if(p > 1){
      if(!as.integer(p)==p) stop("`p` must be an integer if above 1.")
      g <- igraph::sample_gnm(n, m = p, directed = directed)
    } else {
      g <- igraph::sample_gnp(n, p = p, directed = directed)
    }
  } else if (length(n) == 2) {
    if(p > 1){
      if(!as.integer(p)==p) stop("`p` must be an integer if above 1.")
      g <- igraph::sample_bipartite(n[1], n[2],
                                    m = p,
                                    type = "gnm",
                                    directed = directed,
                                    mode = "out")
    } else {
      g <- igraph::sample_bipartite(n[1], n[2],
                                    p = p,
                                    type = "gnp",
                                    directed = directed,
                                    mode = "out")
    }
    
  } else {
    stop("`n` must be of length=1 for a one-mode network or length=2 for a two-mode network.")
  }
  g
}

#' @rdname make_generate 
#' @importFrom igraph sample_degseq
#' @export
generate_configuration <- function(.data){
  if(is_twomode(.data)){
    degs <- node_deg(.data)
    outs <- ifelse(!c(attr(degs, "mode")),c(degs),rep(0,length(degs)))
    ins <- ifelse(c(attr(degs, "mode")),c(degs),rep(0,length(degs)))
    out <- igraph::sample_degseq(outs, ins, method = "simple.no.multiple")
    out <- as_tidygraph(out) %>% mutate(type = c(attr(degs, "mode")))
  } else {
    if(is_complex(.data) || is_multiplex(.data) && is_directed(.data)) 
      out <- igraph::sample_degseq(node_deg(.data, direction = "out"), 
                                   node_deg(.data, direction = "in"),
                                   method = "simple")
    if(is_complex(.data) || is_multiplex(.data) && !is_directed(.data)) 
      out <- igraph::sample_degseq(node_deg(.data), method = "simple")
    if(!(is_complex(.data) || is_multiplex(.data)) && is_directed(.data)) 
      out <- igraph::sample_degseq(node_deg(.data, direction = "out"), 
                                   node_deg(.data, direction = "in"), method = "simple.no.multiple")
    if(!(is_complex(.data) || is_multiplex(.data)) && !is_directed(.data)) 
      out <- igraph::sample_degseq(node_deg(.data), method = "simple.no.multiple")
  }
  as_tidygraph(out)
}

#' @rdname make_generate 
#' @param p Proportion of possible ties in the network that are realised or,
#'   if integer greater than 1, the number of ties in the network.
#' @references 
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#' “Collective Dynamics of ‘Small-World’ Networks.” 
#' _Nature_ 393(6684):440–42.
#' \doi{10.1038/30918}.
#' @importFrom igraph sample_smallworld
#' @examples
#' graphr(generate_smallworld(12, 0.025))
#' graphr(generate_smallworld(12, 0.25))
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

#' @rdname make_generate 
#' @param p Power of the preferential attachment, default is 1.
#' @importFrom igraph sample_pa
#' @references 
#' Barabasi, Albert-Laszlo, and Reka Albert. 1999. 
#' “Emergence of Scaling in Random Networks.” 
#' _Science_ 286(5439):509–12. 
#' \doi{10.1126/science.286.5439.509}.
#' @examples
#' graphr(generate_scalefree(12, 0.25))
#' graphr(generate_scalefree(12, 1.25))
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

#' @rdname make_generate 
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

#' @rdname make_generate 
#' @param contacts Number of contacts or ambassadors chosen from among existing
#'   nodes in the network.
#'   By default 1.
#'   See `igraph::sample_forestfire()`.
#' @param their_out Probability of tieing to a contact's outgoing ties.
#'   By default 0.
#' @param their_in Probability of tieing to a contact's incoming ties.
#'   By default 1.
#' @importFrom igraph sample_forestfire
#' @examples
#' generate_fire(10)
#' @export
generate_fire <- function(n, contacts = 1, their_out = 0, their_in = 1, directed = FALSE){
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n)==2){
    stop("There is currently no forest fire model implemented for two-mode networks.")
  } else {
    out <- igraph::sample_forestfire(n, 
                                     fw.prob = their_out, bw.factor = their_in,
                                     ambs = contacts, directed = directed)
  }
  as_tidygraph(out)
}

#' @rdname make_generate 
#' @param islands Number of islands or communities to create.
#'   By default 2.
#'   See `igraph::sample_islands()` for more.
#' @param bridges Number of bridges between islands/communities.
#'   By default 1.
#' @importFrom igraph sample_islands
#' @examples
#' generate_islands(10)
#' @export
generate_islands <- function(n, islands = 2, p = 0.5, bridges = 1, directed = FALSE){
  directed <- infer_directed(n, directed)
  if(is_manynet(n)){
    m <- net_nodes(n)
    extra_ties <- ifelse(islands > 2, islands * bridges, bridges)
    aimed_ties <- net_ties(n) - extra_ties
    m <- mean(c(table(cut(seq.int(m), islands, labels = FALSE))))
    p <-  (aimed_ties/islands) / ifelse(directed, m*(m-1), (m*(m-1))/2)
    if(p > 1) p <- 1
  } 
  n <- infer_n(n)
  if(length(n)==2){
    stop("There is currently no island model implemented for two-mode networks.")
  } else {
    out <- igraph::sample_islands(islands.n = islands,
                                  islands.size = c(table(cut(seq.int(n), islands, labels = FALSE))),
                                  islands.pin = p,
                                  n.inter = bridges)
    if(net_nodes(out) != n) out <- delete_nodes(out, order(node_constraint(out), decreasing = TRUE)[1:(net_nodes(out)-n)])
    if(directed) out <- to_directed(out)
  }
  as_tidygraph(out)
}

#' @rdname make_generate 
#' @param ties Number of ties to add per new node.
#'   By default a uniform random sample from 1 to 4 new ties.
#' @param agebins Number of aging bins.
#'   By default either \eqn{\frac{n}{10}} or 1,
#'   whichever is the larger.
#'   See `igraphr::sample_last_cit()` for more.
#' @importFrom igraph sample_last_cit
#' @examples
#' generate_citations(10)
#' @export
generate_citations <- function(n, ties = sample(1:4,1), agebins = max(1, n/10), directed = FALSE){
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  out <- igraph::sample_last_cit(n, edges = ties, agebins = agebins, directed = directed)
  as_tidygraph(out)
}

