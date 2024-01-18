#' Making networks with a stochastic element
#' 
#' @description These functions are similar to the `create_*` functions,
#'   but include some element of randomisation. 
#'   They are particularly useful for creating a distribution of networks 
#'   for exploring or testing network properties.
#'   
#'   - `generate_random()` generates a random network with a particular probability.
#'   - `generate_smallworld()` generates a small-world structure via ring rewiring.
#'   - `generate_scalefree()` generates a scale-free structure via preferential attachment.
#'   - `generate_permutation()` generates a permutation of the network
#'   using a Fisher-Yates shuffle on both the rows and columns (for a one-mode network)
#'   or on each of the rows and columns (for a two-mode network).
#'   - `generate_utilities()` generates a random utility matrix.
#' @details
#'   Note that passing the first argument an empirical network
#'   will prompt these functions to generate a network with the same dimensions.
#' @name generate
#' @family makes
#' @inheritParams create
#' @inheritParams is
#' @param directed Whether to generate network as directed. By default FALSE.
#' @return By default an `igraph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_matrix()`, `as_tidygraph()`, or `as_network()`.
NULL

#' @rdname generate 
#' @param p Proportion of possible ties in the network that are realised or,
#'   if integer greater than 1, the number of ties in the network.
#' @references 
#' Erdos, Paul, and Alfred Renyi. (1959). 
#' "\href{https://www.renyi.hu/~p_erdos/1959-11.pdf}{On Random Graphs I}" 
#' _Publicationes Mathematicae_. 6: 290–297.
#' @importFrom igraph sample_bipartite sample_gnp sample_gnm
#' @examples
#' autographr(generate_random(12, 0.4))
#' # autographr(generate_random(c(6, 6), 0.4))
#' @export
generate_random <- function(n, p = 0.5, directed = FALSE, with_attr = TRUE) {
  if(is_manynet(n)){
    m <- network_ties(n)
    directed <- is_directed(n)
    if(is_twomode(n)){
      g <- igraph::sample_bipartite(network_dims(n)[1], 
                                    network_dims(n)[2],
                                    m = m, type = "gnm",
                                    directed = directed,
                                    mode = "out")
    } else {
      g <- igraph::sample_gnm(network_nodes(n), 
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
                                    type = "gmp",
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

#' @rdname generate 
#' @param p Proportion of possible ties in the network that are realised or,
#'   if integer greater than 1, the number of ties in the network.
#' @references 
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#' “Collective Dynamics of ‘Small-World’ Networks.” 
#' _Nature_ 393(6684):440–42.
#' \doi{10.1038/30918}.
#' @importFrom igraph sample_smallworld
#' @examples
#' autographr(generate_smallworld(12, 0.025))
#' autographr(generate_smallworld(12, 0.25))
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

#' @rdname generate 
#' @param p Power of the preferential attachment, default is 1.
#' @importFrom igraph sample_pa
#' @references 
#' Barabasi, Albert-Laszlo, and Reka Albert. 1999. 
#' “Emergence of Scaling in Random Networks.” 
#' _Science_ 286(5439):509–12. 
#' \doi{10.1126/science.286.5439.509}.
#' @examples
#' autographr(generate_scalefree(12, 0.25))
#' autographr(generate_scalefree(12, 1.25))
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

#' @rdname generate 
#' @param with_attr Logical whether any attributes of the object
#'   should be retained. 
#'   By default TRUE. 
#' @examples
#' autographr(ison_adolescents)
#' autographr(generate_permutation(ison_adolescents))
#' @export
generate_permutation <- function(.data, with_attr = TRUE) {
  out <- as_matrix(.data)
  if(is_twomode(.data)){
    out <- .r2perm(out)
  } else {
    out <- .r1perm(out)
  }
  if(with_attr) out <- bind_node_attributes(out, .data)
  out
}

#' @rdname generate 
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

# Helper functions ------------------

.r1perm <- function(m) {
  n <- sample(seq_len(dim(m)[1]))
  if(is_labelled(m)){
    p <- matrix(data = m[n, n], nrow = dim(m)[1], ncol = dim(m)[2],
                dimnames = dimnames(m))
  } else {
    p <- matrix(data = m[n, n], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  p
}

.r2perm <- function(m) {
  n <- sample(seq_len(dim(m)[1]))
  o <- sample(seq_len(dim(m)[2]))
  if(is_labelled(m)){
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2],
                dimnames = dimnames(m))
  } else {
    p <- matrix(data = m[n, o], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  p
}
