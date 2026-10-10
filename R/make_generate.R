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
#'   given degree distribution, as the random counterpart of `create_degree()`.
#'   - `generate_man()` generates a random network conditional on the dyad census
#'   of Mutual, Asymmetric, and Null dyads, respectively.
#'   - `generate_utilities()` generates the utility that each node finds in
#'   each other node, as a signed and weighted network.
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
#' @template param_n
#' @templateVar p_detail For `generate_random()`, it is the proportion of possible ties in the network that are realised or, if an integer greater than 1, the number of ties in the network, by default 0.5. For `generate_configuration()`, it is the share of the ties that are switched with one another, by default 1, so that 0 returns the network as it was. For `generate_man()`, it is a vector of three: the proportions of Mutual, Asymmetric, and Null dyads, which is inferred from `n` if that is a network and is otherwise `c(0.25, 0.5, 0.25)`. For `generate_utilities()`, it is the share of the utilities that are ties, by default 1.
#' @template param_p
#' @template param_directed
#' @template return_make
#' @details
#'   `generate_configuration()` keeps the degree of every node and is random
#'   in which nodes are tied.
#'   The degrees are those of the network given as `n`,
#'   or those given as `outdegree` and `indegree`, as for `create_degree()`.
#'   It then switches the ends of pairs of ties, which changes who is tied to
#'   whom but not how many ties anyone has.
#'   With `p = 1`, the default, a network is drawn anew from those degrees.
#'   With a lower `p` only that share of the ties are switched,
#'   so that the result is in part the network it started from.
#'   
#'   For `generate_man()`, counts such as `p = c(10, 0, 20)` are read as
#'   relative weights and normalised,
#'   so the dyad census is reproduced in expectation rather than exactly.
#'   The default is the dyad distribution of a random (Erdős-Rényi) digraph
#'   in which each arc is present with probability 0.5.
#'   The network is directed unless there are no asymmetric dyads,
#'   as with `p = c(0.1, 0, 0.1)`, since it then has no direction to speak of.
#'   `directed` overrules this either way, and where an undirected network
#'   is asked for from asymmetric dyads, a dyad is tied where it would have
#'   been mutual or asymmetric.
#'   The same holds for two-mode networks, whose ties are undirected,
#'   so only the sum of the mutual and asymmetric dyads is consequential there.
#' @param ... Arguments under the names they had in earlier versions,
#'   which still work: `man` for `p` in `generate_man()`.
NULL

#' @rdname make_random 
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
#' @examples
#' generate_configuration(12, outdegree = 3)
#' generate_configuration(create_core(12), p = 0.25)
#' @export
generate_configuration <- function(n, p = 1, directed = FALSE,
                                   outdegree = NULL, indegree = NULL){
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1)
    snet_abort("`p` must be a single number from 0 to 1.")
  # The network to start from has the degrees to keep: the network given, or
  # that which `create_degree()` makes from the degrees given.
  if (is_manynet(n)) {
    .data <- n
  } else if (length(n) == 1 && (directed || !is.null(indegree))) {
    degs <- default_degree(n, outdegree, indegree, TRUE)
    .data <- create_degree(n, degs$outdegree, degs$indegree)
  } else .data <- create_degree(n, outdegree, indegree)
  if (p < 1) {
    # each switch moves two ties, and keeps the degree of every node
    switches <- round(p * net_ties(.data) / 2)
    if (switches == 0) {
      out <- .data
    } else if (is_twomode(.data)) {
      mat <- as_matrix(to_unweighted(.data))
      ties <- .switch_ties(which(mat != 0, arr.ind = TRUE), switches)
      mat[] <- 0
      mat[ties] <- 1
      out <- as_igraph(mat, twomode = TRUE)
    } else {
      out <- igraph::rewire(as_igraph(.data), 
                            igraph::keeping_degseq(niter = switches))
    }
    return(as_stocnet(out) |> 
             add_info(name = "Configuration network"))
  }
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
  as_stocnet(out) |> 
    add_info(name = "Configuration network")
}

# Switches the second nodes of pairs of two-mode ties, so that every node 
# keeps its degree and every tie still joins the two modes. A switch that
# would repeat a tie there already is not made.
.switch_ties <- function(ties, switches) {
  width <- max(ties[, 1]) + 1
  keys <- ties[, 2] * width + ties[, 1]
  for (s in seq_len(switches)) {
    pair <- sample.int(nrow(ties), 2)
    new <- ties[rev(pair), 2] * width + ties[pair, 1]
    if (any(new %in% keys)) next
    ties[pair, 2] <- ties[rev(pair), 2]
    keys[pair] <- new
  }
  ties
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
#' @references
#' ## On dyad-census conditioned networks
#' Holland, Paul W., and Samuel Leinhardt. 1976.
#' “Local Structure in Social Networks.”
#' In D. Heise (Ed.), _Sociological Methodology_, pp 1-45.
#' San Francisco: Jossey-Bass.
#' @examples
#' generate_man(6)
#' generate_man(6, p = c(0.3, 0, 0.7))
#' generate_man(c(4, 6))
#' @export
generate_man <- function(n, p = NULL, directed = NULL, ...){
  former <- .former_args(list(...), c(man = "p"))
  if (!is.null(former$p)) p <- former$p
  if(!is.null(p)){
    if(!is.numeric(p) || length(p)!=3 || anyNA(p) || any(p < 0) || sum(p) == 0)
      snet_abort(paste("`p` should be a numeric vector of length 3,",
                       "giving the Mutual, Asymmetric, and Null dyads,",
                       "but a vector of length", length(p), "was given."))
    dcen <- p
  } else if (is_manynet(n)){
    dcen <- .net_by_dyad(n)
    if(length(dcen)==2) dcen <- c(dcen[1],0,dcen[2])
  } else dcen <- c(0.25, 0.5, 0.25)
  # Unless the direction is stated, it is that of the network given, or else
  # a network with no asymmetric dyads has no direction to speak of.
  if (is.null(directed)) 
    directed <- if (is_manynet(n)) is_directed(n) else dcen[[2]] > 0
  n <- infer_n(n)
  if (length(n) == 2) {
    out <- .rgbman(n[1], n[2], dcen)
  } else if (directed) {
    thisRequires("sna")
    # the direction is stated, since a matrix that happens to be symmetric
    # would otherwise be read as undirected
    out <- igraph::graph_from_adjacency_matrix(
      sna::rguman(1, n, dcen[1], dcen[2], dcen[3]), mode = "directed")
  } else {
    # an undirected tie cannot be asymmetric, so a dyad is tied where it
    # would have been mutual or asymmetric, as in a two-mode network
    out <- igraph::sample_gnp(n, (dcen[[1]] + dcen[[2]]) / sum(dcen))
  }
  as_stocnet(out) |> 
    add_info(name = "Dyad census network")
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
#' @param form How the utilities are distributed between -1 and 1.
#'   - "normal" (the default) draws them from a normal distribution around 0,
#'   so that most nodes are close to indifferent about most others
#'   and only a few are much liked or much disliked.
#'   - "uniform" draws every utility between -1 and 1 as likely as any other.
#'   - "relative" draws them from a normal distribution and then divides each 
#'   node's utilities by the largest of them, whether liked or disliked,
#'   so that each node's utilities are relative to the node it feels most
#'   strongly about.
#' @param steps Number of moments at which to return the utilities.
#'   By default 1.
#'   If more than 1, the utilities change from each moment to the next
#'   by as much as `volatility` allows,
#'   and a longitudinal network with a wave for each moment is returned.
#' @param volatility How much the utilities change between moments.
#'   At each step a new set of utilities is drawn as `form` describes,
#'   multiplied by `volatility`, and added to those there are.
#'   By default 0.1, so that a utility changes by a tenth of a new draw.
#' @param inertia The share of utilities that keep their value at each step,
#'   from 0 to 1.
#'   These are the utilities that would have changed the least,
#'   so that with some inertia utilities stay as they are until a change large
#'   enough comes along, rather than all drifting a little at every step.
#'   By default 0, so that every utility changes at every step.
#' @details
#'   A utility is how much a node gains or loses from a tie to another node,
#'   from -1 for the most it could lose to 1 for the most it could gain.
#'   `generate_utilities()` returns these as the weights of a signed network.
#'   In a one-mode network every node has a utility for every other node.
#'   In a two-mode network each node of the first mode has a utility for 
#'   each node of the second mode.
#'   
#'   Utilities say what ties nodes would like, not what ties there are,
#'   so they can serve as the basis for a model of how ties come about.
#'   The examples show two such models.
#'   Where a node can tie to another without its consent,
#'   the ties are the positive utilities.
#'   Where a tie needs the consent of both nodes,
#'   the ties are those where the lower of the two utilities is positive.
#'   
#'   With `p` below 1, a node is indifferent to the utilities closest to 0,
#'   and only the share `p` of them that are furthest from 0 are ties,
#'   whether positive or negative.
#'   With `directed = FALSE`, the two nodes of a pair find the same in each
#'   other, and the network is undirected.
#'   
#'   Where there is more than one step, 
#'   how far from 0 a utility must be to be a tie is set at the first moment
#'   and kept, so a tie can come and go as a utility crosses it.
#'   A utility cannot rise above 1 or fall below -1, 
#'   and one that a change would take further stays at that limit
#'   until a change brings it back.
#'   Over many steps the utilities therefore spread out towards the limits,
#'   and more of them are far enough from 0 to be ties than at the start.
#' @examples
#' (utils <- generate_utilities(6))
#' generate_utilities(6, p = 0.25)
#' generate_utilities(6, directed = FALSE)
#' generate_utilities(c(4, 6), form = "uniform")
#' generate_utilities(6, steps = 3)
#' generate_utilities(6, steps = 3, volatility = 0.5, inertia = 0.8)
#' # the ties that one node wants
#' to_unsigned(utils, keep = "positive")
#' # the ties that both nodes want
#' to_unsigned(to_undirected(utils, rule = "min"), keep = "positive")
#' @export
generate_utilities <- function(n, p = 1, directed = TRUE,
                               form = c("normal", "uniform", "relative"),
                               steps = 1, volatility = 0.1, inertia = 0){
  form <- match.arg(form)
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1)
    snet_abort("`p` must be a single number from 0 to 1.")
  if (!is.numeric(steps) || length(steps) != 1 || is.na(steps) ||
      steps < 1 || steps != round(steps))
    snet_abort("`steps` must be a single whole number of at least 1.")
  if (!is.numeric(volatility) || length(volatility) != 1 || 
      is.na(volatility) || volatility < 0)
    snet_abort("`volatility` must be a single number of at least 0.")
  if (!is.numeric(inertia) || length(inertia) != 1 || is.na(inertia) ||
      inertia < 0 || inertia > 1)
    snet_abort("`inertia` must be a single number from 0 to 1.")
  labels <- if (is_manynet(n) && is_labelled(n)) node_names(n)
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  twomode <- length(n) == 2
  dims <- if (twomode) n else c(n, n)
  if (!is.null(labels))
    labels <- if (twomode) list(labels[seq_len(n[1])], labels[-seq_len(n[1])]) else
      list(labels, labels)
  # The pairs of nodes that have a utility of their own: every pair across
  # the modes, every other node for each node, or where the two nodes of a
  # pair find the same in each other, each pair once.
  pairs <- if (twomode) matrix(TRUE, dims[1], dims[2]) else
    if (directed) row(diag(n)) != col(diag(n)) else upper.tri(diag(n))
  # the two nodes of an undirected pair find the same in each other
  mirror <- function(mat) {
    if (!twomode && !directed) mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
    mat
  }
  
  draw <- function() {
    out <- switch(form,
                  uniform = stats::runif(prod(dims), -1, 1),
                  normal = stats::rnorm(prod(dims), 0, 1/3),
                  relative = stats::rnorm(prod(dims)))
    out <- matrix(out, dims[1], dims[2], dimnames = labels)
    # a node has no utility for a tie to itself
    if (!twomode) diag(out) <- 0
    if (form == "relative") 
      out <- out / pmax(apply(abs(out), 1, max), .Machine$double.eps)
    mirror(pmax(pmin(out, 1), -1))
  }
  utilities <- draw()
  # The ties are the share `p` of the utilities that are furthest from 0.
  # How far that is is found among the first utilities and then kept, so that
  # a tie can come and go as a utility changes.
  kept <- round(p * sum(pairs))
  cutoff <- if (kept == 0) Inf else if (kept >= sum(pairs)) 0 else
    sort(abs(utilities[pairs]), decreasing = TRUE)[kept + 1]
  as_ties <- function(utilities) {
    utilities[abs(utilities) <= cutoff] <- 0
    if (twomode) return(as_stocnet(utilities, twomode = TRUE))
    # the direction is stated, since a matrix that happens to be symmetric
    # would otherwise be read as undirected
    as_stocnet(igraph::graph_from_adjacency_matrix(
      utilities, mode = ifelse(directed, "directed", "upper"), weighted = TRUE))
  }
  if (steps == 1) 
    return(as_ties(utilities) |> add_info(name = "Utilities network"))
  
  waves <- vector("list", steps)
  waves[[1]] <- as_ties(utilities)
  for (step in seq_len(steps)[-1]) {
    # the utilities change, and not only those far enough from 0 to be ties
    change <- draw() * volatility
    # the smallest changes are those that inertia holds back
    held <- rank(abs(change[pairs]), ties.method = "first") <= 
      round(inertia * sum(pairs))
    change[pairs][held] <- 0
    utilities <- pmax(pmin(utilities + mirror(change), 1), -1)
    waves[[step]] <- as_ties(utilities)
  }
  from_times(waves) |> add_info(name = "Utilities network")
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
#'   - `generate_citations()` generates a citations model,
#'   in which each new node cites some of the nodes before it.
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
#' @template param_n
#' @templateVar p_detail For `generate_scalefree()`, it is the power of the preferential attachment, by default 1. For `generate_fire()`, it is the probability that the fire burns along each tie that a contact sends, and so that the new node ties to the node at its other end, by default 0. In a two-mode network, it is instead the probability of burning across each two-path, and so of closing a four-cycle. For `generate_citations()`, it is the probability that a node chooses each node that it cites at random, by default 1, where it otherwise cites the node that came most recently, so that 0 returns the same network every time.
#' @template param_p
#' @template param_directed
#' @templateVar degree_detail For `generate_citations()`, this is how many of the nodes before it each new node cites, by default a random number from 1 to 4. `Inf` has each node cite every node before it. In a two-mode network, each new node of the first mode ties to this many nodes of the second mode.
#' @template param_degree
#' @template return_make
#' @param ... Arguments under the names they had in earlier versions,
#'   which still work: `their_out` for `p` in `generate_fire()`, 
#'   and `ties` for `degree` in `generate_citations()`.
NULL

#' @rdname make_growth 
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
#' @param their_in Probability of tieing to a contact's incoming ties.
#'   By default 1.
#'   This is a factor on `p` rather than a probability in its own
#'   right, so `p = 0` gives a tree whatever `their_in` is set to.
#'   In a two-mode network, `p * their_in` is instead the probability
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
#' generate_fire(10, p = 0.3)
#' generate_fire(c(10, 6))
#' @export
generate_fire <- function(n, p = 0, directed = FALSE, contacts = 1, 
                          their_in = 1, ...){
  former <- .former_args(list(...), c(their_out = "p"))
  if (!is.null(former$p)) p <- former$p
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n)==2){
    out <- .fire_twomode(n, contacts, p, their_in)
  } else {
    out <- igraph::sample_forestfire(n, 
                                     fw.prob = p, bw.factor = their_in,
                                     ambs = contacts, directed = directed)
  }
  as_stocnet(out) |> 
    add_info(name = "Forest fire network")
}

#' @rdname make_growth 
#' @details
#'   In `generate_citations()`, a node that chooses at random whom to cite
#'   prefers the nodes that were cited most recently.
#'   With `p = 0` nothing is left to chance, and each node cites the `degree`
#'   nodes that arrived just before it.
#'   With `degree = Inf` as well, each node cites every node before it,
#'   which in a directed network is the complete citation network
#'   that `tidygraph::create_citation()` returns.
#'   With `p = 1` a node can cite another more than once.
#' @param agebins Number of aging bins.
#'   By default either \eqn{\frac{n}{10}} or 1,
#'   whichever is the larger.
#'   See `igraphr::sample_last_cit()` for more.
#' @importFrom igraph sample_last_cit
#' @examples
#' generate_citations(10)
#' generate_citations(10, p = 0.5, directed = TRUE, degree = 3)
#' generate_citations(10, p = 0, directed = TRUE, degree = Inf)
#' generate_citations(c(10, 6))
#' @export
generate_citations <- function(n, p = 1, directed = FALSE, 
                               degree = sample(1:4,1), 
                               agebins = max(1, n/10), ...){
  former <- .former_args(list(...), c(ties = "degree"))
  if (!is.null(former$degree)) degree <- former$degree
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1)
    snet_abort("`p` must be a single number from 0 to 1.")
  if (!is.numeric(degree) || length(degree) != 1 || is.na(degree) || 
      degree < 1)
    snet_abort("`degree` must be a single number of at least 1.")
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if(length(n)>1){
    out <- .citations_twomode(n, degree, agebins, p)
  } else if (p == 1 && is.finite(degree)) {
    out <- igraph::sample_last_cit(n, edges = degree, agebins = agebins,
                                   directed = directed)
  } else {
    out <- .citations_onemode(n, degree, agebins, p, directed)
  }
  as_stocnet(out) |> 
    add_info(name = "Citations network")
}

# A citation model in which chance has only a share in whom a node cites.
# Each node cites `degree` of the nodes before it, or all of them where there
# are fewer. Each citation is left to chance with probability `p`, and those
# that are not go to the nodes that came most recently. Chance prefers the
# nodes that were cited most recently, as `igraph::sample_last_cit()` does,
# but cites no node twice.
.citations_onemode <- function(n, degree, agebins, p, directed) {
  agebins <- max(1, round(agebins))
  pref <- seq_len(agebins + 1)^-3
  # how long ago a node was cited is counted in bins of this many arrivals,
  # as `igraph::sample_last_cit()` counts it
  binwidth <- n %/% agebins + 1
  # a node not yet cited is as likely to be as one cited longest ago
  last_used <- rep(-Inf, n)
  ties <- vector("list", n)
  for (i in seq_len(n)[-1]) {
    before <- seq_len(i - 1)
    cited <- .cite(before, min(degree, i - 1), p,
                   pref[pmin((i - last_used[before]) %/% binwidth, agebins) + 1])
    last_used[cited] <- i
    ties[[i]] <- rbind(i, cited)
  }
  igraph::add_edges(igraph::make_empty_graph(n, directed = directed),
                    unlist(ties))
}

# Chooses `k` of the nodes there are to cite, which are listed as they came.
# Each choice is left to chance with probability `p`, by the weights in
# `prob`, and the others go to the nodes that came last.
.cite <- function(nodes, k, p, prob) {
  # with nothing but chance, no draw is spent on how much of it there is
  chance <- if (p == 1) k else stats::rbinom(1, k, p)
  recent <- utils::tail(nodes, k - chance)
  if (chance == 0) return(recent)
  left <- seq_len(length(nodes) - length(recent))
  c(recent, nodes[left][sample.int(length(left), chance, prob = prob[left])])
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
#'   - `generate_core()` generates a core and a periphery,
#'   as the random counterpart of `create_core()`.
#'
#'   `generate_islands()` and `generate_communities()` plant discrete groups,
#'   and record the group of each node in the node attribute `community`.
#'   `generate_core()` plants two, and records whether each node is in the
#'   core in the node attribute `core`.
#'   `generate_smallworld()` plants local clusters around a ring instead,
#'   so its nodes do not belong to discrete groups.
#'
#'   These functions can create either one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#' @name make_groups
#' @family makes
#' @inheritParams make_create
#' @inheritParams mark_is
#' @template param_n
#' @templateVar p_detail For `generate_smallworld()`, it is the probability that each tie of the ring is rewired, by default 0.05. For `generate_islands()`, it is the probability of a tie between two nodes of the same island, by default 0.5. For `generate_communities()`, it is the probability that a tie of a node leads out of its community, known as the mixing parameter, by default 0.1: a low value gives communities that are easy to tell apart, and a value above 0.5 gives communities with more ties out than in. For `generate_core()`, it is the probability of a tie between a node of the core and a node of the periphery, by default 0.5, or a vector of three: the probabilities of a tie within the core, between the two, and within the periphery.
#' @template param_p
#' @template param_directed
#' @template param_width
#' @templateVar groups_detail For `generate_islands()`, by default 2, and sizes between a smallest and a largest are all as likely as one another. For `generate_communities()`, by default the smallest and the largest are `degree + 1` and `max_degree + 1`, so that a node of any degree can find all the ties it keeps within its community, and sizes between them are drawn from a power law.
#' @template param_groups
#' @templateVar degree_detail For `generate_communities()`, this is the mean over the nodes, by default 10, or less in a network too small for that. In a directed network, it is the mean number of ties that a node sends, which is also the mean number that a node receives. In a two-mode network, it is the mean degree of the nodes of the first mode, and the mean degree of the second mode follows from it, since both modes have the same ties: it is `degree * n[1] / n[2]`.
#' @template param_degree
#' @template return_make
#' @param ... Arguments under the names they had in earlier versions,
#'   which still work: `islands` for `groups` and `bridges` for `width` in 
#'   `generate_islands()`.
NULL

#' @rdname make_groups
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
#' @details
#'   In `generate_islands()`, the nodes of an island are tied to one another
#'   with probability `p`, and `width` further ties bridge each pair of
#'   islands.
#'   In a two-mode network, a node of each mode that share an island are tied
#'   with probability `p`.
#'   Where a network is given as `n`, `p` is instead inferred from it,
#'   so that as many ties are expected as that network has.
#' @examples
#' generate_islands(10)
#' generate_islands(12, groups = c(3, 4, 5), width = 2)
#' generate_islands(c(10, 6))
#' @export
generate_islands <- function(n, p = 0.5, directed = FALSE, width = 1,
                             groups = 2, ...){
  former <- .former_args(list(...), c(islands = "groups", bridges = "width"))
  if (!is.null(former$groups)) groups <- former$groups
  if (!is.null(former$width)) width <- former$width
  if (!is.numeric(width) || length(width) != 1 || is.na(width) ||
      width < 0 || width != round(width))
    snet_abort("`width` must be a single whole number of at least 0.")
  directed <- infer_directed(n, directed)
  aimed <- if (is_manynet(n)) net_ties(n)
  n <- infer_n(n)
  sizes <- .community_sizes(groups, n)
  parts <- if (length(n) == 2) .split_sizes(sizes, n[1])
  if (!is.null(aimed)) {
    # `width` ties join each pair of islands, so the ties aimed at within the
    # islands are those that are left, out of all those there could be there
    possible <- if (length(n) == 2) sum(parts * (sizes - parts)) else
      sum(choose(sizes, 2)) * ifelse(directed, 2, 1)
    p <- (aimed - choose(length(sizes), 2) * width) / possible
    p <- min(max(p, 0), 1, na.rm = TRUE)
  } 
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1)
    snet_abort("`p` must be a single number from 0 to 1.")
  if (length(n) == 2) {
    out <- .islands_twomode(n, parts, sizes - parts, p, width)
  } else {
    first <- cumsum(c(0, sizes))
    within <- lapply(seq_along(sizes), function(k)
      igraph::as_edgelist(igraph::sample_gnp(sizes[k], p, directed = directed),
                          names = FALSE) + first[k])
    between <- list()
    for (i in seq_along(sizes)[-1]) for (j in seq_len(i - 1)) {
      # `width` different pairs of a node from each island
      pairs <- sample.int(sizes[i] * sizes[j], min(width, sizes[i] * sizes[j]))
      ties <- cbind((pairs - 1) %% sizes[i] + 1 + first[i],
                    (pairs - 1) %/% sizes[i] + 1 + first[j])
      # a bridge between directed islands leads one way or the other
      turned <- directed & stats::runif(nrow(ties)) < 0.5
      ties[turned, ] <- ties[turned, 2:1]
      between[[length(between) + 1]] <- ties
    }
    out <- igraph::make_empty_graph(n, directed = directed)
    out <- igraph::add_edges(out, t(rbind(do.call(rbind, within),
                                          do.call(rbind, between))))
    out <- igraph::set_vertex_attr(out, "community", 
                                   value = rep(seq_along(sizes), sizes))
  }
  as_stocnet(out) |> 
    add_info(name = "Islands network")
}

#' @rdname make_groups
#' @param max_degree The maximum degree of the nodes.
#'   By default three times `degree`, or `n - 1` if that is smaller.
#'   In a two-mode network, this can be one number for each mode,
#'   and is by default three times the mean degree of the mode,
#'   or the number of nodes in the other mode if that is smaller.
#' @param degree_exp The exponent of the power law from which the degrees
#'   are drawn.
#'   By default 2.
#'   In a two-mode or directed network, this can be one number for each
#'   mode, or for the ties sent and the ties received.
#' @param community_exp The exponent of the power law from which the community
#'   sizes are drawn.
#'   By default 1.
#' @details
#'   `generate_communities()` follows the benchmark of Lancichinetti,
#'   Fortunato, and Radicchi (2008).
#'   Degrees and community sizes are each drawn from a power law,
#'   so that there are a few large hubs and a few large communities,
#'   as in many observed networks.
#'   Each node then keeps a share `1 - p` of its ties within its
#'   community and sends the remaining share to nodes in other communities.
#'   Ties within and between communities are drawn at random from all the
#'   simple networks with those degrees.
#'   Where a degree sequence cannot be realised as a simple network,
#'   or a node has more ties to keep than its community has other members,
#'   ties are dropped or sent elsewhere, so the realised degrees and mixing
#'   can differ a little from those asked for.
#'   They differ most in small networks and where there are few communities.
#'
#'   The benchmark is defined for one-mode networks.
#'   For a two-mode network, `generate_communities()` extends it by analogy,
#'   in the same way that `generate_islands()` does for islands.
#'   Each community has nodes of both modes,
#'   in the proportion of the two modes in the network as a whole,
#'   and each mode draws its degrees from a power law of its own.
#'   A node then keeps a share `1 - p` of its ties for the nodes of the
#'   other mode in its community.
#'   The two modes must keep the same number of ties in each community,
#'   and the ties that one mode has too many of there are dropped,
#'   so the realised degrees are a little lower than those asked for where
#'   `p` is low.
#'
#'   A directed network is treated in the same way,
#'   with the ties that a node sends and the ties that it receives in place
#'   of the two modes.
#'   Each node draws both from a power law,
#'   and keeps a share `1 - p` of each within its one community.
#'   Lancichinetti and Fortunato (2009) define a directed benchmark too,
#'   from which this differs in how the two are drawn and reconciled,
#'   so it should not be taken for theirs.
#'
#'   `generate_islands()` is a simpler model of the same kind:
#'   the ties in each island are random, rather than following from degrees
#'   that differ, and each pair of islands is joined by a fixed number of
#'   bridges, rather than by a share of each node's ties.
#' @references
#' ## On communities
#' Lancichinetti, Andrea, Santo Fortunato, and Filippo Radicchi. 2008.
#' “Benchmark Graphs for Testing Community Detection Algorithms.”
#' _Physical Review E_ 78(4):046110.
#' \doi{10.1103/PhysRevE.78.046110}
#' 
#' Lancichinetti, Andrea, and Santo Fortunato. 2009.
#' “Benchmarks for Testing Community Detection Algorithms on Directed and
#' Weighted Graphs with Overlapping Communities.”
#' _Physical Review E_ 80(1):016118.
#' \doi{10.1103/PhysRevE.80.016118}
#' @importFrom igraph sample_degseq is_graphical
#' @examples
#' generate_communities(50)
#' generate_communities(50, p = 0.4)
#' generate_communities(50, directed = TRUE, groups = 3)
#' generate_communities(c(60, 40))
#' @export
generate_communities <- function(n, p = 0.1, directed = FALSE, groups = NULL,
                                 degree = NULL, max_degree = NULL,
                                 degree_exp = 2, community_exp = 1) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0 || p > 1)
    snet_abort("`p` must be a single number from 0 to 1.")
  if (!is.null(degree) && 
      (!is.numeric(degree) || length(degree) != 1 || is.na(degree) ||
       degree < 1))
    snet_abort("`degree` must be a single number of at least 1.")
  if (length(n) == 2 || directed) 
    return(.communities_sided(n, p, groups, degree, max_degree,
                              degree_exp, community_exp))
  if (n < 3) snet_abort("At least 3 nodes required to form communities.")
  if (is.null(degree)) degree <- min(10, max(1, floor((n - 1) / 2)))
  if (is.null(max_degree)) max_degree <- min(n - 1, ceiling(3 * degree))
  if (!is.numeric(max_degree) || length(max_degree) != 1 ||
      is.na(max_degree) || max_degree != round(max_degree) ||
      max_degree < degree || max_degree > n - 1)
    snet_abort(paste("`max_degree` must be a single whole number that is at",
                     "least `degree` and less than the number of nodes."))
  
  degs <- .powerlaw_degrees(n, degree, max_degree, degree_exp)
  # Unless the groups are given, a community is large enough for a node of
  # any degree to find all the ties it keeps within it.
  sizes <- if (is.null(groups)) 
    .powerlaw_sizes(n, min(ceiling(degree) + 1, n), min(max_degree + 1, n), 
                    community_exp) else
      .community_sizes(groups, n, community_exp)
  # The ties a node sends out of its community are rounded up or down at
  # random, so that their share is `p` on average whatever the degree.
  outs <- degs * p
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
  sent <- tabulate(rep(memb, outs), length(sizes))
  over <- which(sent > sum(sent) - sent)
  if (length(over) == 1) {
    kept <- .draw_ties(ifelse(memb == over, outs, 0), 
                       2 * sent[over] - sum(sent))
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
  as_stocnet(out) |> 
    add_info(name = "Communities network")
}

#' @rdname make_groups
#' @details
#'   In `generate_core()`, with a single `p` every pair of nodes in the core
#'   is tied and no pair in the periphery is, as in `create_core()`,
#'   and what is random is which nodes of the periphery each node of the
#'   core is tied to.
#'   With three, the core can be less than complete and the periphery less
#'   than empty, as Borgatti and Everett (2000) describe of observed networks.
#'   `mark` says which nodes are in the core, with `TRUE` for a node of the
#'   core, and by default they are the first half of the nodes, or of each
#'   mode.
#'   Note that `create_core()` for now reads a logical `mark` the other way
#'   round, with `TRUE` for a node of the periphery.
#' @references
#' ## On cores and peripheries
#' Borgatti, Stephen P., and Martin G. Everett. 2000.
#' “Models of Core/Periphery Structures.”
#' _Social Networks_ 21(4):375–95.
#' \doi{10.1016/S0378-8733(99)00019-2}
#' @examples
#' generate_core(12)
#' generate_core(12, p = c(0.8, 0.3, 0.1))
#' @export
generate_core <- function(n, p = 0.5, directed = FALSE, mark = NULL) {
  directed <- infer_directed(n, directed)
  mark <- infer_membership(n, mark)
  n <- infer_n(n)
  if (length(mark) != sum(n))
    snet_abort("`mark` must say for each of the {sum(n)} nodes whether it is in the core.")
  # As in `create_core()`, the core is the first of two groups where the
  # nodes are not marked as in it or out of it. `create_core()` reads a
  # logical mark the other way round until that can change with 'netrics'.
  core <- if (is.logical(mark)) as.logical(mark) else 
    as.numeric(as.factor(mark)) == 1
  if (!is.numeric(p) || !length(p) %in% c(1, 3) || anyNA(p) ||
      any(p < 0) || any(p > 1))
    snet_abort(paste("`p` must be one probability, for a tie between the core",
                     "and the periphery, or three: for a tie within the core,",
                     "between the two, and within the periphery."))
  if (length(p) == 1) p <- c(1, p, 0)
  if (length(n) == 2) {
    rows <- core[seq_len(n[1])]
    cols <- core[-seq_len(n[1])]
    # a tie is within the core, between the two, or within the periphery
    # as two, one, or neither of its nodes are in the core
    prob <- p[3 - outer(rows, cols, "+")]
    mat <- matrix(stats::rbinom(length(prob), 1, prob), n[1], n[2])
    out <- as_igraph(mat, twomode = TRUE)
  } else {
    prob <- p[3 - outer(core, core, "+")]
    mat <- matrix(stats::rbinom(length(prob), 1, prob), n, n)
    diag(mat) <- 0
    if (!directed) {
      mat[lower.tri(mat)] <- 0
      mat <- mat + t(mat)
    }
    out <- igraph::graph_from_adjacency_matrix(mat, ifelse(directed, "directed",
                                                           "undirected"))
  }
  out <- igraph::set_vertex_attr(out, "core", value = core)
  as_stocnet(out) |> 
    add_info(name = "Core-periphery network")
}

# Reads the arguments given by a name they had in an earlier version, so
# that a call written for that version still works. `former` names the
# argument each former name now goes by, and the values are returned under
# those names.
.former_args <- function(dots, former) {
  if (!length(dots)) return(list())
  unused <- if (is.null(names(dots))) rep("an unnamed argument", length(dots)) else
    setdiff(names(dots), names(former))
  if (length(unused)) snet_abort("Unused argument{?s}: {unused}.")
  stats::setNames(dots, former[names(dots)])
}

# Reads `groups` as the size of each group among the nodes of the network.
# One number is how many groups there are, which are then as equal in size as
# they can be. Numbers that sum to the nodes of the network are the sizes 
# themselves. Two other numbers are the smallest and the largest a group can
# be, between which the sizes are drawn. This is how `{netrics}` reads
# `groups` too, but for the last.
.community_sizes <- function(groups, n, exponent = 0) {
  total <- sum(n)
  if (!is.numeric(groups) || !length(groups) || anyNA(groups) ||
      any(groups != round(groups)) || any(groups < 1))
    snet_abort(paste("`groups` must be how many groups there are, the smallest",
                     "and the largest a group can be, or the size of each",
                     "group, in whole numbers of at least 1."))
  if (length(groups) == 1) {
    if (groups > min(n))
      snet_abort(paste("There cannot be more groups than there are nodes",
                       "in the network, or in each of its modes."))
    sizes <- total %/% groups + (seq_len(groups) <= total %% groups)
  } else if (sum(groups) == total) {
    sizes <- groups
  } else if (length(groups) == 2 && groups[1] <= groups[2]) {
    sizes <- .powerlaw_sizes(total, min(groups[1], total), min(groups[2], total),
                             exponent)
  } else snet_abort(paste("`groups` must sum to the {total} nodes in the",
                          "network where it gives the size of each group."))
  if (length(n) == 2 && (length(sizes) > min(n) || any(sizes < 2)))
    snet_abort(paste("Each group needs a node of each mode, so there cannot",
                     "be more groups than nodes in the smaller mode."))
  sizes
}

# Draws `n` whole numbers from `lo` to `hi` with probability proportional to
# the number raised to the power of minus `exponent`.
.rpowerlaw <- function(n, lo, hi, exponent) {
  ks <- seq.int(lo, hi)
  ks[sample.int(length(ks), n, replace = TRUE, prob = ks^-exponent)]
}

# Draws `k` of the ties that the nodes have between them at random,
# and returns how many of each node's ties were drawn.
.draw_ties <- function(degs, k) {
  ties <- rep(seq_along(degs), degs)
  tabulate(ties[sample.int(length(ties), k)], length(degs))
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
  snet_abort(paste("Could not divide", n, "nodes into groups of", lo, 
                   "to", hi, "nodes. Please widen `groups`."))
}

# Assigns each node to a community that has more members than the node has
# ties to keep inside it. Nodes with the most such ties choose first, since
# they have the fewest communities to choose from, and each chooses among the
# communities with room in proportion to that room.
# A node that no community with room is large enough for joins the largest one
# with room, and the ties it cannot keep there are sent out instead.
# `limit` is the most ties a node can keep in each community: all its other 
# members in a one-mode network, and its members of the other mode in a 
# two-mode network, where `sizes` counts only those of the node's own mode.
.assign_communities <- function(ins, sizes, limit = sizes - 1) {
  room <- sizes
  memb <- integer(length(ins))
  for (i in order(ins, decreasing = TRUE)) {
    fits <- which(room > 0 & limit >= ins[i])
    if (length(fits) == 0) {
      fits <- which(room > 0)
      fits <- fits[limit[fits] == max(limit[fits])]
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
# With `indegs`, the network has two sides: `degs` are the degrees of the
# first and `indegs` those of the second, and each tie is returned as a node
# of the first side and a node of the second, each numbered within its side.
# The sides are the two modes of a two-mode network, or with `same`, the same
# nodes as the senders and the receivers of the ties of a directed network.
.sample_simple <- function(degs, indegs = NULL, same = FALSE) {
  if (!is.null(indegs)) return(.sample_simple_sided(degs, indegs, same))
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

.sample_simple_sided <- function(degs, indegs, same = FALSE) {
  # the sides have the same ties, so the side with more loses the difference
  # from its highest degrees
  while (sum(degs) != sum(indegs)) {
    if (sum(degs) > sum(indegs)) {
      top <- which.max(degs)
      degs[top] <- degs[top] - 1
    } else {
      top <- which.max(indegs)
      indegs[top] <- indegs[top] - 1
    }
  }
  # In a two-mode network, every tie runs from a node with only ties out to
  # a node with only ties in, and so from the first mode to the second.
  n1 <- ifelse(same, 0, length(degs))
  outs <- if (same) degs else c(degs, rep(0, length(indegs)))
  ins <- if (same) indegs else c(rep(0, n1), indegs)
  while (!igraph::is_graphical(outs, ins)) {
    outs[which.max(outs)] <- max(outs) - 1
    ins[which.max(ins)] <- max(ins) - 1
  }
  if (sum(outs) == 0) return(matrix(integer(0), 0, 2))
  # Switching ties is slower
  # than matching them at random ("fast.heur.simple"), but that starts again
  # whenever it cannot place a tie, and does not end where ties are dense.
  el <- igraph::as_edgelist(igraph::sample_degseq(outs, ins,
                                                  method = "edge.switching.simple"),
                            names = FALSE)
  cbind(el[, 1], el[, 2] - n1)
}

# Moves ties that fell within a community to between communities.
# Each such tie swaps an end with another tie chosen at random,
# where neither new tie is within a community or already there.
# The swap keeps every degree. Ties still within a community after many
# passes are returned as they are, since there are then too few ties
# elsewhere to swap with, as where one community sends out more ties than
# all the others together.
# With `sided`, the first node of each tie is of the first mode, or is its
# sender, so the ties swap only their second nodes. Each then still joins the
# two modes, or leads the way it did, and a tie is not the same tie as the
# one that leads back.
.separate_ties <- function(el, memb, sided = FALSE) {
  n <- length(memb)
  key <- function(a, b) if (sided) a * (n + 1) + b else
    pmin(a, b) * (n + 1) + pmax(a, b)
  keys <- key(el[, 1], el[, 2])
  inside <- which(memb[el[, 1]] == memb[el[, 2]])
  passes <- 0
  while (length(inside) > 0 && passes < 50) {
    for (i in inside) {
      j <- sample.int(nrow(el), 1)
      a <- el[i, 1]
      b <- el[i, 2]
      other <- if (sided || stats::runif(1) < 0.5) el[j, 1:2] else el[j, 2:1]
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
                    "Consider a lower `p` or smaller communities."))
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
# diagonal. `part1` and `part2` are how many nodes of each mode each island
# has. A node of the first mode and a node of the second mode that share an
# island are tied with probability `p`. Each pair of islands is then joined
# by `width` further ties.
.islands_twomode <- function(n, part1, part2, p, width) {
  b1 <- rep(seq_along(part1), part1)
  b2 <- rep(seq_along(part2), part2)
  g <- matrix(0, n[1], n[2])
  same <- outer(b1, b2, "==")
  g[same] <- stats::rbinom(sum(same), 1, p)
  if (width > 0 && length(part1) > 1) {
    for (i in seq_len(length(part1) - 1)) for (j in seq(i + 1, length(part1))) {
      cells <- which(outer(b1 == i, b2 == j, "&") |
                       outer(b1 == j, b2 == i, "&"))
      if (length(cells) == 0) next
      g[cells[sample.int(length(cells), min(width, length(cells)))]] <- 1
    }
  }
  igraph::set_vertex_attr(as_igraph(g, twomode = TRUE), "community",
                          value = c(b1, b2))
}

# A communities model with two sides, for two-mode and for directed networks.
# The benchmark this extends is defined for undirected one-mode networks.
# In a two-mode network the sides are the two modes, and a community has
# nodes of both, as an island does in `.islands_twomode()`. In a directed
# network the sides are the same nodes as the senders of ties and as their
# receivers. A tie within a community then joins a node of each side.
# Three things change from the one-sided model:
# - each side draws its own degrees, and since both sides have the same ties,
#   the mean degree of the second side follows from that of the first;
# - a node can keep no more ties in its community than the community has
#   nodes of the other side;
# - the ties that the two sides keep in a community must be the same in 
#   number. A two-mode community holds the modes in the proportion that the
#   network does, and a directed one holds the same nodes on both sides, so 
#   they are the same in expectation, and only the difference that chance
#   leaves needs to be dealt with.
.communities_sided <- function(n, p, groups, degree, max_degree,
                               degree_exp, community_exp) {
  twomode <- length(n) == 2
  if (!twomode && n < 3) 
    snet_abort("At least 3 nodes required to form communities.")
  sides <- if (twomode) n else c(n, n)
  # how many nodes a node of each side could tie to
  other <- if (twomode) rev(n) else c(n - 1, n - 1)
  if (is.null(degree)) degree <- min(10, max(1, floor(other[1] / 2)))
  if (degree > other[1])
    snet_abort(paste("`degree` cannot be more than the number of nodes",
                     ifelse(twomode, "in the second mode.",
                            "that a node can tie to.")))
  degree <- c(degree, degree * sides[1] / sides[2])
  if (is.null(max_degree)) max_degree <- pmin(other, ceiling(3 * degree))
  if (!is.numeric(max_degree) || !length(max_degree) %in% 1:2 ||
      anyNA(max_degree) || any(max_degree != round(max_degree)))
    snet_abort(paste("`max_degree` must be a whole number, or a whole number",
                     "for each mode."))
  max_degree <- rep_len(max_degree, 2)
  if (any(max_degree < degree) || any(max_degree > other))
    snet_abort(paste("`max_degree` must be at least the mean degree",
                     "({round(degree, 1)}) and no more than the number",
                     "of nodes that a node can tie to ({other})."))
  degree_exp <- rep_len(degree_exp, 2)
  
  if (twomode) {
    # There can be no more communities than there are nodes in the smaller
    # mode, since each has nodes of both.
    fewest <- ceiling(sum(n) / min(n))
    if (length(groups) == 2 && sum(groups) != sum(n) && groups[1] < fewest)
      snet_abort(paste("The minimum of `groups` must be at least {fewest},",
                       "so that each community has nodes of both modes."))
  }
  # Unless the groups are given, a community is large enough for its nodes
  # of each side to keep their ties there.
  sizes <- if (!is.null(groups)) .community_sizes(groups, n, community_exp) else 
    if (twomode) {
      least <- max(ceiling(degree[1] * sum(n) / n[2]), fewest, 2)
      most <- max(least, ceiling(max(max_degree * sum(n) / other)))
      .powerlaw_sizes(sum(n), min(least, sum(n)), min(most, sum(n)), 
                      community_exp)
    } else .powerlaw_sizes(n, min(ceiling(degree[1]) + 1, n), 
                           min(max(max_degree) + 1, n), community_exp)
  # how many nodes of each side each community has
  parts <- if (twomode) list(.split_sizes(sizes, n[1])) else list(sizes, sizes)
  if (twomode) parts[[2]] <- sizes - parts[[1]]
  # how many ties a node of each side can keep in each community, and send
  # out of it
  inside <- if (twomode) rev(parts) else list(sizes - 1, sizes - 1)
  outside <- lapply(1:2, function(m) other[m] - inside[[m]])
  
  degs <- lapply(1:2, function(m) 
    .powerlaw_degrees(sides[m], degree[m], max_degree[m], degree_exp[m]))
  # both sides have the same ties, so the side with more loses some at random
  more <- which.max(vapply(degs, sum, numeric(1)))
  degs[[more]] <- degs[[more]] - 
    .draw_ties(degs[[more]], sum(degs[[more]]) - sum(degs[[3 - more]]))
  outs <- lapply(degs, function(d) {
    out <- d * p
    floor(out) + (stats::runif(length(d)) < out - floor(out))
  })
  memb <- if (twomode) lapply(1:2, function(m) 
    .assign_communities(degs[[m]] - outs[[m]], parts[[m]], 
                        limit = inside[[m]])) else 
      # a node is in one community, whichever side of a tie it is on
      rep(list(.assign_communities(pmax(degs[[1]] - outs[[1]], 
                                        degs[[2]] - outs[[2]]), sizes)), 2)
  ins <- vector("list", 2)
  for (m in 1:2) {
    # nodes are listed community by community
    degs[[m]] <- degs[[m]][order(memb[[m]])]
    outs[[m]] <- outs[[m]][order(memb[[m]])]
    memb[[m]] <- sort(memb[[m]])
    outs[[m]] <- pmin(outs[[m]], outside[[m]][memb[[m]]])
    ins[[m]] <- pmin(degs[[m]] - outs[[m]], inside[[m]][memb[[m]]])
    outs[[m]] <- pmin(degs[[m]] - ins[[m]], outside[[m]][memb[[m]]])
  }
  # The two sides must keep the same number of ties in a community.
  # The side that keeps fewer there keeps some of the ties it sent out, and
  # the side that keeps more sends as many out, so that neither the degrees
  # nor the mixing change. Where the first has too few ties sent out for
  # that, the second drops the ties that are left over.
  for (k in seq_along(sizes)) {
    members <- list(which(memb[[1]] == k), which(memb[[2]] == k))
    kept <- c(sum(ins[[1]][members[[1]]]), sum(ins[[2]][members[[2]]]))
    m <- which.max(kept)
    less <- members[[3 - m]]
    more <- members[[m]]
    room <- pmin(outs[[3 - m]][less], inside[[3 - m]][k] - ins[[3 - m]][less])
    moved <- .draw_ties(room, min((kept[m] - kept[3 - m]) %/% 2, sum(room)))
    ins[[3 - m]][less] <- ins[[3 - m]][less] + moved
    outs[[3 - m]][less] <- outs[[3 - m]][less] - moved
    lost <- .draw_ties(ins[[m]][more], kept[m] - kept[3 - m] - sum(moved))
    ins[[m]][more] <- ins[[m]][more] - lost
    room <- pmin(lost, outside[[m]][k] - outs[[m]][more])
    sent <- .draw_ties(room, min(sum(moved), sum(room)))
    outs[[m]][more] <- outs[[m]][more] + sent
  }
  # One side of a community cannot send out more ties than the other side
  # of all the other communities sends out to meet them, which is most
  # likely where there are only a few communities. 
  # The ties it sends out beyond that are dropped.
  for (m in 1:2) {
    sent <- lapply(1:2, function(m) tabulate(rep(memb[[m]], outs[[m]]), 
                                             length(sizes)))
    for (k in which(sent[[m]] > sum(sent[[3 - m]]) - sent[[3 - m]]))
      outs[[m]] <- outs[[m]] - 
        .draw_ties(ifelse(memb[[m]] == k, outs[[m]], 0),
                   sent[[m]][k] - sum(sent[[3 - m]]) + sent[[3 - m]][k])
  }
  # the ties dropped leave one side with more ties to send out than the other
  m <- which.max(vapply(outs, sum, numeric(1)))
  outs[[m]] <- outs[[m]] - 
    .draw_ties(outs[[m]], sum(outs[[m]]) - sum(outs[[3 - m]]))
  
  within <- lapply(seq_along(sizes), function(k) {
    members <- list(which(memb[[1]] == k), which(memb[[2]] == k))
    el <- .sample_simple(ins[[1]][members[[1]]], ins[[2]][members[[2]]],
                         same = !twomode)
    cbind(members[[1]][el[, 1]], members[[2]][el[, 2]])
  })
  between <- .sample_simple(outs[[1]], outs[[2]], same = !twomode)
  ties <- rbind(do.call(rbind, within), between)
  # the second mode is numbered after the first, as in the network
  if (twomode) ties[, 2] <- ties[, 2] + n[1]
  placed <- seq_len(nrow(ties)) > nrow(ties) - nrow(between)
  ties[placed, ] <- .separate_ties(ties[placed, , drop = FALSE], 
                                   if (twomode) unlist(memb) else memb[[1]], 
                                   sided = TRUE)
  out <- if (twomode) 
    igraph::make_bipartite_graph(rep(c(FALSE, TRUE), n), c(t(ties))) else
      igraph::add_edges(igraph::make_empty_graph(n, directed = TRUE), t(ties))
  # a tie that could not be moved out of a community may repeat one within it
  out <- igraph::simplify(out)
  out <- igraph::set_vertex_attr(out, "community", 
                                 value = if (twomode) unlist(memb) else memb[[1]])
  as_stocnet(out) |> 
    add_info(name = "Communities network")
}

# Divides each community between the two modes in the proportion that the
# network holds them, with at least one node of each mode in each community.
# `n1` is the number of nodes in the first mode, and the number of each
# community's nodes that are of the first mode is returned.
# The nodes that rounding leaves over go where it took the most.
.split_sizes <- function(sizes, n1) {
  share <- sizes * n1 / sum(sizes)
  part <- pmin(pmax(floor(share), 1), sizes - 1)
  while ((left <- n1 - sum(part)) != 0) {
    free <- if (left > 0) which(part < sizes - 1) else which(part > 1)
    pick <- free[which.max(sign(left) * (share - part)[free])]
    part[pick] <- part[pick] + sign(left)
  }
  part
}

# A two-mode citation model.
# `igraph::sample_last_cit()` is a recency model: a new node cites old nodes
# with a probability that depends on how long ago each was last cited.
# Here the mechanism is kept but the target crosses the mode divide.
# A new node of the first mode ties to `ties` nodes of the second mode,
# each chosen by how recently that node was last tied to, or with
# probability `1 - p`, the nodes that came most recently.
# Because both modes grow, new second-mode nodes keep entering in the freshest
# bin, so the concentration of ties turns over instead of locking in.
.citations_twomode <- function(n, ties, agebins, p = 1) {
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
    chosen <- .cite(cols, min(ties, length(cols)), p, prob)
    g[act[1], chosen] <- 1
    last_used[chosen] <- k
  }
  as_igraph(g, twomode = TRUE)
}
