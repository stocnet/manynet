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
#'   
#'   `generate_utilities()` returns a `stocnet` object instead,
#'   which is always directed where it is one-mode,
#'   since what one node finds in another need not be what the other finds
#'   in it.
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
#' @param form How the utilities are distributed between -1 and 1.
#'   - "normal" (the default) draws them from a normal distribution around 0,
#'   so that most nodes are close to indifferent about most others
#'   and only a few are much liked or much disliked.
#'   - "uniform" draws every utility between -1 and 1 as likely as any other.
#'   - "relative" draws them from a normal distribution and then divides each 
#'   node's utilities by the largest of them, whether liked or disliked,
#'   so that each node's utilities are relative to the node it feels most
#'   strongly about.
#' @param threshold How far a utility must be from 0 to be a tie, from 0 to 1.
#'   A utility above `threshold` is a positive tie,
#'   a utility below `-threshold` is a negative tie,
#'   and a node is indifferent to those between the two, which are not ties.
#'   By default 0, so that every utility is a tie.
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
#'   Where there is more than one step, 
#'   `threshold` applies to the utilities at each moment,
#'   so a tie can come and go as a utility crosses it.
#'   A utility cannot rise above 1 or fall below -1, 
#'   and one that a change would take further stays at that limit
#'   until a change brings it back.
#'   Over many steps the utilities therefore spread out towards the limits,
#'   and more of them are far enough from 0 to be ties than at the start.
#' @examples
#' (utils <- generate_utilities(6))
#' generate_utilities(6, threshold = 0.25)
#' generate_utilities(c(4, 6), form = "uniform")
#' generate_utilities(6, steps = 3)
#' generate_utilities(6, steps = 3, volatility = 0.5, inertia = 0.8)
#' # the ties that one node wants
#' to_unsigned(utils, keep = "positive")
#' # the ties that both nodes want
#' to_unsigned(to_undirected(utils, rule = "min"), keep = "positive")
#' @export
generate_utilities <- function(n, form = c("normal", "uniform", "relative"),
                               threshold = 0, steps = 1, volatility = 0.1,
                               inertia = 0){
  form <- match.arg(form)
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) ||
      threshold < 0 || threshold > 1)
    snet_abort("`threshold` must be a single number from 0 to 1.")
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
  n <- infer_n(n)
  twomode <- length(n) == 2
  dims <- if (twomode) n else c(n, n)
  if (!is.null(labels))
    labels <- if (twomode) list(labels[seq_len(n[1])], labels[-seq_len(n[1])]) else
      list(labels, labels)
  
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
    pmax(pmin(out, 1), -1)
  }
  utilities <- draw()
  # the utilities a node is indifferent to are not ties
  as_ties <- function(utilities) {
    utilities[abs(utilities) <= threshold] <- 0
    as_stocnet(utilities, twomode = twomode)
  }
  if (steps == 1) 
    return(as_ties(utilities) |> add_info(name = "Utilities network"))
  
  waves <- vector("list", steps)
  waves[[1]] <- as_ties(utilities)
  for (step in seq_len(steps)[-1]) {
    # the utilities change, and not only those far enough from 0 to be ties
    change <- draw() * volatility
    # the smallest changes are those that inertia holds back
    pairs <- if (twomode) rep(TRUE, prod(dims)) else c(row(change) != col(change))
    held <- rank(abs(change[pairs]), ties.method = "first") <= 
      round(inertia * sum(pairs))
    change[pairs][held] <- 0
    utilities <- pmax(pmin(utilities + change, 1), -1)
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
#'   These functions can create either one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#'   `generate_communities()` creates only undirected networks.
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
#'   In a two-mode network, this is the mean degree of the nodes of the
#'   first mode.
#'   The mean degree of the second mode follows from it,
#'   since both modes have the same ties: it is `degree * n[1] / n[2]`.
#' @param max_degree The maximum degree of the nodes.
#'   By default three times `degree`, or `n - 1` if that is smaller.
#'   In a two-mode network, this can be one number for each mode,
#'   and is by default three times the mean degree of the mode,
#'   or the number of nodes in the other mode if that is smaller.
#' @param mixing The share of each node's ties that go to nodes in other
#'   communities, from 0 to 1.
#'   By default 0.1.
#'   A low share gives communities that are easy to tell apart.
#'   A share above 0.5 gives communities with more ties out than in.
#' @param community A vector of the minimum and the maximum community size.
#'   By default `c(degree + 1, max_degree + 1)`,
#'   so that a node of any degree can find all the ties it keeps within its
#'   community.
#'   In a two-mode network, a community has nodes of both modes,
#'   and these are the sizes of the two modes together.
#' @param degree_exp The exponent of the power law from which the degrees
#'   are drawn.
#'   By default 2.
#'   In a two-mode network, this can be one number for each mode.
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
#'   The benchmark is defined for one-mode networks.
#'   For a two-mode network, `generate_communities()` extends it by analogy,
#'   in the same way that `generate_islands()` does for islands.
#'   Each community has nodes of both modes,
#'   in the proportion of the two modes in the network as a whole,
#'   and each mode draws its degrees from a power law of its own.
#'   A node then keeps a share `1 - mixing` of its ties for the nodes of the
#'   other mode in its community.
#'   The two modes must keep the same number of ties in each community,
#'   and the ties that one mode has too many of there are dropped,
#'   so the realised degrees are a little lower than those asked for where
#'   `mixing` is low.
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
#' generate_communities(c(60, 40))
#' @export
generate_communities <- function(n, degree = NULL, max_degree = NULL,
                                 mixing = 0.1, community = NULL,
                                 degree_exp = 2, community_exp = 1) {
  n <- infer_n(n)
  if (!is.numeric(mixing) || length(mixing) != 1 || is.na(mixing) ||
      mixing < 0 || mixing > 1)
    snet_abort("`mixing` must be a single number from 0 to 1.")
  if (!is.null(community) && 
      (!is.numeric(community) || length(community) != 2 || anyNA(community) ||
       any(community != round(community)) || community[1] < 2 ||
       community[1] > community[2]))
    snet_abort(paste("`community` must be a vector of two whole numbers,",
                     "the minimum (at least 2) and the maximum community size."))
  if (!is.null(degree) && 
      (!is.numeric(degree) || length(degree) != 1 || is.na(degree) ||
       degree < 1))
    snet_abort("`degree` must be a single number of at least 1.")
  if (length(n) == 2) 
    return(.communities_twomode(n, degree, max_degree, mixing, community,
                                degree_exp, community_exp))
  if (n < 3) snet_abort("At least 3 nodes required to form communities.")
  if (is.null(degree)) degree <- min(10, max(1, floor((n - 1) / 2)))
  if (is.null(max_degree)) max_degree <- min(n - 1, ceiling(3 * degree))
  if (!is.numeric(max_degree) || length(max_degree) != 1 ||
      is.na(max_degree) || max_degree != round(max_degree) ||
      max_degree < degree || max_degree > n - 1)
    snet_abort(paste("`max_degree` must be a single whole number that is at",
                     "least `degree` and less than the number of nodes."))
  if (is.null(community))
    community <- c(ceiling(degree) + 1, max_degree + 1)
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
  as_tidygraph(out) |> 
    add_info(name = "Communities network")
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
  snet_abort(paste("Could not divide", n, "nodes into communities of", lo, 
                   "to", hi, "nodes. Please widen `community`."))
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
# With `indegs`, the network is two-mode: `degs` are the degrees of the first
# mode and `indegs` those of the second, and each tie is returned as a node
# of the first mode and a node of the second, each numbered within its mode.
.sample_simple <- function(degs, indegs = NULL) {
  if (!is.null(indegs)) return(.sample_simple_twomode(degs, indegs))
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

.sample_simple_twomode <- function(degs, indegs) {
  # the modes have the same ties, so the mode with more loses the difference
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
  n1 <- length(degs)
  outs <- c(degs, rep(0, length(indegs)))
  ins <- c(rep(0, n1), indegs)
  while (!igraph::is_graphical(outs, ins)) {
    outs[which.max(outs)] <- max(outs) - 1
    ins[which.max(ins)] <- max(ins) - 1
  }
  if (sum(outs) == 0) return(matrix(integer(0), 0, 2))
  # Every tie runs from a node with only ties out to a node with only ties
  # in, and so from the first mode to the second. Switching ties is slower
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
# In a two-mode network the first node of each tie is of the first mode,
# so the ties swap only their second nodes and each still joins the two modes.
.separate_ties <- function(el, memb, twomode = FALSE) {
  n <- length(memb)
  key <- function(a, b) pmin(a, b) * (n + 1) + pmax(a, b)
  keys <- key(el[, 1], el[, 2])
  inside <- which(memb[el[, 1]] == memb[el[, 2]])
  passes <- 0
  while (length(inside) > 0 && passes < 50) {
    for (i in inside) {
      j <- sample.int(nrow(el), 1)
      a <- el[i, 1]
      b <- el[i, 2]
      other <- if (twomode || stats::runif(1) < 0.5) el[j, 1:2] else el[j, 2:1]
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

# A two-mode communities model.
# The benchmark this extends is defined for one-mode networks.
# Here a community has nodes of both modes, as an island does in
# `.islands_twomode()`, and a tie within a community joins a node of each
# mode. Three things then change from the one-mode model:
# - each mode draws its own degrees, and since both modes have the same ties,
#   the mean degree of the second mode follows from that of the first;
# - a node can keep no more ties in its community than the community has
#   nodes of the other mode;
# - the ties that the two modes keep in a community must be the same in 
#   number. Each community holds the modes in the proportion that the network
#   does, so they are the same in expectation, and only the difference that
#   chance leaves is sent out of the community instead.
.communities_twomode <- function(n, degree, max_degree, mixing, community,
                                 degree_exp, community_exp) {
  other <- rev(n)
  if (is.null(degree)) degree <- min(10, max(1, floor(n[2] / 2)))
  if (degree > n[2])
    snet_abort(paste("`degree` cannot be more than the number of nodes in",
                     "the second mode."))
  degree <- c(degree, degree * n[1] / n[2])
  if (is.null(max_degree)) max_degree <- pmin(other, ceiling(3 * degree))
  if (!is.numeric(max_degree) || !length(max_degree) %in% 1:2 ||
      anyNA(max_degree) || any(max_degree != round(max_degree)))
    snet_abort(paste("`max_degree` must be a whole number, or a whole number",
                     "for each mode."))
  max_degree <- rep_len(max_degree, 2)
  if (any(max_degree < degree) || any(max_degree > other))
    snet_abort(paste("`max_degree` must be at least the mean degree of each",
                     "mode ({round(degree, 1)}) and no more than the number",
                     "of nodes in the other mode ({other})."))
  degree_exp <- rep_len(degree_exp, 2)
  # A community must be large enough for its nodes of each mode to keep
  # their ties there, and there can be no more communities than there are
  # nodes in the smaller mode, since each has nodes of both.
  fewest <- ceiling(sum(n) / min(n))
  if (is.null(community)) {
    community <- c(max(ceiling(degree[1] * sum(n) / n[2]), fewest, 2),
                   ceiling(max(max_degree * sum(n) / other)))
    community[2] <- max(community)
  }
  if (community[1] < fewest)
    snet_abort(paste("The minimum of `community` must be at least {fewest},",
                     "so that each community has nodes of both modes."))
  community <- pmin(community, sum(n))
  
  degs <- lapply(1:2, function(m) 
    .powerlaw_degrees(n[m], degree[m], max_degree[m], degree_exp[m]))
  # both modes have the same ties, so the mode with more loses some at random
  more <- which.max(vapply(degs, sum, numeric(1)))
  degs[[more]] <- degs[[more]] - 
    .draw_ties(degs[[more]], sum(degs[[more]]) - sum(degs[[3 - more]]))
  sizes <- .powerlaw_sizes(sum(n), community[1], community[2], community_exp)
  parts <- .split_sizes(sizes, n[1])
  parts <- list(parts, sizes - parts)
  
  outs <- lapply(degs, function(d) {
    out <- d * mixing
    floor(out) + (stats::runif(length(d)) < out - floor(out))
  })
  memb <- lapply(1:2, function(m) 
    .assign_communities(degs[[m]] - outs[[m]], parts[[m]], 
                        limit = parts[[3 - m]]))
  ins <- vector("list", 2)
  for (m in 1:2) {
    # nodes are listed community by community within each mode
    degs[[m]] <- degs[[m]][order(memb[[m]])]
    outs[[m]] <- outs[[m]][order(memb[[m]])]
    memb[[m]] <- sort(memb[[m]])
    inside <- parts[[3 - m]][memb[[m]]]
    outs[[m]] <- pmin(outs[[m]], other[m] - inside)
    ins[[m]] <- pmin(degs[[m]] - outs[[m]], inside)
    outs[[m]] <- pmin(degs[[m]] - ins[[m]], other[m] - inside)
  }
  # The two modes must keep the same number of ties in a community.
  # The mode that keeps fewer there keeps some of the ties it sent out, and
  # the mode that keeps more sends as many out, so that neither the degrees
  # nor the mixing change. Where the first has too few ties sent out for
  # that, the second drops the ties that are left over.
  for (k in seq_along(sizes)) {
    members <- list(which(memb[[1]] == k), which(memb[[2]] == k))
    kept <- c(sum(ins[[1]][members[[1]]]), sum(ins[[2]][members[[2]]]))
    m <- which.max(kept)
    less <- members[[3 - m]]
    more <- members[[m]]
    room <- pmin(outs[[3 - m]][less], parts[[m]][k] - ins[[3 - m]][less])
    moved <- .draw_ties(room, min((kept[m] - kept[3 - m]) %/% 2, sum(room)))
    ins[[3 - m]][less] <- ins[[3 - m]][less] + moved
    outs[[3 - m]][less] <- outs[[3 - m]][less] - moved
    lost <- .draw_ties(ins[[m]][more], kept[m] - kept[3 - m] - sum(moved))
    ins[[m]][more] <- ins[[m]][more] - lost
    room <- other[m] - parts[[3 - m]][k] - outs[[m]][more]
    sent <- .draw_ties(pmin(lost, room), min(sum(moved), sum(pmin(lost, room))))
    outs[[m]][more] <- outs[[m]][more] + sent
  }
  # One mode of a community cannot send out more ties than the other mode
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
  # the ties dropped leave one mode with more ties to send out than the other
  m <- which.max(vapply(outs, sum, numeric(1)))
  outs[[m]] <- outs[[m]] - 
    .draw_ties(outs[[m]], sum(outs[[m]]) - sum(outs[[3 - m]]))
  
  within <- lapply(seq_along(sizes), function(k) {
    members <- list(which(memb[[1]] == k), which(memb[[2]] == k))
    el <- .sample_simple(ins[[1]][members[[1]]], ins[[2]][members[[2]]])
    cbind(members[[1]][el[, 1]], members[[2]][el[, 2]])
  })
  between <- .sample_simple(outs[[1]], outs[[2]])
  ties <- rbind(do.call(rbind, within), between)
  # the second mode is numbered after the first, as in the network
  ties[, 2] <- ties[, 2] + n[1]
  placed <- seq_len(nrow(ties)) > nrow(ties) - nrow(between)
  ties[placed, ] <- .separate_ties(ties[placed, , drop = FALSE], 
                                   unlist(memb), twomode = TRUE)
  out <- igraph::make_bipartite_graph(rep(c(FALSE, TRUE), n), c(t(ties)))
  # a tie that could not be moved out of a community may repeat one within it
  out <- igraph::simplify(out)
  out <- igraph::set_vertex_attr(out, "community", value = unlist(memb))
  as_tidygraph(out) |> 
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
