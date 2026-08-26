#' Modifying networks to their backbone
#' @name modif_backbone
#' @description
#'   These functions reduce a network to its backbone, the ties that carry
#'   more weight, or hold more structure, than a null model expects:
#'
#'   - `to_backbone()` returns the network with only its backbone ties.
#'   - `tie_is_backbone()` marks which ties are in the backbone.
#'
#'   A global cutoff such as `to_unweighted()` compares every tie against the
#'   same number, so it deletes a network's whole periphery before it touches
#'   a hub. A backbone filter compares each tie against a null model local to
#'   its endpoints, so a tie that is weak overall but strong for its own node
#'   is retained. This makes the multiscale structure of a weighted network
#'   visible, both for analysis and for drawing.
#' @details
#'   The work is done by the `stocnet` method, since that class holds both the
#'   ties in the order the network records them and the metadata in which the
#'   transformation is recorded. Every other class is coerced to it and back
#'   again, so all of them are supported.
#'
#'   Each filter is applied to the ties as the network holds them. Where a
#'   network holds parallel ties, each is filtered on its own rather than as
#'   the bundle they make together, so use `to_simplex()` first where that is
#'   not wanted.
#'
#'   Only closed-form filters are offered here. For the backbone models that
#'   resample a null distribution, such as the stochastic and fixed degree
#'   sequence models, or that extract the backbone of a two-mode projection,
#'   see the \pkg{backbone} package (Neal 2022).
#' @references
#'   ## On the disparity filter
#'   Serrano, M. Angeles, Marian Boguna, and Alessandro Vespignani. 2009.
#'   "Extracting the multiscale backbone of complex weighted networks".
#'   _Proceedings of the National Academy of Sciences_ 106(16): 6483-6488.
#'   \doi{10.1073/pnas.0808904106}
#'
#'   ## On locally adaptive network sparsification
#'   Foti, Nicholas J., James M. Hughes, and Daniel N. Rockmore. 2011.
#'   "Nonparametric sparsification of complex multiscale networks".
#'   _PLoS ONE_ 6(2): e16431.
#'   \doi{10.1371/journal.pone.0016431}
#'
#'   ## On the noise-corrected filter
#'   Coscia, Michele, and Frank M. H. Neffke. 2017.
#'   "Network backboning with noisy data".
#'   _IEEE International Conference on Data Engineering_ 33: 425-436.
#'   \doi{10.1109/ICDE.2017.100}
#'
#'   ## On the marginal likelihood filter
#'   Dianati, Navid. 2016.
#'   "Unwinding the hairball graph: Pruning algorithms for weighted complex
#'   networks".
#'   _Physical Review E_ 93(1): 012304.
#'   \doi{10.1103/PhysRevE.93.012304}
#'
#'   ## On Simmelian backbones
#'   Nick, Bobo, Conrad Lee, Padraig Cunningham, and Ulrik Brandes. 2013.
#'   "Simmelian backbones: Amplifying hidden homophily in Facebook networks".
#'   _Advances in Social Networks Analysis and Mining_ 2013: 525-532.
#'   \doi{10.1145/2492517.2492569}
#'
#'   Nocaj, Arlind, Mark Ortmann, and Ulrik Brandes. 2015.
#'   "Untangling the hairballs of multi-centered, small-world online social
#'   media networks".
#'   _Journal of Graph Algorithms and Applications_ 19(2): 595-618.
#'   \doi{10.7155/jgaa.00370}
#'
#'   ## On other backbone models
#'   Neal, Zachary P. 2022.
#'   "backbone: An R package to extract network backbones".
#'   _PLoS ONE_ 17(5): e0269137.
#'   \doi{10.1371/journal.pone.0269137}
#' @template param_data
#' @family ties
#' @template fam_modif
NULL

# The filters, and which of them test each end of a tie separately.
.backbone_filters <- c("disparity", "lans", "noise", "mlf", "simmelian")
.backbone_local <- c("disparity", "lans")

#' @rdname modif_backbone
#' @param filter Which backbone filter to apply, one of:
#'
#'   - "disparity": the disparity filter of Serrano et al. (2009), which tests
#'   a tie's share of its node's strength against a null model in which that
#'   strength is divided at random. Assumes heavy-tailed weights.
#'   - "lans": locally adaptive network sparsification, Foti et al. (2011),
#'   which replaces that null model with the empirical distribution of the
#'   node's own tie weights, and so assumes nothing about their shape.
#'   - "noise": the noise-corrected filter of Coscia and Neffke (2017), which
#'   tests a tie's weight against the strengths of both its endpoints and
#'   allows for the uncertainty of the null itself. Corrects the disparity
#'   filter's bias towards hubs.
#'   - "mlf": the marginal likelihood filter of Dianati (2016), which reads
#'   the weights as counts of independent events and tests each against a
#'   maximum-entropy null. Requires whole-number weights.
#'   - "simmelian": the Simmelian backbone of Nick et al. (2013), which ranks
#'   each node's neighbours by how embedded the tie is and retains the ties
#'   whose endpoints rank each other similarly. Ignores tie weights, and so is
#'   the only filter available for an unweighted network. Reads a directed
#'   network as an undirected one, since it counts shared neighbours.
#'
#'   By default `NULL`, which uses "lans" where the network is weighted and
#'   "simmelian" where it is not. "lans" leads because it assumes nothing about
#'   the shape of the weights, and because it retains each node's strongest tie
#'   whatever that shape is. "disparity" is the better known filter, but its
#'   null model expects heavy-tailed weights: where weights are more even, a
#'   tie's share of its node's strength approaches 1/k for every tie, the
#'   p-value approaches 1/e, and the filter retains nothing at all.
#' @param threshold The cutoff below which a tie is retained.
#'   For the four statistical filters this is a significance level,
#'   by default 0.05.
#'   For "simmelian" it is instead one minus the similarity of the two
#'   endpoints' rankings, by default 0.5.
#'   By default `NULL`, which uses whichever of these the filter calls for.
#' @param endpoints Whether a tie is retained where it passes the filter at
#'   "either" of its endpoints, by default, or only where it passes at "both".
#'   "both" is the more demanding, since a tie must be locally salient to both 
#'   the sender and the receiver.
#'   Applies to the "disparity" and "lans" filters only,
#'   since the others weigh both endpoints at once.
#' @examples
#'   to_backbone(ison_networkers)
#'   to_backbone(ison_networkers, filter = "disparity", threshold = 0.2)
#' @export
to_backbone <- function(.data, filter = NULL, threshold = NULL,
                        endpoints = c("either", "both")) UseMethod("to_backbone")

#' @export
to_backbone.default <- function(.data, filter = NULL, threshold = NULL,
                                endpoints = c("either", "both")){
  as_input(.data, to_backbone, filter = filter, threshold = threshold,
           endpoints = endpoints)
}

#' @export
to_backbone.stocnet <- function(.data, filter = NULL, threshold = NULL,
                                endpoints = c("either", "both")){
  spec <- .backbone_spec(.data, filter, threshold)
  keep <- .backbone_keep(.data, spec, endpoints)
  out <- keep_ties(.data, which(keep))
  # The criterion carries the threshold as well as the filter, since the two
  # together are what decided which ties went. This answers GRAND item 4.4 on
  # its own, so nothing is recorded twice.
  out <- .record_exclusion(out, .data,
                           paste0("not in the ", spec$filter,
                                  " backbone at threshold ", spec$threshold),
                           "ties")
  add_info(out, name = net_name(.data,
                               prefix = paste0(spec$filter,
                                               " backbone of")))
}

# Resolving the filter and the threshold together, since each default depends
# on the other's choice, and reporting both so that a user who named neither
# can see what was done. The abort comes before the message, so that a network
# that cannot be filtered says so rather than announcing a filter it will not
# run.
.backbone_spec <- function(.data, filter, threshold){
  weighted <- is_weighted(.data)
  if(is.null(filter)) filter <- if(weighted) "lans" else "simmelian"
  filter <- match.arg(filter, .backbone_filters)
  if(is_signed(.data))
    snet_abort("A signed network cannot be filtered to its backbone,",
               "since a negative weight has no place in these null models.",
               "Please use {.fn to_unsigned} first.")
  if(!weighted && filter != "simmelian")
    snet_abort("The {.val {filter}} filter needs a weighted network.",
               "Please use {.fn to_weighted} first,",
               "or {.code filter = \"simmelian\"}, which needs no weights.")
  if(is.null(threshold)) threshold <- if(filter == "simmelian") 0.5 else 0.05
  if(!(is.numeric(threshold) && length(threshold) == 1))
    snet_abort("{.arg threshold} must be a single number.")
  snet_info("Using the {.val {filter}} filter at a threshold of",
            "{.val {threshold}}.")
  if(weighted && filter == "simmelian")
    snet_info("The {.val simmelian} filter reads a network's structure",
              "rather than its weights, so the weights here are ignored.")
  list(filter = filter, threshold = threshold)
}

# Returns one logical per tie, in the order the network holds them.
.backbone_keep <- function(.data, spec, endpoints){
  endpoints <- match.arg(endpoints, c("either", "both"))
  if(endpoints == "both" && !spec$filter %in% .backbone_local)
    snet_info("The {.val {spec$filter}} filter weighs both endpoints of a tie",
              "at once, so {.arg endpoints} makes no difference to it.")
  ties <- .backbone_ties(.data)
  if(length(ties$from) == 0) return(logical(0))
  .backbone_warn_repeats(.data, ties)
  scores <- .backbone_scores(ties, spec$filter, endpoints,
                             is_directed(.data))
  # A tie whose weight was never recorded has no value to test, so it is kept
  # rather than dropped, as it is by `to_unweighted()`.
  scores[is.na(scores)] <- 0
  out <- scores < spec$threshold
  if(!any(out))
    snet_warn("The {.val {spec$filter}} filter retains no tie at a threshold",
              "of {.val {spec$threshold}}.",
              "Please raise {.arg threshold}, or try another {.arg filter}:",
              "the {.val disparity} filter especially expects heavy-tailed",
              "weights, and retains nothing where weights are more even.")
  out
}

# Each filter builds its null model from the ties as the network holds them,
# so a dyad tied more than once, whether by parallel ties or by a tie restated
# at each wave, is tested once for each of them rather than once for the pair.
# This is rarely what is wanted, and quietly weakens every test, so it is
# reported rather than assumed.
.backbone_warn_repeats <- function(.data, ties){
  dyads <- if(is_directed(.data)) paste(ties$from, ties$to) else
    paste(pmin(ties$from, ties$to), pmax(ties$from, ties$to))
  repeats <- length(dyads) - length(unique(dyads))
  if(repeats == 0) return(invisible(NULL))
  advice <- if(is_longitudinal(.data) || is_dynamic(.data))
    "Please use {.fn to_time} or {.fn to_waves} to filter one moment at a time." else
      "Please use {.fn to_simplex} or {.fn to_flat} to gather them first."
  snet_warn("This network ties some pairs of nodes more than once,",
            "so {repeats} tie{?s} {?is/are} tested on {?its/their} own rather",
            "than as part of the bundle {?it makes/they make}.", advice)
  invisible(NULL)
}

# Reads a network's ties as plain vectors, in the order it holds them. A
# stocnet is read from its own ties table rather than from a coerced copy,
# since coercion reciprocates its undirected layers and so changes both the
# order and the number of the ties, as `make_tie_mark()` also allows for.
.backbone_ties <- function(.data){
  if(inherits(.data, "stocnet")){
    from <- as.integer(.data$ties$from)
    to <- as.integer(.data$ties$to)
    weight <- .data$ties[["weight"]]
    nodes <- nrow(.data$nodes)
  } else {
    graph <- as_igraph(.data)
    el <- igraph::as_edgelist(graph, names = FALSE)
    from <- as.integer(el[,1])
    to <- as.integer(el[,2])
    weight <- igraph::edge_attr(graph, "weight")
    nodes <- igraph::vcount(graph)
  }
  if(is.null(weight)) weight <- rep(1, length(from))
  list(from = from, to = to, weight = as.numeric(weight), nodes = nodes)
}

.backbone_scores <- function(ties, filter, endpoints, directed){
  if(filter == "simmelian") return(.bb_simmelian(ties, directed))
  if(filter == "mlf") return(.bb_mlf(ties, directed))
  if(filter == "noise") return(.bb_noise(ties, directed))
  # The disparity and LANS filters test one end of a tie at a time. A directed
  # network is tested against the sender's out-ties at one end and the
  # receiver's in-ties at the other. An undirected network has one incident
  # set per node, so both ends are tested against the same doubled index.
  test <- if(filter == "disparity") .bb_disparity else .bb_lans
  m <- length(ties$from)
  if(directed){
    from_p <- test(ties$from, ties$weight, ties$nodes)
    to_p <- test(ties$to, ties$weight, ties$nodes)
  } else {
    both <- test(c(ties$from, ties$to), rep(ties$weight, 2), ties$nodes)
    from_p <- both[seq_len(m)]
    to_p <- both[m + seq_len(m)]
  }
  # A tie is kept where its score falls below the threshold, so requiring both
  # endpoints takes the larger of the two scores and either takes the smaller.
  if(endpoints == "both") pmax(from_p, to_p) else pmin(from_p, to_p)
}

# Summing and counting a value over the ties incident to each node, returning
# one entry per node. A tie whose weight is missing is left out of both, since
# it cannot contribute a value it does not have.
.bb_by_node <- function(ids, values, nodes){
  obs <- !is.na(values)
  grp <- factor(ids[obs], levels = seq_len(nodes))
  total <- tapply(values[obs], grp, sum)
  count <- tapply(values[obs], grp, length)
  total[is.na(total)] <- 0
  count[is.na(count)] <- 0
  list(strength = as.numeric(total), degree = as.numeric(count))
}

# The disparity filter. A node of degree one divides its strength among no
# other tie, so the exponent is zero and the score is one: such a tie is not
# retained on its own account, only on that of its other endpoint.
.bb_disparity <- function(ids, weight, nodes){
  agg <- .bb_by_node(ids, weight, nodes)
  share <- weight / agg$strength[ids]
  (1 - share)^(agg$degree[ids] - 1)
}

# Locally adaptive network sparsification. The score is the share of a node's
# own tie weights that are at least as large as this one, so the node's
# heaviest tie always scores zero and its lightest scores one.
.bb_lans <- function(ids, weight, nodes){
  out <- rep(NA_real_, length(weight))
  for(idx in split(seq_along(weight), factor(ids, levels = seq_len(nodes)))){
    if(!length(idx)) next
    own <- weight[idx]
    obs <- !is.na(own)
    if(!any(obs)) next
    out[idx] <- 1 - vapply(own, function(w)
      mean(own[obs] <= w), numeric(1))
  }
  out
}

# The marginal likelihood filter. The weights are read as counts of
# independent events, of which there are `total` in all, and each tie is
# tested against the chance that so many of them fell on that pair of nodes.
.bb_mlf <- function(ties, directed){
  weight <- ties$weight
  if(any(abs(weight - round(weight)) > .Machine$double.eps^0.5, na.rm = TRUE))
    snet_abort("The {.val mlf} filter reads tie weights as counts of events,",
               "so it needs whole-number weights.",
               "Please use another {.arg filter}, or round the weights.")
  if(directed){
    out <- .bb_by_node(ties$from, weight, ties$nodes)$strength
    inn <- .bb_by_node(ties$to, weight, ties$nodes)$strength
    total <- sum(weight, na.rm = TRUE)
    prob <- out[ties$from] * inn[ties$to] / total^2
  } else {
    str <- .bb_by_node(c(ties$from, ties$to), rep(weight, 2),
                       ties$nodes)$strength
    total <- sum(weight, na.rm = TRUE)
    prob <- str[ties$from] * str[ties$to] / (2 * total^2)
  }
  # The upper tail from the weight itself, so that the score answers how
  # surprising a weight at least this large would be.
  stats::pbinom(weight - 1, size = round(total), prob = pmin(prob, 1),
                lower.tail = FALSE)
}

# The noise-corrected filter. The lift of a tie's weight over what the two
# endpoints' strengths lead one to expect, less the number of standard
# deviations that the threshold asks for. Unlike the marginal likelihood
# filter it does not need whole-number weights, and it allows for the
# uncertainty of the null model as well as of the tie.
.bb_noise <- function(ties, directed){
  weight <- ties$weight
  total <- sum(weight, na.rm = TRUE)
  if(directed){
    si <- .bb_by_node(ties$from, weight, ties$nodes)$strength[ties$from]
    sj <- .bb_by_node(ties$to, weight, ties$nodes)$strength[ties$to]
  } else {
    str <- .bb_by_node(c(ties$from, ties$to), rep(weight, 2),
                       ties$nodes)$strength
    si <- str[ties$from]
    sj <- str[ties$to]
  }
  kappa <- total / (si * sj)
  score <- (kappa * weight - 1) / (kappa * weight + 1)
  variance <- si * sj * (total - si) * (total - sj) / (total^2 * (total - 1))
  # The delta method carries the variance of the weight onto the lift.
  slope <- 2 * kappa / (kappa * weight + 1)^2
  deviations <- score / (slope * sqrt(variance))
  # Reported as an upper-tail probability, so that the threshold means the
  # same thing here as it does for the other statistical filters.
  stats::pnorm(deviations, lower.tail = FALSE)
}

# The Simmelian backbone. Each node ranks its neighbours by how many
# neighbours it shares with each, and a tie is retained where the two rankings
# agree near the top. The score is one minus the largest overlap of the two
# nodes' first k neighbours, over every k, so that a tie whose endpoints rank
# each other first scores zero.
.bb_simmelian <- function(ties, directed){
  n <- ties$nodes
  m <- length(ties$from)
  adj <- vector("list", n)
  for(v in seq_len(n)) adj[[v]] <- integer(0)
  for(e in seq_len(m)){
    adj[[ties$from[e]]] <- c(adj[[ties$from[e]]], ties$to[e])
    adj[[ties$to[e]]] <- c(adj[[ties$to[e]]], ties$from[e])
  }
  adj <- lapply(adj, unique)
  # Neighbours ranked by shared neighbours, the embeddedness of the tie
  # between them, with the most embedded first.
  ranked <- lapply(seq_len(n), function(v){
    nb <- setdiff(adj[[v]], v)
    if(!length(nb)) return(integer(0))
    shared <- vapply(nb, function(u) length(intersect(adj[[v]], adj[[u]])),
                     numeric(1))
    nb[order(-shared)]
  })
  vapply(seq_len(m), function(e){
    a <- ranked[[ties$from[e]]]
    b <- ranked[[ties$to[e]]]
    if(!length(a) || !length(b)) return(1)
    best <- 0
    for(k in seq_len(min(length(a), length(b)))){
      top_a <- a[seq_len(k)]
      top_b <- b[seq_len(k)]
      shared <- length(intersect(top_a, top_b))
      best <- max(best, shared / length(union(top_a, top_b)))
    }
    1 - best
  }, numeric(1))
}
