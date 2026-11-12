# Opinion ####

#' Making opinion models on networks
#'
#' @description
#'   These functions play models of opinion dynamics upon networks,
#'   in which nodes change their opinions towards those of the nodes
#'   they listen to.
#'
#'   - `play_opinion()` plays an opinion model upon a network,
#'   and returns the network with a record of the opinions in its changes.
#'   - `as_opinion()` reads the record of an opinion from a network,
#'   such as one that `play_opinion()` returns,
#'   and returns a table of the opinion of each node at each step.
#'   This table is of the class 'opinionModel',
#'   with 'learn_model' as its second class for one release.
#'
#'   The opinions are numbers, such as the probability that a node
#'   puts on some statement being true, or a position on a scale.
#'   For opinions that are categories, see [play_diffusion()].
#'
#' @section Rules:
#'   `rule` chooses how the nodes update their opinions at each step:
#'
#'   - `"degroot"` (the default): all nodes at once take the average of the
#'   opinions of the nodes they listen to, weighted by the influence matrix W
#'   (DeGroot 1974).
#'   This is deterministic.
#'   On a connected network with no stubbornness, it reaches a consensus.
#'   - `"hegselmann_krause"`: all nodes at once take the weighted average of
#'   the opinions of only those nodes they listen to whose opinions are
#'   within `confidence` of their own (Hegselmann and Krause 2002).
#'   This is deterministic, and opinions can settle into several clusters.
#'   - `"deffuant"`: at each step, one node is drawn at random,
#'   and it draws one of the nodes it listens to, in proportion to W.
#'   If their opinions are within `confidence` of each other,
#'   each moves halfway towards the other, where it listens to the other
#'   (Deffuant et al. 2000).
#'   This is stochastic, so set a seed to repeat a result.
#'   Since only one pair can change at each step, this rule usually needs
#'   many more `steps` than the others.
#'
#'   `confidence` is used only by the bounded-confidence rules,
#'   `"hegselmann_krause"` and `"deffuant"`.
#'   Where `opinions` hold several issues,
#'   the distance between two nodes' opinions is the Euclidean distance
#'   across the issues.
#'
#' @section Self-weight and stubbornness:
#'   Two quantities decide how much a node holds on to its own opinion:
#'
#'   - Its __self-weight__ is how much it listens to itself,
#'   compared with the nodes it listens to.
#'   It is the diagonal of W, and in a network it is a self-loop:
#'   a loop of weight 2 on a node with one tie of weight 1 to another node
#'   gives that node a self-weight of 2/3 (see [to_influence()]).
#'   A self-weight pulls a node towards its _current_ opinion.
#'   - Its __stubbornness__ is how much it keeps its _first_ opinion,
#'   compared with what the network says.
#'   It is `stubbornness`, where 0 is DeGroot's model and
#'   1 is a node that never moves from its first opinion.
#'
#'   The update is \eqn{x(t+1) = (I - S) W x(t) + S x(0)},
#'   where \eqn{S} is the diagonal matrix of stubbornness.
#'   This is the model of Friedkin and Johnsen (1990),
#'   and `stubbornness` is 1 minus what they call susceptibility.
#'
#'   On a path A–B–C with the first opinions 0, 0.5, and 1:
#'
#'   | Setting | Final opinions | What it shows |
#'   |---|---|---|
#'   | No self-weight, stubbornness 0 (DeGroot) | 0.5, 0.5, 0.5 | Consensus. |
#'   | Self-weight 1/2 at every node | 0.5, 0.5, 0.5 | Slower, but the same consensus. |
#'   | Self-weight 2/3 at A only | 0.33, 0.33, 0.33 | A has more influence, but there is still a consensus. |
#'   | Stubbornness 0.5 at every node | 0.25, 0.5, 0.75 | Disagreement lasts. |
#'   | Stubbornness 1 at A | 0, 0, 0 | A pulls everyone to its first opinion. |
#'   | Self-weight 1 at A | 0, 0, 0 | The same as stubbornness 1: A listens only to itself. |
#'
#'   So a node that listens only to itself is fully stubborn.
#'   That is the only case where the two quantities give the same result.
#'   With self-weights alone, disagreement lasts only where two or more nodes
#'   are fully stubborn, or where the network is not connected.
#'   With `stubbornness`, disagreement lasts also where every node is only
#'   partly stubborn.
#'
#' @section The record:
#'   `play_opinion()` returns the network it was given,
#'   with the opinions at the start as a node attribute,
#'   and each later change in the changes of the network.
#'   A node gets a row for a step only where its opinion has moved by more
#'   than `tolerance` since its last row, so the record stays small,
#'   and `as_opinion()` rebuilds each step to within `tolerance`.
#'   The run stops when no opinion moves by `tolerance` or more in a step,
#'   or, for `"deffuant"`, when no pair that listens to each other
#'   can move any more, or after `steps` steps.
#'   Where the attribute named by `opinions` already has a record,
#'   the run continues from the last opinions and the last time of that record.
#' @name make_opinion
#' @family makes
#' @family models
#' @inheritParams mark_is
#' @param opinions The opinions of the nodes at the start.
#'   One of:
#'   - a numeric or logical vector, with one value for each node,
#'   which is recorded as the node attribute `"opinions"`;
#'   - the name of a numeric node attribute, which is then also the name
#'   under which the changes are recorded;
#'   - a matrix of nodes by issues, with one column for each issue,
#'   recorded under its column names, or `"opinions_1"`, `"opinions_2"`, and
#'   so on.
#'   Several names of node attributes are read as several issues.
#'
#'   For `as_opinion()`, the name of the node attribute whose record is read.
#' @param rule The rule by which the nodes update their opinions,
#'   one of `"degroot"` (the default), `"hegselmann_krause"`, or `"deffuant"`.
#'   See the section on rules below.
#' @param stubbornness How much each node holds on to its first opinion,
#'   from 0 (the default, DeGroot's model) to 1 (it never moves).
#'   One value for all the nodes, a vector with one value for each node,
#'   or the name of a numeric node attribute.
#' @param confidence How far apart two opinions can be for one node still to
#'   listen to the other.
#'   A node listens only to the nodes it is tied to whose opinions are
#'   within `confidence` of its own, and ignores the rest.
#'   For example, with opinions from 0 to 1 and `confidence = 0.2`,
#'   a node with an opinion of 0.5 listens to its neighbours from 0.3 to 0.7.
#'   A small value lets opinions settle into several separate clusters,
#'   and a large value lets them meet in one.
#'   The name comes from models of "bounded confidence",
#'   in which a node trusts only opinions close enough to its own.
#'   It is used only by `"hegselmann_krause"` and `"deffuant"`.
#'   By default `Inf`, so that every node listens to all the nodes
#'   it is tied to.
#' @inheritParams modif_influence
#' @param steps The largest number of steps to play.
#'   By default the number of nodes in the network.
#' @param tolerance The smallest change in an opinion that counts.
#'   The run stops when no opinion changes by this much in a step,
#'   and smaller changes are not recorded.
#' @returns
#'   `play_opinion()` returns the network of the class it was given,
#'   with the opinions at the start as node attributes,
#'   and the later changes in its changes.
#'   A matrix or an edgelist cannot hold these, so for one of them
#'   a 'stocnet' is returned.
#' @references
#' DeGroot, Morris H. 1974.
#' "Reaching a consensus",
#' _Journal of the American Statistical Association_, 69(345): 118–21.
#' \doi{10.1080/01621459.1974.10480137}
#'
#' Deffuant, Guillaume, David Neau, Frederic Amblard, and Gérard Weisbuch. 2000.
#' "Mixing beliefs among interacting agents",
#' _Advances in Complex Systems_, 3(1): 87-98.
#' \doi{10.1142/S0219525900000078}
#'
#' Friedkin, Noah E., and Eugene C. Johnsen. 1990.
#' "Social influence and opinions",
#' _Journal of Mathematical Sociology_, 15(3-4): 193-206.
#' \doi{10.1080/0022250X.1990.9990069}
#'
#' Golub, Benjamin, and Matthew O. Jackson 2010.
#' "Naive learning in social networks and the wisdom of crowds",
#' _American Economic Journal_, 2(1): 112-49.
#' \doi{10.1257/mic.2.1.112}
#'
#' Hegselmann, Rainer, and Ulrich Krause. 2002.
#' "Opinion dynamics and bounded confidence: Models, analysis and simulation",
#' _Journal of Artificial Societies and Social Simulation_, 5(3).
#' @examples
#'   net <- play_opinion(ison_networkers,
#'       opinions = rbinom(net_nodes(ison_networkers), 1, prob = 0.25))
#'   as_opinion(net)
#'   # A stubborn node pulls the others towards its first opinion
#'   path <- create_explicit(A-B-C)
#'   as_opinion(play_opinion(path, opinions = c(0, 0.5, 1),
#'                           stubbornness = c(1, 0, 0), steps = 50))
#' @export
play_opinion <- function(.data,
                         opinions,
                         rule = c("degroot", "hegselmann_krause", "deffuant"),
                         stubbornness = 0,
                         confidence = Inf,
                         weights = NULL,
                         steps,
                         tolerance = 0.0005){
  rule <- match.arg(rule)
  if(missing(steps)) steps <- net_nodes(.data)
  if(is.matrix(.data) || is.data.frame(.data)){
    snet_info("A matrix or edgelist cannot hold node attributes or changes,",
              "so the network is returned as a {.cls stocnet}.")
    .data <- as_stocnet(.data)
  }
  if(missing(opinions)){
    if(!"opinions" %in% net_node_attributes(.data))
      snet_abort("Please give the opinions at the start with {.arg opinions},",
                 "as a vector, a matrix, or the name of a node attribute.")
    opinions <- "opinions"
  }
  .play_opinion(.data, opinions, rule, stubbornness, confidence, weights,
                steps, tolerance, record = tolerance)
}

# `record` is the smallest change that gets a row in the record. It is
# `tolerance` for `play_opinion()`, and 0 for `play_learning()`, whose table
# shows every change, however small.
.play_opinion <- function(.data, opinions, rule, stubbornness, confidence,
                          weights, steps, tolerance, record = tolerance){
  if(!is.numeric(confidence) || length(confidence) != 1 ||
     is.na(confidence) || confidence < 0)
    snet_abort("{.arg confidence} must be a single number of 0 or more.")
  if(!is.numeric(tolerance) || length(tolerance) != 1 ||
     is.na(tolerance) || tolerance < 0)
    snet_abort("{.arg tolerance} must be a single number of 0 or more.")
  if(rule == "degroot" && is.finite(confidence))
    snet_warn("{.arg confidence} is used only by the bounded-confidence rules,",
              "so it is ignored by {.val degroot}.",
              "Please set {.code rule = \"hegselmann_krause\"} or",
              "{.code rule = \"deffuant\"} to use it.")
  n <- net_nodes(.data)
  start <- .opinion_start(.data, opinions, n)
  s <- .opinion_stubbornness(.data, stubbornness, n)
  W <- .influence_matrix(.data, weights)
  dimnames(W) <- NULL

  x0 <- start$x
  x <- x0
  recorded <- x0
  t <- start$time
  rows <- list()
  if(rule == "deffuant"){
    talks <- W
    diag(talks) <- 0
    pairs <- which(talks > 0, arr.ind = TRUE)
  }
  repeat{
    if(t - start$time >= steps) break
    old <- x
    if(rule == "deffuant"){
      if(!.deffuant_can_move(x, pairs, s, confidence, tolerance)) break
      i <- sample.int(n, 1)
      j <- sample.int(n, 1, prob = W[i, ])
      if(i != j && .opinion_distance(x[i, ], x[j, ]) <= confidence){
        xi <- x[i, ]
        xj <- x[j, ]
        x[i, ] <- (1 - s[i]) * (xi + (xj - xi) / 2) + s[i] * x0[i, ]
        if(W[j, i] > 0)
          x[j, ] <- (1 - s[j]) * (xj + (xi - xj) / 2) + s[j] * x0[j, ]
      }
    } else {
      M <- W
      if(rule == "hegselmann_krause" && is.finite(confidence)){
        M <- W * (as.matrix(stats::dist(x)) <= confidence)
        sums <- rowSums(M)
        # A node that listens to no one inside its confidence keeps its opinion
        alone <- sums == 0
        M[cbind(which(alone), which(alone))] <- 1
        sums[alone] <- 1
        M <- M / sums
      }
      x <- (1 - s) * (M %*% x) + s * x0
      if(all(abs(old - x) < tolerance)) break
    }
    t <- t + 1
    moved <- abs(x - recorded) > record
    if(any(moved)){
      for(k in which(colSums(moved) > 0)){
        who <- which(moved[, k])
        rows[[length(rows) + 1]] <- data.frame(time = t, node = who,
                                               var = start$vars[k],
                                               value = x[who, k])
      }
      recorded[moved] <- x[moved]
    }
  }

  if(!start$on_nodes){
    for(k in seq_along(start$vars))
      .data <- add_node_attribute(.data, start$vars[k], x0[, k])
  }
  if(length(rows)){
    changes <- do.call(rbind, rows)
    changes <- changes[order(changes$time, changes$node), , drop = FALSE]
    rownames(changes) <- NULL
    .data <- bind_changes(.data, changes)
  }
  .data
}

.opinion_start <- function(.data, opinions, n){
  time <- 0
  if(is.character(opinions)){
    unknown <- setdiff(opinions, net_node_attributes(.data))
    if(length(unknown))
      snet_abort("There is no node attribute {.val {unknown}} in the network.",
                 "Please give the opinions as a vector or matrix,",
                 "or name a node attribute that holds them.")
    x <- vapply(opinions, function(a){
      v <- node_attribute(.data, a)
      if(!is.numeric(v) && !is.logical(v))
        snet_abort("The node attribute {.val {a}} must be numeric.")
      as.numeric(v)
    }, numeric(n))
    x <- matrix(x, nrow = n)
    vars <- opinions
    on_nodes <- TRUE
    # Where the attribute already has a record, the run continues from it
    changes <- as_changelist(.data)
    if(!is.null(changes) && any(changes$var %in% vars)){
      for(k in seq_along(vars)){
        if(!vars[k] %in% changes$var) next
        history <- as_opinion(.data, vars[k])
        x[, k] <- unlist(history[nrow(history), ], use.names = FALSE)
      }
      time <- max(changes$time[changes$var %in% vars])
    }
  } else {
    if(is.data.frame(opinions)) opinions <- as.matrix(opinions)
    if(is.matrix(opinions)){
      if(nrow(opinions) != n)
        snet_abort("A matrix of {.arg opinions} must have one row for each",
                   "of the {n} nodes in the network, not {nrow(opinions)}.")
      vars <- colnames(opinions)
      if(is.null(vars))
        vars <- if(ncol(opinions) == 1) "opinions" else
          paste0("opinions_", seq_len(ncol(opinions)))
      x <- opinions
    } else {
      if(length(opinions) != n)
        snet_abort("{.arg opinions} must have one value for each of the",
                   "{n} nodes in the network, not {length(opinions)}.")
      x <- matrix(opinions, ncol = 1)
      vars <- "opinions"
    }
    if(!is.numeric(x) && !is.logical(x))
      snet_abort("{.arg opinions} must be numeric or logical.")
    x <- matrix(as.numeric(x), nrow = n)
    on_nodes <- FALSE
  }
  if(anyNA(x))
    snet_abort("{.arg opinions} must not hold missing values.",
               "Please impute them first, or leave those nodes out.")
  list(x = x, vars = vars, on_nodes = on_nodes, time = time)
}

.opinion_stubbornness <- function(.data, stubbornness, n){
  if(is.character(stubbornness)){
    if(length(stubbornness) != 1 ||
       !stubbornness %in% net_node_attributes(.data))
      snet_abort("There is no node attribute {.val {stubbornness}}",
                 "in the network.")
    stubbornness <- node_attribute(.data, stubbornness)
  }
  if(is.logical(stubbornness)) stubbornness <- stubbornness * 1
  if(!is.numeric(stubbornness) || !length(stubbornness) %in% c(1, n) ||
     anyNA(stubbornness) || any(stubbornness < 0 | stubbornness > 1))
    snet_abort("{.arg stubbornness} must be one number, or one number for",
               "each of the {n} nodes, from 0 to 1.")
  rep_len(as.numeric(stubbornness), n)
}

.opinion_distance <- function(a, b) sqrt(sum((a - b)^2))

# Whether any pair that talks can still move by `tolerance` or more:
# the two are inside each other's confidence, but not yet within tolerance,
# and the one that listens is not fully stubborn.
.deffuant_can_move <- function(x, pairs, s, confidence, tolerance){
  if(!nrow(pairs)) return(FALSE)
  d <- sqrt(rowSums((x[pairs[, 1], , drop = FALSE] -
                       x[pairs[, 2], , drop = FALSE])^2))
  any(d <= confidence & d / 2 * (1 - s[pairs[, 1]]) >= tolerance)
}
