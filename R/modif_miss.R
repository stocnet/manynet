# Missing ####

#' Imputing missing and incomplete network data
#' @name modif_miss
#' @description
#'   These functions impute what a network did not observe:
#'
#'   - `impute_ties()` imputes the ties a network records as missing,
#'   and the values of the ties it records as incomplete.
#'   - `impute_nodes()` imputes the attributes of the nodes it records as
#'   incomplete.
#'   - `to_imputed()` runs both in a single call.
#'
#'   A network is *missing* a tie where the tie itself was not observed,
#'   so that whether it exists is not known.
#'   A tie or a node is *incomplete* where it is there and observed,
#'   but an attribute of it is not known,
#'   such as the strength of a tie or the age of a node.
#'   Imputing the first is a question of existence, and imputing the second is
#'   a question of value, so each takes its own rules.
#'
#'   If there is nothing to impute,
#'   the network data is returned unaltered and no warning is given,
#'   so that these functions can be used to ensure conformance.
#' @section What is recorded:
#'   Imputation manufactures data, so what was imputed, how much of it, and by
#'   which rule is recorded under the "imputation" name of the network's
#'   transformations, which `describe_transformations()` describes.
#'   Item 4.6 of the GRAND guidelines asks for the imputation method and the
#'   number of nodes or ties that were imputed, and places it among the
#'   transformations of raw data into analytic data,
#'   beside symmetrising, dichotomising, projecting, and aggregating,
#'   which is where the other `to_*()` functions record themselves.
#'
#'   One entry is added for the missing ties, one for the incomplete tie
#'   values, and one for each node attribute, so that a reader can tell which
#'   attributes hold manufactured values and which were observed throughout:
#'
#'   ```
#'   as_infolist(to_imputed(ison_classmates))$transformations$imputation
#'   #> "73 missing ties (zero)"  "4 incomplete 'religion' values (modal)" ...
#'   ```
#'
#'   The element accumulates rather than replaces,
#'   so a network imputed in more than one step reports each of them in order.
#'   A matrix or an edgelist has nowhere to hold information about itself,
#'   so nothing is recorded for those two classes.
#' @template param_data
#' @param rule How the imputed value is arrived at.
#'   See the Rules section for the options and what each does.
#'   By default "zero".
#' @param which Which of the states a tie can be in to impute,
#'   one or more of "nonresponse", "unrecorded", and "incomplete".
#'   See the four states section.
#'   By default all three.
#' @param attribute A character vector naming the node attributes to impute.
#'   By default NULL, which imputes every attribute that holds a missing value.
#' @param ties The rule `to_imputed()` passes to `impute_ties()`,
#'   or NULL to leave the ties alone.
#' @param nodes The rule `to_imputed()` passes to `impute_nodes()`,
#'   or NULL to leave the nodes alone.
#' @template fam_modif
#' @references
#' ## On missing data
#'   Krause, Robert, Mark Huisman, Christian Steglich, and Tom A.B. Snijders. 2020.
#'   "Missing data in cross-sectional networks: An extensive comparison of missing data treatment methods".
#'   _Social Networks_, 62: 99-112.
#'   \doi{10.1016/j.socnet.2020.02.004}
#' @examples
#' missTest <- ison_adolescents |>
#'    add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) |>
#'    as_matrix()
#' missTest
#' impute_ties(missTest)
#' impute_ties(missTest, "mean")
NULL

# impute_ties() ####

#' @rdname modif_miss
#' @export
impute_ties <- function(.data,
                        rule = c("zero", "density", "reciprocity", "indegree",
                                 "mean", "median", "modal"),
                        which = c("nonresponse", "unrecorded", "incomplete"))
  UseMethod("impute_ties")

#' @export
impute_ties.default <- function(.data,
                                rule = c("zero", "density", "reciprocity",
                                         "indegree", "mean", "median", "modal"),
                                which = c("nonresponse", "unrecorded",
                                          "incomplete")){
  as_input(.data, impute_ties, rule = rule, which = which)
}

#' @export
impute_ties.tbl_graph <- function(.data,
                                  rule = c("zero", "density", "reciprocity",
                                           "indegree", "mean", "median",
                                           "modal"),
                                  which = c("nonresponse", "unrecorded",
                                            "incomplete")){
  todo <- .impute_todo(.data, rule, which)
  out <- .data
  # Every statistic is taken from `.data`, the network as observed, so that
  # neither part of the call shifts the ground under the other. Whether a tie
  # exists is settled first, since that is what decides which edges are there
  # for the second part to give a value to.
  if(todo$existence){
    miss <- .missing_flat(.data, todo$which)
    keep <- stats::rbinom(nrow(miss$ties), 1,
                          .impute_probs(todo$rule, .present_flat(.data),
                                        miss$ties, .dyads_possible(.data),
                                        net_nodes(.data),
                                        is_directed(.data))) == 1
    # A tie already among the edges, but marked missing by a weight of NA, is
    # kept or deleted where it stands. One that is not there at all is added.
    out <- .settle_flat_edges(out, miss$held[!keep], miss$held[keep])
    out <- .add_flat_ties(out, miss$ties[keep & is.na(miss$held), , drop = FALSE],
                          .data)
    out <- .drop_missing_record(out)
    out <- .record_imputation(out, nrow(miss$ties), "missing ties", todo$rule)
  }
  # A weight column that holds nothing but 0, 1, and NA is no more valued than
  # a matrix of zeros and ones, so filling such a weight settles whether the
  # tie exists, exactly as filling a matrix cell does.
  if(todo$value){
    unvalued <- sum(is.na(tie_attribute(out, "weight")))
    out <- .fill_weights(out, .miss_statistic(.data, todo$rule))
    out <- .record_imputation(out, unvalued, "incomplete tie values", todo$rule)
  }
  out
}

#' @export
impute_ties.igraph <- function(.data,
                               rule = c("zero", "density", "reciprocity",
                                        "indegree", "mean", "median", "modal"),
                               which = c("nonresponse", "unrecorded",
                                         "incomplete")){
  as_igraph(impute_ties(as_tidygraph(.data), rule = rule, which = which))
}

#' @export
impute_ties.network <- function(.data,
                                rule = c("zero", "density", "reciprocity",
                                         "indegree", "mean", "median", "modal"),
                                which = c("nonresponse", "unrecorded",
                                          "incomplete")){
  as_network(impute_ties(as_tidygraph(.data), rule = rule, which = which))
}

#' @export
impute_ties.matrix <- function(.data,
                               rule = c("zero", "density", "reciprocity",
                                        "indegree", "mean", "median", "modal"),
                               which = c("nonresponse", "unrecorded",
                                         "incomplete")){
  todo <- .impute_todo(.data, rule, which)
  if(!anyNA(.data)) return(.data)
  # A matrix holds one cell for each dyad and nothing else, so a cell it
  # records as missing is both the tie and the tie's value. Whichever of the
  # states was selected, there is only the one thing here to impute.
  if(todo$existence){
    # Read once, before the cells start changing: filling the first of them
    # makes the matrix asymmetric, and a matrix is only read as undirected
    # while it is still symmetric.
    directed <- is_directed(.data); twomode <- is_twomode(.data)
    miss <- as_missinglist(.data)
    # An undirected matrix holds each dyad twice, so it reports each missing
    # tie twice too. One of the two is dropped, and the draw made for the one
    # that remains is written to both cells below.
    if(!directed && !twomode) miss <- miss[miss$from <= miss$to, , drop = FALSE]
    probs <- .impute_probs(todo$rule, .present_flat(.data), miss,
                           .dyads_possible(.data), nrow(.data), directed)
    drawn <- stats::rbinom(nrow(miss), 1, probs)
    for(r in seq_len(nrow(miss))){
      i <- miss$from[[r]]; j <- miss$to[[r]]
      if(twomode) j <- j - nrow(.data)
      .data[i, j] <- drawn[[r]]
      if(!directed && !twomode) .data[j, i] <- drawn[[r]]
    }
  } else .data[is.na(.data)] <- .miss_statistic(.data, todo$rule)
  .data
}

#' @export
impute_ties.data.frame <- function(.data,
                                   rule = c("zero", "density", "reciprocity",
                                            "indegree", "mean", "median",
                                            "modal"),
                                   which = c("nonresponse", "unrecorded",
                                             "incomplete")){
  todo <- .impute_todo(.data, rule, which)
  if(ncol(.data) < 3 || !anyNA(.data[, 3])) return(.data)
  .data[is.na(.data[, 3]), 3] <- if(todo$existence && todo$rule != "zero")
    .miss_average(.data) else .miss_statistic(.data, todo$rule)
  .data
}

#' @export
impute_ties.stocnet <- function(.data,
                                rule = c("zero", "density", "reciprocity",
                                         "indegree", "mean", "median", "modal"),
                                which = c("nonresponse", "unrecorded",
                                          "incomplete")){
  todo <- .impute_todo(.data, rule, which)
  out <- .data
  if(todo$value && !is.null(out$ties[["weight"]])){
    unvalued <- sum(is.na(out$ties$weight))
    out$ties$weight[is.na(out$ties$weight)] <- .miss_statistic(.data, todo$rule)
    out <- .record_imputation(out, unvalued, "incomplete tie values", todo$rule)
  }
  if(todo$existence){
    miss <- .missing_stocnet(.data, todo$which)
    if(!is.null(miss) && nrow(miss)){
      keep <- .stocnet_keep(.data, miss, todo$rule)
      add <- miss[keep, , drop = FALSE]
      out <- .clear_missing(out)
      if(nrow(add)) out$ties <- .bind_imputed_ties(out$ties, add, .data)
      out <- .record_imputation(out, nrow(miss), "missing ties", todo$rule)
    } else out <- .clear_missing(out)
  }
  out
}

# impute_nodes() ####

#' @rdname modif_miss
#' @examples
#' impute_nodes(fict_lotr, "modal", "Race")
#' @export
impute_nodes <- function(.data,
                         rule = c("modal", "mean", "median", "neighbourhood"),
                         attribute = NULL) UseMethod("impute_nodes")

#' @export
impute_nodes.default <- function(.data,
                                 rule = c("modal", "mean", "median",
                                          "neighbourhood"),
                                 attribute = NULL){
  as_input(.data, impute_nodes, rule = rule, attribute = attribute)
}

#' @export
impute_nodes.tbl_graph <- function(.data,
                                   rule = c("modal", "mean", "median",
                                            "neighbourhood"),
                                   attribute = NULL){
  rule <- match.arg(rule)
  attribute <- .incomplete_attributes(.data, attribute)
  if(!length(attribute)) return(.data)
  # Every attribute is imputed from the network as observed, so that one
  # imputed attribute is never the evidence for another.
  filled <- lapply(attribute, function(a)
    .impute_attribute(.data, node_attribute(.data, a), rule))
  out <- .data
  for(i in seq_along(attribute))
    out <- igraph::set_vertex_attr(out, attribute[[i]], value = filled[[i]])
  .record_attribute_imputation(as_tidygraph(out), .data, attribute, rule)
}

#' @export
impute_nodes.igraph <- function(.data,
                                rule = c("modal", "mean", "median",
                                         "neighbourhood"),
                                attribute = NULL){
  as_igraph(impute_nodes(as_tidygraph(.data), rule = rule,
                         attribute = attribute))
}

#' @export
impute_nodes.network <- function(.data,
                                 rule = c("modal", "mean", "median",
                                          "neighbourhood"),
                                 attribute = NULL){
  as_network(impute_nodes(as_tidygraph(.data), rule = rule,
                          attribute = attribute))
}

#' @export
impute_nodes.stocnet <- function(.data,
                                 rule = c("modal", "mean", "median",
                                          "neighbourhood"),
                                 attribute = NULL){
  rule <- match.arg(rule)
  attribute <- .incomplete_attributes(.data, attribute)
  if(!length(attribute)) return(.data)
  filled <- lapply(attribute, function(a)
    .impute_attribute(.data, .data$nodes[[a]], rule))
  out <- .data
  for(i in seq_along(attribute)) out$nodes[[attribute[[i]]]] <- filled[[i]]
  .record_attribute_imputation(out, .data, attribute, rule)
}

#' @export
impute_nodes.matrix <- function(.data,
                                rule = c("modal", "mean", "median",
                                         "neighbourhood"),
                                attribute = NULL){
  # A matrix holds no nodal attributes, so there are none to impute.
  .data
}

#' @export
impute_nodes.data.frame <- function(.data,
                                    rule = c("modal", "mean", "median",
                                             "neighbourhood"),
                                    attribute = NULL){
  # An edgelist holds no nodal attributes, so there are none to impute.
  .data
}

# to_imputed() ####

#' @rdname modif_miss
#' @examples
#' to_imputed(ison_classmates)
#' @export
to_imputed <- function(.data, ties = "zero", nodes = "modal")
  UseMethod("to_imputed")

#' @export
to_imputed.default <- function(.data, ties = "zero", nodes = "modal"){
  out <- .data
  # Each rule is worked out from the network as observed, so neither call
  # shifts the ground under the other and the order they run in is immaterial.
  if(!is.null(ties)) out <- impute_ties(out, rule = ties)
  if(!is.null(nodes)) out <- impute_nodes(out, rule = nodes)
  out
}

# Which and rule ------------------

# Which rules answer whether a tie exists, and which answer what its value is.
# "zero" answers both: a missing tie becomes no tie, and an unknown value
# becomes no value.
.existence_rules <- c("zero", "density", "reciprocity", "indegree")
.value_rules <- c("zero", "mean", "median", "modal")

# What a call to `impute_ties()` has been asked to do, as the rule, the states
# selected, and whether each of the two parts applies. A rule that suits some
# of the selected states and not others leaves the rest alone and says so,
# rather than refusing the whole call.
.impute_todo <- function(.data, rule, which){
  rule <- match.arg(rule, c("zero", "density", "reciprocity", "indegree",
                            "mean", "median", "modal"))
  which <- match.arg(which, c("nonresponse", "unrecorded", "incomplete"),
                     several.ok = TRUE)
  wants_existence <- any(c("nonresponse", "unrecorded") %in% which)
  wants_value <- "incomplete" %in% which
  existence <- wants_existence && rule %in% .existence_rules
  value <- wants_value && rule %in% .value_rules
  if(!existence && !value){
    # Named plainly here, since cli reads a brace expression that starts with
    # a dot as one of its own styles rather than as a variable.
    suits <- if(wants_existence) .existence_rules else .value_rules
    what <- if(wants_existence) "whether a tie exists" else "what a tie's value is"
    snet_abort(c(x = "The {.val {rule}} rule does not apply to any of the states you selected.",
                 i = "For {what}, use {.val {suits}}."))
  }
  if(wants_existence && !existence)
    snet_info("Leaving the missing ties as they are, since {.val {rule}} says what a tie's value is and not whether it exists.")
  if(wants_value && !value)
    snet_info("Leaving the incomplete ties as they are, since {.val {rule}} says whether a tie exists and not what its value is.")
  if(existence) .report_unavailable(.data)
  list(rule = rule, which = which, existence = existence, value = value)
}

# A tie to a node that was not in the network was never one that could have
# been observed, so it is passed over rather than imputed. Saying how many
# were passed over is the difference between a network that is missing data
# and one that simply changed size.
.report_unavailable <- function(.data){
  if(!inherits(.data, "stocnet")) return(invisible(NULL))
  inactive <- sum(!.node_ever_active(.data))
  if(inactive)
    snet_info("Passing over the {inactive} node{?s} that {?was/were} not in the network, since a tie to a node that is not there is not one that could have been observed.")
  invisible(NULL)
}

.node_ever_active <- function(.data){
  times <- .stocnet_times(.data)
  apply(.node_state(.data, "active", times, default = TRUE), 1, any)
}

# Recording ------------------

# GRAND item 4.6 asks for the imputation method and the number of nodes or
# ties that were imputed, so both are recorded under the "imputation" name of
# the network's transformations, beside the symmetrising and the projecting
# that the rest of section 4 covers. The rule leads and its consequence
# follows in parentheses, as in every other name.
.record_imputation <- function(.data, count, what, rule){
  if(!count) return(.data)
  if(count == 1) what <- gsub("s$", "", what)
  .record_transformation(.data, "imputation",
                         paste0(rule, " (", count, " ", what, ")"))
}

# One entry for each attribute, so that a reader can see which of them hold
# manufactured values and which were observed throughout.
.record_attribute_imputation <- function(out, observed, attribute, rule){
  nodes <- if(inherits(observed, "stocnet")) observed$nodes else
    tibble::as_tibble(as_tidygraph(observed), active = "nodes")
  for(a in attribute)
    out <- .record_imputation(out, sum(is.na(nodes[[a]])),
                              paste0("incomplete '", a, "' values"), rule)
  out
}

# Statistics ------------------

# The value an incomplete tie is imputed with. Taken over the matrix rather
# than over the list of ties, so that cells the network records as absent
# count towards it, and so that every class arrives at the same figure.
.miss_statistic <- function(.data, rule){
  if(rule == "zero") return(0)
  x <- as_matrix(.data)
  if(!is_twomode(.data) && !is_complex(.data)) diag(x) <- NA
  switch(rule,
         mean = mean(x, na.rm = TRUE),
         median = stats::median(x, na.rm = TRUE),
         modal = .modal(x[!is.na(x)]),
         mean(x, na.rm = TRUE))
}

# The average value a missing tie is imputed with, which for a binary network
# is the density.
.miss_average <- function(.data){
  x <- as_matrix(.data)
  # a node's tie to itself is not usually a tie that could have been observed,
  # so counting the diagonal would bias the average down by (n-1)/n
  if(!is_twomode(.data) && !is_complex(.data)) diag(x) <- NA
  mean(x, na.rm = TRUE)
}

# The most frequent value, breaking a tie between two equally frequent values
# at random rather than by whichever sorts first.
.modal <- function(x){
  x <- x[!is.na(x)]
  if(!length(x)) return(NA)
  counts <- table(x)
  top <- names(counts)[counts == max(counts)]
  pick <- if(length(top) == 1L) top else sample(top, 1L)
  if(is.factor(x)) factor(pick, levels = levels(x)) else
    if(is.numeric(x)) as.numeric(pick) else
      if(is.logical(x)) as.logical(pick) else pick
}

# How many ties a network could have observed on one occasion, an occasion
# being one layer at one moment.
.dyads_possible <- function(.data){
  n <- as.numeric(net_nodes(.data))
  if(is_twomode(.data)) prod(table(node_is_mode(.data))) else
    if(is_directed(.data)) n*(n-1) else n*(n-1)/2
}

# Probabilities ------------------

# The probability each missing tie is imputed as present, as a vector the
# length of `miss`. `pres` and `miss` hold the ties observed as present and
# the ties not observed at all, over one occasion.
.impute_probs <- function(rule, pres, miss, dyads, n, directed){
  if(!nrow(miss)) return(numeric(0))
  if(rule == "zero") return(rep(0, nrow(miss)))
  density <- min(1, nrow(pres) / dyads)
  switch(rule,
         density = rep(density, nrow(miss)),
         indegree = .probs_indegree(pres, miss, n, directed, density),
         reciprocity = .probs_reciprocity(pres, miss, dyads, density),
         rep(density, nrow(miss)))
}

.pair_key <- function(from, to) paste(from, to, sep = "\r")

# Drawing a missing tie at the proportion of the responding nodes that named
# the node it runs to, so that a popular node is the more likely to be
# imputed a tie. For an undirected network the node's degree stands in for its
# indegree, since there is only the one.
.probs_indegree <- function(pres, miss, n, directed, density){
  named <- if(directed) pres$to else c(pres$to, pres$from)
  unobs <- if(directed) miss$to else c(miss$to, miss$from)
  count <- table(factor(named, levels = seq_len(n)))
  gaps <- table(factor(unobs, levels = seq_len(n)))
  # The nodes that could have named this one, less the ones that did not report
  askable <- (n - 1) - as.numeric(gaps)
  prop <- as.numeric(count) / askable
  prop[!is.finite(prop) | askable <= 0] <- density
  pmin(1, pmax(0, prop[miss$to]))
}

# Reconstructing a missing tie from what the other node reported. A tie is
# drawn at the proportion of observed ties that run both ways where the other
# node named this one, and at the proportion that run one way only where it
# did not, rather than simply copying the other node's report, which would
# assume a reciprocity no observed network shows.
.probs_reciprocity <- function(pres, miss, dyads, density){
  presk <- .pair_key(pres$from, pres$to)
  missk <- .pair_key(miss$from, miss$to)
  pres_rev <- .pair_key(pres$to, pres$from)
  miss_rev <- .pair_key(miss$to, miss$from)
  nP <- length(presk); nM <- length(missk); nO <- dyads - nM
  # Reversing a pair is a bijection, so the pairs whose reciprocal is present
  # number as many as the present ties, less those that were not observed.
  mutual <- sum(pres_rev %in% presk)
  d_recip <- nP - sum(miss_rev %in% presk)
  rev_miss_obs <- nM - sum(miss_rev %in% missk)
  d_norecip <- nO - d_recip - rev_miss_obs
  n_norecip <- nP - mutual - sum(pres_rev %in% missk)
  p_recip <- if(d_recip > 0) mutual / d_recip else density
  p_norecip <- if(d_norecip > 0) n_norecip / d_norecip else density
  out <- rep(density, nM)
  reciprocated <- miss_rev %in% presk
  out[reciprocated] <- p_recip
  # Where the other node was asked and did not name this one, the pair is
  # observed to run one way at most.
  out[!reciprocated & !(miss_rev %in% missk)] <- p_norecip
  pmin(1, pmax(0, out))
}

# Flat classes ------------------

# The ties a network of a class other than stocnet records as missing. Only a
# stocnet records which of non-response and an unrecorded tie applies, so for
# the others both names select the whole list.
.missing_flat <- function(.data, which){
  if(!all(c("nonresponse", "unrecorded") %in% which))
    snet_info("This class does not record why each tie is missing, so all of them are imputed. Only a stocnet object tells non-response and an unrecorded tie apart.")
  ties <- as_missinglist(.data)
  ties <- if(is.null(ties) || !nrow(ties))
    dplyr::tibble(from = integer(0), to = integer(0)) else ties[c("from", "to")]
  # `held` numbers the edge that already carries each missing tie, or NA where
  # the network holds no edge for it at all.
  held <- rep(NA_integer_, nrow(ties))
  marked <- .edges_marked_missing(.data)
  if(length(marked)){
    el <- .edgelist_indices(.data)
    ties <- dplyr::bind_rows(ties, el[marked, , drop = FALSE])
    held <- c(held, marked)
  }
  list(ties = ties, held = held)
}

# A binary network's ties are sometimes held as weights of 1, so that a tie
# recorded as missing can sit among them as a weight of NA. Such a weight
# marks a tie whose existence is in question, and not a tie of unknown value.
.holds_values <- function(.data){
  if(is.matrix(.data) || is.data.frame(.data)) return(is_weighted(.data))
  "weight" %in% igraph::edge_attr_names(.data) && is_weighted(.data)
}

.edges_marked_missing <- function(.data){
  # A matrix holds a missing tie as a missing cell rather than among its ties,
  # so `as_missinglist()` has already returned every one of them.
  if(is.matrix(.data) || is.data.frame(.data)) return(integer(0))
  if(!"weight" %in% igraph::edge_attr_names(.data) || .holds_values(.data))
    return(integer(0))
  which(is.na(igraph::edge_attr(.data, "weight")))
}

.edgelist_indices <- function(.data){
  if(is.matrix(.data)){
    idx <- which(!is.na(.data) & .data != 0, arr.ind = TRUE)
    to <- idx[, 2]
    if(is_twomode(.data)) to <- to + nrow(.data)
    return(dplyr::tibble(from = as.integer(idx[, 1]), to = as.integer(to)))
  }
  el <- igraph::as_edgelist(as_igraph(.data), names = FALSE)
  dplyr::tibble(from = as.integer(el[, 1]), to = as.integer(el[, 2]))
}

.present_flat <- function(.data){
  el <- .edgelist_indices(.data)
  marked <- .edges_marked_missing(.data)
  if(length(marked)) el <- el[-marked, , drop = FALSE]
  el
}

# Settling the edges the network already holds for a missing tie: the ones
# imputed as absent are deleted, and the ones imputed as present lose the
# weight of NA that marked them as never observed.
.settle_flat_edges <- function(.data, drop, keep){
  drop <- drop[!is.na(drop)]; keep <- keep[!is.na(keep)]
  out <- .data
  if(length(keep)){
    wts <- igraph::edge_attr(out, "weight")
    wts[keep] <- 1
    out <- igraph::set_edge_attr(out, "weight", value = wts)
  }
  if(length(drop)) out <- igraph::delete_edges(out, drop)
  as_tidygraph(out)
}

.fill_weights <- function(.data, stat){
  weight <- NULL
  # an unweighted network has no tie values, and so none that can be missing
  if(!"weight" %in% igraph::edge_attr_names(.data)) return(.data)
  out <- .data |> mutate_ties(weight = ifelse(is.na(weight), stat, weight))
  # A weight column holding nothing but 0 and 1 records which ties are present
  # rather than what they are worth, so a tie left weighing 0 is not a tie at
  # all and its edge goes with it. Were it left in place, `as_matrix()` would
  # read the column as unweighted and show the tie as present after all.
  wts <- igraph::edge_attr(out, "weight")
  if(.holds_only_binary(wts) && any(wts == 0, na.rm = TRUE))
    out <- igraph::delete_edges(out, which(wts == 0))
  as_tidygraph(out)
}

.add_flat_ties <- function(.data, add, observed){
  if(!nrow(add)) return(.data)
  out <- igraph::add_edges(.data, as.vector(rbind(add$from, add$to)))
  # An imputed tie in a weighted network needs a value as well as an end, and
  # the average observed value is the one that leaves the average unmoved.
  if("weight" %in% igraph::edge_attr_names(.data)){
    wts <- igraph::edge_attr(out, "weight")
    wts[is.na(wts) & seq_along(wts) > igraph::ecount(.data)] <-
      .miss_statistic(observed, "mean")
    out <- igraph::set_edge_attr(out, "weight", value = wts)
  }
  as_tidygraph(out)
}

.drop_missing_record <- function(.data){
  if("missings" %in% igraph::graph_attr_names(.data))
    .data <- igraph::delete_graph_attr(.data, "missings")
  as_tidygraph(.data)
}

# Stocnet ------------------

# The ties a stocnet records as missing, restricted to the states selected.
# The registry holds the ties that no node's non-response implies, so the two
# states are the registry and everything else.
.missing_stocnet <- function(.data, which){
  miss <- as_missinglist(.data)
  if(is.null(miss) || !nrow(miss)) return(NULL)
  wanted <- intersect(which, c("nonresponse", "unrecorded"))
  if(length(wanted) == 2L) return(miss)
  reg <- .missing_registry(.data)
  in_reg <- if(is.null(reg)) rep(FALSE, nrow(miss)) else
    .tie_key(miss) %in% .tie_key(reg)
  miss[if(identical(wanted, "unrecorded")) in_reg else !in_reg, , drop = FALSE]
}

# Which of a stocnet's missing ties become ties. The draw is made within each
# layer and moment, since a multiplex or longitudinal network holds several
# ties for each dyad and pooling them would put the density far too high.
.stocnet_keep <- function(.data, miss, rule){
  if(rule == "zero") return(rep(FALSE, nrow(miss)))
  directed <- is_directed(.data)
  if(rule == "reciprocity" && !directed)
    snet_abort(c(x = "The {.val reciprocity} rule needs a directed network.",
                 i = "In an undirected network a tie is already what both nodes reported, so there is nothing to reconstruct from. Use {.val density} instead."))
  dyads <- .stocnet_dyads_each(.data)
  n <- as.numeric(net_nodes(.data))
  cols <- intersect(c("layer", "time"), names(.data$ties))
  key <- function(x) if(!length(cols)) rep("", nrow(x)) else
    do.call(paste, c(lapply(cols, function(c) as.character(x[[c]])),
                     list(sep = "\r")))
  tie_occ <- key(.data$ties)
  miss_occ <- key(miss)
  out <- logical(nrow(miss))
  for(occ in unique(miss_occ)){
    at <- miss_occ == occ
    probs <- .impute_probs(rule, .data$ties[tie_occ == occ, c("from", "to"),
                                            drop = FALSE],
                           miss[at, , drop = FALSE], dyads, n, directed)
    out[at] <- stats::rbinom(sum(at), 1, probs) == 1
  }
  out
}

# Adding the imputed ties to a stocnet's own ties, keeping only the columns
# the ties component holds and giving a weighted network a value for each.
.bind_imputed_ties <- function(ties, add, observed){
  add <- add[intersect(names(ties), names(add))]
  if("weight" %in% names(ties) && !"weight" %in% names(add))
    add$weight <- if(is_weighted(observed))
      .miss_statistic(observed, "mean") else 1
  dplyr::bind_rows(ties, add)
}

# Node attributes ------------------

# Which node attributes hold a missing value, since imputing a complete one
# would only be work for nothing.
.incomplete_attributes <- function(.data, attribute){
  nodes <- if(inherits(.data, "stocnet")) .data$nodes else
    as_tibble(as_tidygraph(.data), active = "nodes")
  named <- if(is.null(attribute))
    setdiff(names(nodes), manynet_reserved_node_attributes) else attribute
  missed <- setdiff(named, names(nodes))
  if(length(missed))
    snet_abort("There {?is/are} no node attribute{?s} named {.val {missed}} in this network.")
  named <- named[vapply(nodes[named], anyNA, logical(1))]
  # An attribute no node was observed to hold gives nothing to impute from,
  # so it is left as it is rather than filled with a statistic of nothing.
  empty <- named[vapply(nodes[named], function(x) all(is.na(x)), logical(1))]
  if(length(empty))
    snet_info("Leaving {.val {empty}} as {?it is/they are}, since no node was observed to hold {?a value/values} for {?it/them}.")
  setdiff(named, empty)
}

# One attribute's values, with the missing ones filled in. A rule that names a
# statistic needs a numeric attribute, so a categorical one takes its modal
# value whatever was asked for.
.impute_attribute <- function(.data, values, rule){
  gaps <- is.na(values)
  if(!any(gaps)) return(values)
  if(!rule %in% c("modal", "neighbourhood") && !is.numeric(values)){
    snet_info("Using the modal value for the categorical attributes, since {.val {rule}} needs a numeric one.")
    rule <- "modal"
  }
  # A factor is filled as plain text and made a factor again at the end, so
  # that a level is never assigned into a vector that does not declare it.
  levs <- if(is.factor(values)) levels(values) else NULL
  plain <- if(is.factor(values)) as.character(values) else values
  plain[gaps] <- if(rule == "neighbourhood")
    .neighbourhood_values(.data, plain, gaps) else
      .attribute_statistic(plain, rule)
  if(is.null(levs)) plain else factor(plain, levels = levs)
}

.attribute_statistic <- function(values, rule){
  switch(rule,
         modal = .modal(values),
         mean = mean(values, na.rm = TRUE),
         median = stats::median(values, na.rm = TRUE),
         .modal(values))
}

# Each incomplete node takes the statistic of the values its neighbours were
# observed to hold. A node with no neighbour, or none that was observed, has
# nothing nearby to draw on, so the network's own statistic stands in.
.neighbourhood_values <- function(.data, values, gaps){
  stat <- if(is.numeric(values)) "mean" else "modal"
  overall <- .attribute_statistic(values, stat)
  g <- to_undirected(as_igraph(.data))
  idx <- which(gaps)
  out <- rep(overall, length(idx))
  for(k in seq_along(idx)){
    near <- values[as.integer(igraph::neighbors(g, idx[[k]]))]
    near <- near[!is.na(near)]
    if(length(near)) out[[k]] <- .attribute_statistic(near, stat)
  }
  out
}
