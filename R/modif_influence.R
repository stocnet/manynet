#' Modifying networks into influence networks
#' @name modif_influence
#' @description
#'   `to_influence()` reformats a network into the influence network
#'   that the opinion models of [play_opinion()] use,
#'   in which each node divides its attention among the nodes it listens to.
#'   The result is a directed, weighted network whose weights are
#'   the row-stochastic influence matrix W:
#'   the ties that each node sends sum to 1.
#'
#'   A tie from i to j means that i listens to j,
#'   and its weight is the share of i's attention that goes to j.
#'   A self-loop is the weight that a node gives to its own current opinion,
#'   its self-weight, so a loop of weight 2 on a node with one other tie
#'   of weight 1 gives that node a self-weight of 2/3.
#'   A node that sends no tie listens only to itself,
#'   and so gets a self-weight of 1.
#'
#'   Unlike the `as_*()` group of functions,
#'   this function returns the same class as it is given.
#'   For a matrix, it returns W itself.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_influence"))
#'   ```
#' @template param_data
#' @param weights How the influence of one node on another is weighed.
#'   By default `NULL`, which uses the ties of the network,
#'   with their weights where the network is weighted,
#'   normalised so that the ties each node sends sum to 1.
#'   Self-loops are kept as self-weights.
#'   Otherwise the name of a numeric tie attribute to weigh the ties by,
#'   or a row-stochastic matrix with one row and column for each node,
#'   which is used as it is.
#' @returns A directed, weighted network of the same class,
#'   whose tie weights are the influence matrix W.
#' @family ties
#' @template fam_modif
#' @examples
#' to_influence(ison_adolescents)
#' as_matrix(to_influence(ison_adolescents))
#' @export
to_influence <- function(.data, weights = NULL) UseMethod("to_influence")

#' @export
to_influence.default <- function(.data, weights = NULL){
  as_input(.data, to_influence, weights = weights)
}

#' @export
to_influence.matrix <- function(.data, weights = NULL){
  .influence_matrix(.data, weights)
}

#' @export
to_influence.igraph <- function(.data, weights = NULL){
  W <- .influence_matrix(.data, weights)
  dimnames(W) <- NULL
  out <- igraph::graph_from_adjacency_matrix(W, mode = "directed",
                                             weighted = TRUE, diag = TRUE)
  for(a in igraph::vertex_attr_names(.data))
    out <- igraph::set_vertex_attr(out, a,
                                   value = igraph::vertex_attr(.data, a))
  for(a in igraph::graph_attr_names(.data))
    igraph::graph_attr(out, a) <- igraph::graph_attr(.data, a)
  out
}

#' @export
to_influence.tbl_graph <- function(.data, weights = NULL){
  as_tidygraph(to_influence.igraph(as_igraph(.data), weights = weights))
}

#' @export
to_influence.stocnet <- function(.data, weights = NULL){
  W <- .influence_matrix(.data, weights)
  ties <- which(W != 0, arr.ind = TRUE)
  ties <- ties[order(ties[, 1], ties[, 2]), , drop = FALSE]
  out <- .data
  out$ties <- dplyr::tibble(from = as.integer(ties[, 1]),
                            to = as.integer(ties[, 2]),
                            weight = unname(W[ties]))
  # W is one relation, made from all the ties at all the times,
  # so the details of how those ties were layered or timed no longer apply
  out$info$directed <- TRUE
  out$info$layers <- NULL
  out$info$update <- NULL
  out$info$observation <- NULL
  validate_stocnet(out)
}

# The influence matrix ####

# The one definition of W, used by `to_influence()` and `play_opinion()`
# (and, through `to_influence()`, by the opinion measures in netrics).
# Row i is whom node i listens to, from the ties that i sends.
.influence_matrix <- function(.data, weights = NULL){
  if(is_twomode(.data))
    snet_unavailable("An influence matrix needs a one-mode network.",
                     "Please project the network first, with {.fn to_mode1}.")
  n <- net_nodes(.data)
  if(is.matrix(weights)){
    if(!is.numeric(weights) || any(dim(weights) != n))
      snet_abort("A {.arg weights} matrix must be numeric, with one row and",
                 "one column for each of the {n} nodes in the network.")
    if(anyNA(weights) || any(weights < 0))
      snet_abort("A {.arg weights} matrix must have no missing or negative values.")
    if(any(abs(rowSums(weights) - 1) > sqrt(.Machine$double.eps)))
      snet_abort("Each row of a {.arg weights} matrix must sum to 1.",
                 "Please normalise it, or give the ties and use {.fn to_influence}.")
    W <- weights
  } else {
    if(!is.null(weights)){
      if(!is.character(weights) || length(weights) != 1)
        snet_abort("{.arg weights} must be {.code NULL}, the name of a tie",
                   "attribute, or a row-stochastic matrix.")
      if(!weights %in% net_tie_attributes(.data))
        snet_abort("There is no tie attribute {.val {weights}} in the network.",
                   "The tie attributes are {.val {net_tie_attributes(.data)}}.")
      values <- tie_attribute(.data, weights)
      if(!is.numeric(values) && !is.logical(values))
        snet_abort("The tie attribute {.val {weights}} must be numeric.")
      .data <- add_tie_attribute(.data, "weight", as.numeric(values))
    }
    A <- as_matrix(.data)
    if(length(dim(A)) != 2 || any(dim(A) != n))
      snet_unavailable("An influence matrix needs ties between pairs of nodes,",
                       "so it cannot yet be made from hyperedges or reports.")
    A <- matrix(as.numeric(A), nrow(A), ncol(A), dimnames = dimnames(A))
    A[is.na(A)] <- 0
    if(any(A < 0))
      snet_unavailable("An influence matrix cannot yet be made from",
                       "negative ties.",
                       "Please keep the positive ties first, with {.fn to_positive}.")
    sums <- rowSums(A)
    # A node that listens to no one keeps its own opinion
    alone <- sums == 0
    A[cbind(which(alone), which(alone))] <- 1
    sums[alone] <- 1
    W <- A / sums
  }
  if(is_labelled(.data)){
    dimnames(W) <- list(node_labels(.data), node_labels(.data))
  } else dimnames(W) <- NULL
  W
}
