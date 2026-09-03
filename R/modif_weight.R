#' Modifying tie weight formats
#' @name modif_weight
#' @description
#'   These functions reformat tie attributes like their weight or sign:
#' 
#'   - `to_unweighted()` reformats weighted network data to unweighted network 
#'   data, with all tie weights removed.
#'   - `to_unsigned()` reformats signed network data to unsigned network data,
#'   keeping just the "positive" or the "negative" ties, or "both",
#'   which keeps every tie but replaces its sign with its magnitude.
#'   - `to_normalised()` rescales tie weights relative to the other ties of the
#'   same node, so that a value reads as a share rather than a count.
#' 
#'   If the format condition is not met,
#'   for example `to_undirected()` is used on a network that is already undirected,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(sign|weight|normalis)"))
#'   ```
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @template param_data
#' @family ties
#' @template fam_modif
NULL

#' @rdname modif_weight
#' @param keep In the case of a signed network, whether to retain
#'   the "positive" or the "negative" ties, or "both",
#'   which retains every tie but replaces its sign with its magnitude.
#' @importFrom igraph delete_edges E delete_edge_attr
#' @examples
#' marvel <- to_uniplex(fict_marvel, "relationship")
#' to_unsigned(marvel, "positive")
#' to_unsigned(marvel, "both")
#' @export
to_unsigned <- function(.data, 
                        keep = c("positive", "negative",
                                 "both")) UseMethod("to_unsigned")

#' @export
to_unsigned.default <- function(.data, keep = c("positive", "negative",
                                                "both")){
  as_input(.data, to_unsigned, keep = keep)
}

#' @export
to_unsigned.matrix <- function(.data, 
                               keep = c("positive", "negative", "both")){
  keep <- match.arg(keep)
  out <- .data
  if(keep == "positive"){
    out[out < 0] <- 0
  } else if (keep == "negative"){
    out[out > 0] <- 0
    out <- abs(out)
  } else out <- abs(out)
  out
}

#' @export
to_unsigned.data.frame <- function(.data, 
                                   keep = c("positive", "negative", "both")){
  if(!is_signed(.data)) return(.data)
  keep <- match.arg(keep)
  # signs may be held either in a 'sign' column or as negative weights.
  # The ties of the other sign are dropped rather than zeroed, so that an
  # edgelist reads as the other methods' networks do.
  # a tibble warns where `$` names a column it does not have, so `[[` is used
  signs <- if(!is.null(.data[["sign"]])) sign(.data[["sign"]]) else
    sign(.data[["weight"]])
  out <- .data[switch(keep,
                      positive = signs >= 0,
                      negative = signs <= 0,
                      both = rep(TRUE, length(signs))), , drop = FALSE]
  rownames(out) <- NULL
  out$sign <- NULL
  # the weights that remain carry the magnitude of the relation, not its
  # direction, so an unsigned network keeps them positive
  if(!is.null(out[["weight"]])) out$weight <- abs(out[["weight"]])
  out
}

#' @export
to_unsigned.tbl_graph <- function(.data, 
                                  keep = c("positive", "negative", "both")){
  keep <- match.arg(keep)
  out <- to_unsigned(as_igraph(.data), keep = keep)
  dropped <- switch(keep, positive = "negative ties",
                    negative = "positive ties", both = "no ties")
  # 'both' excludes no tie, so this records nothing. Taking the magnitude of a
  # weight is not an exclusion, and none of the transformation items names it,
  # so it goes unrecorded until one does.
  as_tidygraph(out) |> .record_exclusion(.data, dropped, "ties")
}

#' @export
to_unsigned.stocnet <- function(.data,
                                keep = c("positive", "negative", "both")){
  if(!is_signed(.data)) return(.data)
  keep <- match.arg(keep)
  # signs may be held either in a 'sign' column or as negative weights.
  # The ties to drop are named rather than the ties to keep, so that a tie
  # with no sign is kept, as it is in the igraph method.
  signs <- as.numeric(tie_signs(.data))
  dropped <- switch(keep, positive = which(signs < 0),
                    negative = which(signs > 0), both = integer(0))
  out <- keep_ties(.data, setdiff(seq_len(nrow(.data$ties)), dropped))
  out$ties$sign <- NULL
  # the weights that remain carry the magnitude of the relation, not its
  # direction, so an unsigned network keeps them positive
  if(!is.null(out$ties$weight)) out$ties$weight <- abs(out$ties$weight)
  dropped <- switch(keep, positive = "negative ties",
                    negative = "positive ties", both = "no ties")
  # 'both' excludes no tie, so this records nothing. Taking the magnitude of a
  # weight is not an exclusion, and none of the transformation items names it,
  # so it goes unrecorded until one does.
  .record_exclusion(out, .data, dropped, "ties")
}

#' @export
to_unsigned.igraph <- function(.data,
                               keep = c("positive", "negative", "both")){
  if (is_signed(.data)) {
    keep <- match.arg(keep)
    # signs may be held either in a 'sign' attribute or as negative weights
    signs <- as.numeric(tie_signs(.data))
    out <- if (keep == "positive") {
      igraph::delete_edges(.data, which(signs < 0))
    } else if (keep == "negative") {
      igraph::delete_edges(.data, which(signs > 0))
    } else .data
    if ("sign" %in% igraph::edge_attr_names(out))
      out <- igraph::delete_edge_attr(out, "sign")
    if ("weight" %in% igraph::edge_attr_names(out)) {
      # the weights that remain carry the magnitude of the relation, not its
      # direction, so an unsigned network keeps them positive
      wts <- abs(igraph::edge_attr(out, "weight"))
      out <- if (all(wts == 1, na.rm = TRUE))
        igraph::delete_edge_attr(out, "weight") else
          igraph::set_edge_attr(out, "weight", value = wts)
    }
    out
  } else .data
}

#' @export
to_unsigned.network <- function(.data,
                                keep = c("positive", "negative", "both")){
  keep <- match.arg(keep)
  as_network(to_unsigned(as_igraph(.data), keep = keep))
}

#' @rdname modif_weight
#' @importFrom dplyr filter select
#' @export
to_unweighted <- function(.data, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.default <- function(.data, threshold = 1){
  as_input(.data, to_unweighted, threshold = threshold)
}

#' @export
to_unweighted.tbl_graph <- function(.data, threshold = 1) {
  if(!is_weighted(.data)) return(.data)
  edges <- weight <- NULL
  # A tie recorded as missing has no value to compare with the threshold,
  # and so is kept as missing rather than dropped. The weights are then kept
  # too, since they are all that records which ties those are.
  out <- .data |> activate(edges) |>
    dplyr::filter(is.na(weight) | weight >= threshold)
  out <- if(anyNA(tie_weights(out)))
    dplyr::mutate(out, weight = ifelse(is.na(weight), NA_real_, 1)) else
      dplyr::select(out, -c(weight))
  .record_dichotomisation(out, .data, threshold)
}

#' @export
to_unweighted.stocnet <- function(.data, threshold = 1) {
  if(!is_weighted(.data)) return(.data)
  weights <- .data$ties$weight
  # A tie recorded as missing has no value to compare with the threshold, and
  # so is kept as missing rather than dropped, as in the tbl_graph method.
  out <- keep_ties(.data, which(is.na(weights) | weights >= threshold))
  out$ties$weight <- if(anyNA(out$ties$weight))
    ifelse(is.na(out$ties$weight), NA_real_, 1) else NULL
  .record_dichotomisation(out, .data, threshold)
}

#' @export
to_unweighted.igraph <- function(.data, threshold = 1) {
  as_igraph(to_unweighted(as_tidygraph(.data), threshold))
}

#' @export
to_unweighted.network <- function(.data, threshold = 1) {
  as_network(to_unweighted(as_tidygraph(.data), threshold))
}

#' @export
to_unweighted.matrix <- function(.data, threshold = 1) {
  (.data >= threshold)*1
}

#' @export
to_unweighted.data.frame <- function(.data, threshold = 1) {
  if(is_edgelist(.data)) .data[,1:2]
  else snet_abort("Not an edgelist")
}

#' @rdname modif_weight
#' @param mark A mark (logical vector) the length of the ties in the network.
#' @export
to_signed <- function(.data, mark = NULL) UseMethod("to_signed")

#' @export
to_signed.default <- function(.data, mark = NULL){
  as_input(.data, to_signed, mark = mark)
}

#' @export
to_signed.matrix <- function(.data, mark = NULL){
  if(is.null(mark)){
    out <- ifelse(stats::runif(length(.data))>=0.5, .data, -.data)  
    snet_info("Since no mark given, signs are generated by splitting",
              "a uniform distribution.")
  } else out <- ifelse(mark, .data[.data!=0], -.data[.data!=0])
  if(is_labelled(.data)){
    out <- matrix(out, nrow(.data), ncol(.data), 
                  dimnames = list(rownames(.data),colnames(.data)))
  } else out <- matrix(out, nrow(.data), ncol(.data))
  out
}

#' @export
to_signed.data.frame <- function(.data, mark = NULL){
  if(is.null(mark)) mark <- stats::runif(nrow(.data))>=0.5
  out <- data.frame(.data, sign = ifelse(mark, 1, -1))
  dplyr::tibble(out)
}

#' @export
to_signed.tbl_graph <- function(.data, mark = NULL){
  if(is.null(mark)){
    ties <- net_ties(.data)
    snet_info("Since no mark given, signs are generated by splitting",
              "a uniform distribution.")
    .data |> mutate_ties(sign = ifelse(stats::runif(ties)>=0.5, 1, -1))
  } else .data |> mutate_ties(sign = ifelse(mark, 1, -1))
}

#' @export
to_signed.igraph <- function(.data, mark = NULL){
  as_igraph(to_signed.tbl_graph(as_tidygraph(.data), mark = mark))
}

#' @export
to_signed.network <- function(.data, mark = NULL){
  as_network(to_signed.tbl_graph(as_tidygraph(.data), mark = mark))
}

#' @rdname modif_weight
#' @param measure A numeric vector (measure) that will be added as the tie
#'   weights to the network.
#'   If this is NULL, then the tie weights will be drawn from a 
#'   Poisson distribution with \eqn{\lambda = 4}.
#' @export
to_weighted <- function(.data, measure = NULL) UseMethod("to_weighted")

#' @export
to_weighted.default <- function(.data, measure = NULL){
  as_input(.data, to_weighted, measure = measure)
}

#' @export
to_weighted.tbl_graph <- function(.data, measure = NULL){
  if(is.null(measure)){
    measure <- stats::rpois(net_ties(.data), lambda = 4)
    snet_info("Since no measure values given, weights are generated from",
              "a Poisson distribution with lambda = 4.")
  }
  .data |> mutate_ties(weight = measure)
}

#' @export
to_weighted.igraph <- function(.data, measure = NULL){
  as_igraph(to_weighted.tbl_graph(as_tidygraph(.data), measure = measure))
}

#' @export
to_weighted.network <- function(.data, measure = NULL){
  as_network(to_weighted.tbl_graph(as_tidygraph(.data), measure = measure))
}

# Normalisation ####

#' @rdname modif_weight
#' @param rule How each tie value is rescaled, relative to the other values
#'   recorded for the same node.
#'   - "max" (the default) divides by the largest of them, so that a node's
#'   strongest tie is 1 and its others are read against that.
#'   - "mean" divides by the average of them, counting every dyad and not just
#'   those tied, so that 1 marks a tie of typical strength.
#'   - "sum" divides by the total of them, so that they add to 1 and each reads
#'   as the share of the node's ties that goes to that partner.
#'
#'   Missing values propagate rather than being ignored,
#'   so that a node with an unobserved tie has all its values unobserved.
#'   Use `impute_ties()` first to state a different assumption.
#' @template param_across
#' @details
#'   `to_normalised()` divides by the sending node's denominator for "rows",
#'   so that a value says what share of \eqn{i}'s ties goes to \eqn{j},
#'   and by the receiving node's denominator for "columns".
#'
#'   Rescaling a one-mode network across its rows or columns makes it
#'   asymmetric, since what \eqn{i} sends \eqn{j} is generally not what \eqn{j}
#'   sends \eqn{i}. Where such a network is undirected, each tie is therefore
#'   split into two, and the network is returned directed.
#' @examples
#' to_normalised(ison_networkers, rule = "sum", across = "rows")
#' @export
to_normalised <- function(.data, rule = c("max", "mean", "sum"),
                          across = c("both", "rows",
                                     "columns")) UseMethod("to_normalised")

#' @rdname modif_weight
#' @export
to_normalized <- to_normalised

#' @export
to_normalised.default <- function(.data, rule = c("max", "mean", "sum"),
                                  across = c("both", "rows", "columns")){
  as_input(.data, to_normalised, rule = rule, across = across)
}

#' @export
to_normalised.matrix <- function(.data, rule = c("max", "mean", "sum"),
                                 across = c("both", "rows", "columns")){
  .normalise_matrix(.data, match.arg(rule), match.arg(across))
}

#' @export
to_normalised.tbl_graph <- function(.data, rule = c("max", "mean", "sum"),
                                    across = c("both", "rows", "columns")){
  rule <- match.arg(rule)
  across <- match.arg(across)
  out <- .data
  if(.normalise_splits(.data, across)){
    snet_info("Rescaling {across} gives each direction of a tie its own value,",
              "so the network is returned directed.")
    # Each tie becomes two arcs. `to_directed()` is not used here because it
    # gives an undirected tie one direction at random, which would throw half
    # the values away.
    out <- as_tidygraph(igraph::as_directed(as_igraph(out), mode = "mutual"))
  }
  mat <- .normalise_matrix(as_matrix(out), rule, across)
  # The values are written back onto the ties that are already there, rather
  # than the network being rebuilt from the matrix, so that node attributes and
  # everything recorded about the network survive.
  out <- mutate_ties(out, weight = mat[.normalise_index(out, mat)])
  .record_transformation(out, "normalisation", paste(rule, "across", across))
}

#' @export
to_normalised.igraph <- function(.data, rule = c("max", "mean", "sum"),
                                 across = c("both", "rows", "columns")){
  as_igraph(to_normalised(as_tidygraph(.data), rule = rule, across = across))
}

#' @export
to_normalised.network <- function(.data, rule = c("max", "mean", "sum"),
                                  across = c("both", "rows", "columns")){
  as_network(to_normalised(as_tidygraph(.data), rule = rule, across = across))
}

#' @export
to_normalised.data.frame <- function(.data, rule = c("max", "mean", "sum"),
                                     across = c("both", "rows", "columns")){
  if(!is_edgelist(.data)) snet_abort("Not an edgelist")
  as_edgelist(to_normalised(as_tidygraph(.data), rule = rule, across = across))
}

#' @export
to_normalised.stocnet <- function(.data, rule = c("max", "mean", "sum"),
                                  across = c("both", "rows", "columns")){
  rule <- match.arg(rule)
  across <- match.arg(across)
  out <- .data
  if(.normalise_splits(.data, across)){
    snet_info("Rescaling {across} gives each direction of a tie its own value,",
              "so the network is returned directed.")
    # An undirected layer holds one row per dyad, so the reverse of each row is
    # added and the network declared directed, which is what coercion does too.
    swapped <- out$ties
    swapped$from <- out$ties$to
    swapped$to <- out$ties$from
    out$ties <- dplyr::bind_rows(out$ties, swapped)
    directed <- out$info$directed %||% TRUE
    directed[] <- TRUE
    out$info$directed <- directed
  }
  mat <- .normalise_matrix(as_matrix(out), rule, across)
  out$ties$weight <- mat[.normalise_index(out, mat)]
  .record_transformation(out, "normalisation", paste(rule, "across", across))
}

# Whether rescaling this network across this margin makes it asymmetric while
# it has no way of holding that. A two-mode network never does, since its rows
# and columns are different nodesets, and "both" never does either.
.normalise_splits <- function(.data, across){
  across != "both" && !is_twomode(.data) && !is_directed(.data)
}

# The cell of the matrix each tie sits in. A two-mode network numbers its
# second nodeset on from its first, so those indices are brought back to the
# columns they name.
.normalise_index <- function(.data, mat){
  el <- igraph::as_edgelist(as_igraph(.data), names = FALSE)
  cols <- el[,2]
  if(is_twomode(.data)) cols <- cols - nrow(mat)
  cbind(el[,1], cols)
}

# The denominator for each row of a matrix. The column case is this applied to
# the transpose, so there is one calculation rather than three.
.normalise_denominators <- function(mat, rule){
  switch(rule,
         max = apply(mat, 1, max),
         mean = rowMeans(mat),
         sum = rowSums(mat))
}

# A denominator that cannot divide anything. An isolate's is zero, and a
# negative row total, which a signed network can give, leaves "both" taking the
# square root of a negative number.
.normalise_unusable <- function(x){
  is.nan(x) | is.infinite(x) | (!is.na(x) & x == 0)
}

.normalise_matrix <- function(mat, rule, across){
  rows <- .normalise_denominators(mat, rule)
  cols <- .normalise_denominators(t(mat), rule)
  denom <- switch(across,
                  rows = matrix(rows, nrow(mat), ncol(mat)),
                  columns = matrix(cols, nrow(mat), ncol(mat), byrow = TRUE),
                  both = sqrt(outer(rows, cols)))
  out <- mat / denom
  # Dividing by an unusable denominator gives NaN, or -Inf where the largest of
  # an empty row is taken, neither of which is a tie value. Those cells keep the
  # zero they had, and the user is told how many nodes this happened for. A
  # value missing in the data stays missing, which is not the same thing.
  unusable <- .normalise_unusable(denom)
  out[unusable] <- mat[unusable]
  count <- sum(.normalise_unusable(switch(across, rows = rows, columns = cols,
                                          both = c(rows, cols))))
  if(count > 0)
    snet_warn("{count} row{?s}/column{?s} had no value to be scaled against,",
              "such as an isolate's. Those ties are left as they were.")
  out
}
