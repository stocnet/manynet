#' Equivalence clustering algorithms
#' 
#' @description 
#'   These functions combine an appropriate `node_by_*()` function
#'   together with methods for calculating the hierarchical clusters
#'   provided by a certain distance calculation.
#'   
#'   - `node_in_equivalence()` assigns nodes membership based on their equivalence 
#'   with respective to some census/class.
#'   The following functions call this function, together with an appropriate census.
#'     - `node_in_structural()` assigns nodes membership based on their
#'     having equivalent ties to the same other nodes.
#'     - `node_in_regular()` assigns nodes membership based on their
#'     having equivalent patterns of ties.
#'     - `node_in_automorphic()` assigns nodes membership based on their
#'     having equivalent distances to other nodes.
#'   
#'   A `plot()` method exists for investigating the dendrogram
#'   of the hierarchical cluster and showing the returned cluster
#'   assignment.
#' @name member_equivalence
#' @family memberships
#' @inheritParams mark_is
#' @param census A matrix returned by a `node_by_*()` function.
#' @param k Typically a character string indicating which method
#'   should be used to select the number of clusters to return.
#'   By default `"silhouette"`, other options include `"elbow"` and `"strict"`.
#'   `"strict"` returns classes with members only when strictly equivalent.
#'   `"silhouette"` and `"elbow"` select classes based on the distance between
#'   clusters or between nodes within a cluster.
#'   Fewer, identifiable letters, e.g. `"e"` for elbow, is sufficient.
#'   Alternatively, if `k` is passed an integer, e.g. `k = 3`,
#'   then all selection routines are skipped in favour of this number of clusters.
#' @param cluster Character string indicating whether clusters should be 
#'   clustered hierarchically (`"hierarchical"`) or 
#'   through convergence of correlations (`"concor"`). 
#'   Fewer, identifiable letters, e.g. `"c"` for CONCOR, is sufficient.
#' @param distance Character string indicating which distance metric
#'   to pass on to `stats::dist`.
#'   By default `"euclidean"`, but other options include
#'   `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"`, and `"minkowski"`.
#'   Fewer, identifiable letters, e.g. `"e"` for Euclidean, is sufficient.
#' @param range Integer indicating the maximum number of (k) clusters
#'   to evaluate.
#'   Ignored when `k = "strict"` or a discrete number is given for `k`.
#' @importFrom stats as.dist hclust cutree coef cor median
#' @source \url{https://github.com/aslez/concoR}
NULL

#' @rdname member_equivalence 
#' @export
node_in_equivalence <- function(.data, census,
                             k = c("silhouette", "elbow", "strict"),
                             cluster = c("hierarchical", "concor", "cosine"),
                             distance = c("euclidean", "maximum", "manhattan", 
                                          "canberra", "binary", "minkowski"),
                             range = 8L){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  hc <- switch(match.arg(cluster),
               hierarchical = cluster_hierarchical(census, 
                                                      match.arg(distance)),
               concor = cluster_concor(.data, census),
               cosine = cluster_cosine(census, 
                                       match.arg(distance)))
  
  if(!is.numeric(k))
    k <- switch(match.arg(k),
                strict = k_strict(hc, .data),
                elbow = k_elbow(hc, .data, census, range),
                silhouette = k_silhouette(hc, .data, range))
  
  out <- make_node_member(stats::cutree(hc, k), .data)
  attr(out, "hc") <- hc
  attr(out, "k") <- k
  out
}

#' @rdname member_equivalence
#' @examples
#' \donttest{
#' (nse <- node_in_structural(ison_algebra))
#' if(require("ggdendro", quietly = TRUE)){
#' plot(nse)
#' }
#' }
#' @export
node_in_structural <- function(.data,
                                        k = c("silhouette", "elbow", "strict"),
                                        cluster = c("hierarchical", "concor","cosine"),
                                        distance = c("euclidean", "maximum", "manhattan", 
                                                     "canberra", "binary", "minkowski"),
                                        range = 8L){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  mat <- node_by_tie(.data)
  if(any(colSums(t(mat))==0)){
    mat <- cbind(mat, (colSums(t(mat))==0))
  } 
  node_in_equivalence(.data, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}

#' @rdname member_equivalence
#' @examples
#' \donttest{
#' (nre <- node_in_regular(ison_southern_women,
#'   cluster = "concor"))
#' if(require("ggdendro", quietly = TRUE)){
#' plot(nre)
#' }
#' }
#' @export
node_in_regular <- function(.data, 
                            k = c("silhouette", "elbow", "strict"),
                            cluster = c("hierarchical", "concor","cosine"),
                            distance = c("euclidean", "maximum", "manhattan", 
                                         "canberra", "binary", "minkowski"),
                            range = 8L){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  if(is_twomode(.data)){
    snet_info("Since this is a two-mode network,", 
              "using {.fn node_by_tetrad} to", 
              "profile nodes' embedding in local structures.")
    mat <- as.matrix(node_by_tetrad(.data))
  } else {
    snet_info("Since this is a one-mode network,", 
              "using {.fn node_by_triad} to", 
              "profile nodes' embedding in local structures.")
    mat <- node_by_triad(.data)
  }
  if(any(colSums(mat) == 0)) mat <- mat[,-which(colSums(mat) == 0)]
  node_in_equivalence(.data, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}

#' @rdname member_equivalence
#' @examples
#' \donttest{
#' if(require("sna", quietly = TRUE)){
#' (nae <- node_in_automorphic(ison_southern_women,
#'   k = "elbow"))
#' if(require("ggdendro", quietly = TRUE)){
#' plot(nae)
#' }
#' }
#' }
#' @export
node_in_automorphic <- function(.data,
                                         k = c("silhouette", "elbow", "strict"),
                                         cluster = c("hierarchical", "concor","cosine"),
                                         distance = c("euclidean", "maximum", "manhattan", 
                                                      "canberra", "binary", "minkowski"),
                                         range = 8L){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  mat <- node_by_path(.data)
  node_in_equivalence(.data, mat, 
                   k = k, cluster = cluster, distance = distance, range = range)
}
