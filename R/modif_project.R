# Projecting ####

#' Modifying networks projection
#' @name modif_project
#' @description
#'   These functions offer tools for projecting manynet-consistent data:
#' 
#'   - `to_mode()` projects a two-mode network to a one-mode network
#'   of the node set given by the `mode` argument, which accepts either the
#'   index of the mode or its name.
#'   - `to_mode1()` projects a two-mode network to a one-mode network
#'   of the first node set's (e.g. rows) joint affiliations to nodes in the second node set (columns). 
#'   - `to_mode2()` projects a two-mode network to a one-mode network
#'   of the second node set's (e.g. columns) joint affiliations to nodes in the first node set (rows).
#'   - `to_linegraph()` projects a network to its line graph,
#'   where the ties become nodes and incident nodes become their ties.
#'   - `to_hypergraph()` projects one-mode or two-mode network data into hypergraph data, 
#'   where ties can connect more than two nodes.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(mode[0-9]|linegraph|hypergraph)"))
#'   ```
#' @section Comparison of two-mode projection methods:
#'
#' | Category | Feature | `manynet::to_mode1()`/`to_mode2()` | `igraph::bipartite_projection()` | `network`/`sna` manual |
#' |---|---|---|---|---|
#' | **Input** | Dedicated function | yes | yes | no |
#' | | Accepted input classes | igraph, network, tidygraph, matrix, edgelist | igraph only | any (manual extraction) |
#' | | Detects mode membership from | `mode` node attribute | `type` vertex attribute | `bipartite` network attr (positional) |
#' | **Projection** | Returns both projections at once | no — one per call | yes — list of two | two manual calls |
#' | | Projects mode 1 (actors) | `to_mode1()` | `which = "true"` | `A %*% t(A)` |
#' | | Projects mode 2 (events) | `to_mode2()` | `which = "false"` | `t(A) %*% A` |
#' | **Weights** | Raw co-membership counts | yes | yes (`multiplicity = TRUE`) | yes |
#' | | Binary (unweighted) output | yes | yes (`multiplicity = FALSE`) | threshold manually |
#' | | Jaccard normalisation | yes | no | code manually |
#' | | Cosine normalisation | yes | no | code manually |
#' | | Other similarity measures | 25 in all, see `similarity` | no | code manually |
#' | **Attributes** | Retains node attributes | yes | yes | no — lost in matrix round-trip |
#' | | Retains edge attributes | weight only | weight only | no |
#' | | Removes self-loops automatically | yes | yes | `diag(P) <- 0` manually |
#' | **Output** | Output class matches input | yes | no | no  |
#' | | Directed projection support | limited | no — undirected only | yes — asymmetric matrix |
#' | **Usability** | Lines of code (basic case) | 1 | 1 | 4–6 |
#' | | Lines of code (Jaccard weights) | 1 | ~8 manual | ~8 manual |
#' | | Pipe-friendly | yes | with wrappers | no |
#' @template param_data
#' @template fam_modif
#' @family projections
NULL

#' @rdname modif_project
#' @param similarity Method for establishing ties in the projection,
#'   "count" by default.
#'
#'   The measures are grouped below by what they are sensitive to.
#'   Within a group they are monotone transformations of one another,
#'   and so rank dyads identically, differing only in their scale.
#'   Choosing between the groups therefore matters rather more than
#'   choosing within one.
#'
#'   Measures defined for valued as well as binary data:
#'
#'   - "count" counts the coinciding ties, the cross-product \eqn{XX'}.
#'   For valued data it sums the products of tie strengths.
#'   Interpret it as the degree of opportunity between two nodes.
#'   This is the default, and the quickest, since it can use
#'   `igraph::bipartite_projection()` directly.
#'   - "match" counts the cells in which two nodes hold exactly the same
#'   value, joint absences included. For binary data this is "rand" multiplied
#'   by the number of nodes in the other mode, but for valued data it is the
#'   more general measure, since it registers agreement at any tie strength.
#'   Use it where the level of involvement is meaningful in itself.
#'   - "crossmin" sums the smaller of each pair of tie strengths.
#'   Interpret it as the capacity two nodes could jointly bring to bear.
#'   For binary data it reduces to "count".
#'   - "overlap" divides "crossmin" by the smaller of the two nodes' total tie
#'   strength, the Szymkiewicz-Simpson or overlap coefficient, and so lies in
#'   \eqn{[0,1]} for non-negative data. Use it where one node is much more
#'   active than the other, and the less active node's rarity should not
#'   depress the score. For binary data it is the count over the smaller
#'   degree. xUCINET keeps that count for valued data too, where it can
#'   exceed 1 and is no longer the coefficient.
#'   - "ruzicka" divides "crossmin" by the two nodes' combined tie strength
#'   less than shared, Ruzicka's weighted Jaccard coefficient, and so also
#'   lies in \eqn{[0,1]}. For binary data it is "jaccard", so use it where
#'   "jaccard" is wanted but the tie strengths should be kept.
#'   - "maxcrossmin" takes the largest such minimum rather than their sum,
#'   so that a single strong shared affiliation stands for the pair.
#'   For binary data it collapses to an indicator of any shared affiliation,
#'   and so is of little use there.
#'   - "sqdiff" inverts the sum of squared differences in tie strength,
#'   as \eqn{1/(1+d)}, so that identical rows give 1 and larger values mean
#'   more alike, as for every other measure here.
#'   Recover the raw sum of squared differences as \eqn{1/x - 1}.
#'   - "pearson" gives Pearson's product-moment correlation and "covariance"
#'   its unstandardised counterpart. Use "covariance" where the variance in
#'   involvement is itself of interest, and "pearson" where it is not.
#'   - "spearman" and "kendall" are the rank counterparts of "pearson".
#'   Use them where tie strengths order the affiliations reliably but their
#'   spacing does not, as with ordinal ratings.
#'   - "cosine" gives the cosine of the angle between two nodes' rows.
#'   It differs from "pearson" in not centring them first, so that it reads
#'   two nodes as alike where their involvements are proportional rather than
#'   where they depart from the average in the same direction.
#'   For binary data it agrees with "ochiai", except that a node with no ties
#'   at all is reported as no more similar to another than any other node,
#'   where "ochiai" would divide by zero.
#'   - "euclidean" and "manhattan" invert the straight-line and the
#'   city-block distance between two rows, again as \eqn{1/(1+d)}.
#'   "manhattan" sums the absolute differences in tie strength, so that a
#'   large discrepancy on one affiliation counts no more than the same total
#'   spread over several; "euclidean" penalises the concentrated discrepancy
#'   more heavily, as "sqdiff" does. Recover either raw distance as
#'   \eqn{1/x - 1}.
#'
#'   Measures defined for binary data only, where a valued network is
#'   dichotomised at zero with a warning. Writing \eqn{a} for the cells in
#'   which both nodes are present, \eqn{b} and \eqn{c} for those in which just
#'   one is, and \eqn{d} for those in which neither is:
#'
#'   - Sensitive to co-presence, ignoring joint absence: "jaccard" is
#'   \eqn{a/(a+b+c)}, opportunity weighted by participation; "czekanowski"
#'   (the Dice or Sorensen coefficient) is \eqn{2a/(2a+b+c)}, which
#'   double-weights co-presence; and "sokalsneath" is \eqn{a/(a+2(b+c))},
#'   which instead double-weights mismatch. Use these where joint
#'   non-participation says nothing, as in a sparse affiliation network with
#'   many events. For valued data, "ruzicka" is the counterpart of "jaccard"
#'   that keeps the tie strengths.
#'   - Sensitive to matching, counting joint absence as evidence: "rand",
#'   the Simple Matching Coefficient, is \eqn{(a+d)/(a+b+c+d)}; "hamann" is
#'   \eqn{((a+d)-(b+c))/(a+b+c+d)}, the same quantity rescaled onto
#'   \eqn{[-1,1]} so that its sign reports whether matches outnumber
#'   mismatches; and "rogerstanimoto" is \eqn{(a+d)/(a+2(b+c)+d)}, which
#'   double-weights mismatch. Interpret these as the degree of behavioural
#'   mirroring between two nodes, and use them where not attending is as
#'   informative as attending.
#'   - Sensitive to association, through the odds ratio \eqn{ad/bc}: "yule"
#'   is Yule's Q, \eqn{(ad-bc)/(ad+bc)}, which has a straightforward reading
#'   as a rescaled odds ratio; and "bonacich" is
#'   \eqn{\sqrt{ad}/(\sqrt{ad}+\sqrt{bc})}, which is Yule's Y rescaled onto
#'   \eqn{[0,1]}. Both saturate at 1 wherever \eqn{b} or \eqn{c} is zero.
#'   - Geometric: "ochiai" is \eqn{a/\sqrt{(a+b)(a+c)}}, the cosine of the
#'   angle between two nodes' affiliation vectors, and "ochiai2" (also known
#'   as Sokal and Sneath's fifth measure) is
#'   \eqn{ad/\sqrt{(a+b)(a+c)(d+b)(d+c)}}, its counterpart including joint
#'   absence. Neither is monotone in any of the above, so both are worth
#'   trying alongside them.
#'   - "hamming" inverts the Hamming distance, the number of cells in which
#'   the two nodes differ, as \eqn{1/(1+d)}. It is a monotone transformation
#'   of "rand", and so ranks dyads identically, but states the disagreement
#'   as a count rather than as a proportion of agreement.
#' @seealso [to_proximity()], which applies these same measures to a one-mode
#'   network, comparing nodes on their ties to one another rather than on
#'   their affiliations to a second mode.
#'
#'   [to_cosine()], which takes the cosine over the columns of a
#'   matrix without projecting it.
#' @references
#' ## On two-mode projection
#'   Borgatti, Stephen P., and Daniel S. Halgin. 2011.
#'   "Analyzing affiliation networks".
#'   In _The SAGE Handbook of Social Network Analysis_, 417-433. London: SAGE.
#'
#'   Bonacich, Phillip. 1972.
#'   "Technique for analyzing overlapping memberships".
#'   _Sociological Methodology_ 4: 176-185.
#'   \doi{10.2307/270732}
#' @importFrom igraph bipartite_projection
#' @importFrom stats cor cov dist
#' @examples
#' to_mode1(ison_southern_women)
#' to_mode2(ison_southern_women)
#' @export
to_mode1 <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  # projecting a network that is already one-mode is a no-op
  if(!is_twomode(.data)) return(.data)
  UseMethod("to_mode1")
}

# A projection keeps one mode and discards the other, so a change recorded
# about a node of the discarded mode describes a node the result does not have.
# The projection keeps the nodes it retains in their original order, so the new
# index of a kept node is its rank among them. Without this the changes still
# name the old indices, which `validate_stocnet()` then rejects.
.project_changes <- function(.data, kept){
  if(is.null(.data$changes) || nrow(.data$changes) == 0) return(.data)
  out <- .data
  out$changes <- dplyr::filter(.data$changes, node %in% kept) |>
    dplyr::mutate(node = match(node, kept))
  if(nrow(out$changes) == 0) out$changes <- NULL
  out
}

#' @export
to_mode1.default <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")){
  as_input(.data, to_mode1, similarity = similarity)
}

#' @export
to_mode1.stocnet <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")){
  similarity <- match.arg(similarity)
  # The tidygraph method is called directly rather than through `as_input()`,
  # which would pick this method again and recurse.
  out <- to_mode1(as_tidygraph(.project_changes(.data, which(!node_is_mode(.data)))),
                  similarity = similarity)
  as_stocnet(out)
}

#' @export
to_mode1.matrix <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  # the rows are already the mode being projected
  .project(.data, match.arg(similarity))
}

#' @export
to_mode1.igraph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite_projection(.data)$proj1 else {
    if(!is_labelled(.data)){
      nind <- seq_len(net_nodes(.data))
      temp <- .data |> mutate_nodes(name = paste0("x", nind))
      out <- temp |> as_matrix() |> to_mode1(similarity) |> as_igraph() |> 
        join_nodes(object2 = temp, join_type = "left",
                   .by = dplyr::join_by(name)) |> 
        mutate_nodes(name = NULL)
    } else out <- as_igraph(to_mode1(as_matrix(.data), similarity)) |> 
        join_nodes(object2 = .data, join_type = "left",
                                     .by = dplyr::join_by(name))
    out |> mutate_nodes(type = NULL) |>
      select_nodes(dplyr::where(~ !all(is.na(.))))
  }
}

#' @export
to_mode1.tbl_graph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  similarity <- match.arg(similarity)
  out <- as_tidygraph(to_mode1(as_igraph(.data), similarity = similarity))
  if(similarity %in% .proj_signed){
    # an isolate gives NaN under several measures, and `NaN < 0` is NA,
    # which would otherwise leave the sign missing rather than positive
    wt <- tie_weights(out)
    out <- out |> mutate_ties(sign = dplyr::if_else(!is.na(wt) & wt < 0, -1, 1))
  }
  if(!is.null(net_name(.data))) out <- out |> 
      add_info(name = net_name(.data, prefix = "Projection of"))
  if(!is.null(layer_names(.data))) out <- out |> 
      add_info(ties = paste0("co-", layer_names(.data)))
  if(!is.null(mode_names(.data))) out <- out |> 
      add_info(nodes = mode_names(.data)[1],
               ties = paste0("co-", mode_names(.data)[2]))
  out |> .record_transformation("projection", paste0("mode 1 (", similarity, ")"))
}

#' @export
to_mode1.network <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
 as_network(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @export
to_mode1.data.frame <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  as_edgelist(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @rdname modif_project
#' @export
to_mode2 <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  # projecting a network that is already one-mode is a no-op
  if(!is_twomode(.data)) return(.data)
  UseMethod("to_mode2")
}

#' @export
to_mode2.default <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")){
  as_input(.data, to_mode2, similarity = similarity)
}

#' @export
to_mode2.stocnet <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")){
  similarity <- match.arg(similarity)
  # The tidygraph method is called directly rather than through `as_input()`,
  # which would pick this method again and recurse.
  out <- to_mode2(as_tidygraph(.project_changes(.data, which(node_is_mode(.data)))),
                  similarity = similarity)
  as_stocnet(out)
}

#' @export
to_mode2.matrix <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  # transposed so that the columns become the rows being projected
  .project(t(.data), match.arg(similarity))
}

#' @export
to_mode2.igraph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite_projection(.data)$proj2 else {
    if(!is_labelled(.data)){
      nind <- seq_len(net_nodes(.data))
      temp <- .data |> mutate_nodes(name = paste0("x", nind))
      out <- temp |> as_matrix() |> to_mode2(similarity) |> as_igraph() |> 
        join_nodes(object2 = temp, join_type = "left",
                   .by = dplyr::join_by(name)) |> 
        mutate_nodes(name = NULL)
    } else out <- as_igraph(to_mode2(as_matrix(.data), similarity)) |> 
        join_nodes(object2 = .data, join_type = "left",
                   .by = dplyr::join_by(name))
    out |> mutate_nodes(type = NULL) |>
      select_nodes(dplyr::where(~ !all(is.na(.))))
  }
}

#' @export
to_mode2.tbl_graph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  similarity <- match.arg(similarity)
  out <- as_tidygraph(to_mode2(as_igraph(.data), similarity = similarity))
  if(similarity %in% .proj_signed){
    # an isolate gives NaN under several measures, and `NaN < 0` is NA,
    # which would otherwise leave the sign missing rather than positive
    wt <- tie_weights(out)
    out <- out |> mutate_ties(sign = dplyr::if_else(!is.na(wt) & wt < 0, -1, 1))
  }
  if(!is.null(net_name(.data))) out <- out |> 
      add_info(name = net_name(.data, prefix = "Projection of"))
  if(!is.null(layer_names(.data))) out <- out |> 
      add_info(ties = paste0("co-", layer_names(.data)))
  if(!is.null(mode_names(.data))) out <- out |> 
      add_info(nodes = mode_names(.data)[2],
               ties = paste0("co-", mode_names(.data)[1]))
  out |> .record_transformation("projection", paste0("mode 2 (", similarity, ")"))
}

#' @export
to_mode2.network <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  as_network(to_mode2(as_tidygraph(.data), similarity)) 
}

#' @export
to_mode2.data.frame <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  as_edgelist(to_mode2(as_tidygraph(.data), similarity))
}

#' @rdname modif_project
#' @param mode Which mode to project the network to,
#'   either as an index, 1 or 2, or as the name of the mode.
#'   Mode 1 is the first node set, e.g. the rows, and is the default.
#'
#'   A name is matched against the names of the modes,
#'   which `mode_names()` returns and `add_info()` sets.
#'   The match ignores case, plurals, and any other words in the name,
#'   so that a network whose modes are named "women" and "social events"
#'   can be projected with either "social events", "events", or "event".
#'   Where a name matches both modes, as "events" would where they are named
#'   "social events" and "work events", the function reports the ambiguity
#'   and asks for a name that tells them apart, or an index.
#'
#'   The network must have two modes.
#'   Projecting one mode of a network of three or more would need a second
#'   mode to project through, which this function does not yet accept,
#'   so it reports the modes it found instead.
#' @examples
#' to_mode(ison_southern_women, 2)
#' to_mode(ison_southern_women, "events")
#' @export
to_mode <- function(.data, mode = 1,
                    similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka")) {
  # a network of three or more modes would otherwise fall through the one-mode
  # no-op below and be returned unchanged, since it is not two-mode either
  if(net_modes(.data) > 2)
    snet_abort("{.fun to_mode} projects a two-mode network,",
               "but this network has {net_modes(.data)} modes:",
               "{phrase(mode_names(.data))}.",
               "Projecting one of them would need a second mode to project",
               "through, which {.fun to_mode} does not yet accept.")
  # projecting a network that is already one-mode is a no-op
  if(!is_twomode(.data)) return(.data)
  similarity <- match.arg(similarity)
  if(.infer_mode(.data, mode) == 1L) to_mode1(.data, similarity) else
    to_mode2(.data, similarity)
}

#' @rdname modif_project
#' @importFrom igraph make_line_graph E
#' @examples
#' to_linegraph(ison_adolescents)
#' @export
to_linegraph <- function(.data) UseMethod("to_linegraph")

#' @export
to_linegraph.default <- function(.data){
  as_input(.data, to_linegraph)
}

#' @export
to_linegraph.igraph <- function(.data){
  out <- igraph::make_line_graph(.data)
  if(!is_labelled(.data)) {
    igraph::V(out)$name <- paste0(igraph::as_edgelist(.data)[,1], 
                                  ifelse(is_directed(.data), "->", "-"), 
                                  igraph::as_edgelist(.data)[,2])
  } else {
    igraph::V(out)$name <- attr(igraph::E(.data), "vnames")
  }
  igraph::V(out)$name <- gsub("\\|", "-", igraph::V(out)$name)
  out
}

.net_waves <- function(.data){
  .data <- manynet::expect_nodes(.data)
  tie_waves <- length(unique(manynet::tie_attribute(.data, "wave")))
  if(manynet::is_changing(.data)){
    chltime <- manynet::as_changelist(.data)$time
    chg_waves <- (max(chltime)+1) - max(min(chltime)-1, 0)
  } else chg_waves <- 1
  max(tie_waves, chg_waves)
}

#' @rdname modif_project
#' @section Hypergraphs: 
#'   This function projects one-mode or two-mode network data into hypergraph data,
#'   where ties can connect more than two nodes.
#'   The projection differs depending on whether the network is one-mode or two-mode,
#'   and the output can differ by class of the input/output data.
#'   
#'   For two-mode networks, the hyperedges are the nodes of the second mode, 
#'   and the nodes of the first mode are connected to them if they share a tie.
#'   In a 'stocnet' object, the hyperedges are stored in the `ties` data frame, 
#'   with the `from` column containing a list of nodes connected to each hyperedge.
#'   This is thus a compact representation of the hypergraph.
#'   igraph-like objects do not have a native representation of hyperedges, 
#'   so the output is a two-mode graph where the hyperedges are represented 
#'   as nodes of the second mode.
#'   
#'   For one-mode networks, the hyperedges are the maximal cliques of the network. 
#'   Again, while 'stocnet' objects can store the hyperedges in a compact form,
#'   igraph-like objects represent them as nodes of the second mode in a two-mode graph.
#'   
#' @export
to_hypergraph <- function(.data) UseMethod("to_hypergraph")

#' @export
to_hypergraph.default <- function(.data){
  as_input(.data, to_hypergraph)
}

#' @export
to_hypergraph.igraph <- function(.data){
  out <- .data
  if(!is_twomode(.data)){
    # Directions are ignored for maximal clique calculations anyway, but
    # converting explicitly avoids both the igraph warning and a segfault in
    # igraph 2.3.3 when max_cliques() is called on a directed graph after
    # any_multiple() (see https://github.com/igraph/rigraph):
    # any_multiple(g); max_cliques(g) # crashes with C stack overflow
    cl <- igraph::max_cliques(igraph::as_undirected(out, mode = "collapse"))
    if(is_labelled(.data)){
      lst <- stats::setNames(lapply(cl, names), LETTERS[seq_along(cl)])
    } else {
      lst <- stats::setNames(lapply(cl, as.integer), LETTERS[seq_along(cl)])
    }
    incidence <- data.frame(from = utils::stack(lst)$values,
                            to   = utils::stack(lst)$ind)
    out <- igraph::graph_from_data_frame(incidence, directed = FALSE)
  }
  out
}

#' @importFrom igraph maximal.cliques
#' @export
to_hypergraph.stocnet <- function(.data) {
  
  out <- .data
  if (is_twomode(.data)) {
    # Each 'to' node becomes a hyperedge
    out$ties <- out$ties  |> 
      dplyr::distinct(from, to) |> 
      dplyr::group_by(to) |> 
      dplyr::summarise(from = list(unique(from)), .groups = "drop") |> 
      dplyr::select(from, to, dplyr::everything())
  } else {
    # as_undirected() avoids an igraph 2.3.3 segfault; see to_hypergraph.igraph()
    cliques <- igraph::max_cliques(
      igraph::as_undirected(as_igraph(.data), mode = "collapse"))
    out$ties <- out$ties |> 
      dplyr::mutate(from = lapply(cliques, function(x) as.integer(x)),
                    to = LETTERS[seq_along(cliques)]) |> 
      dplyr::select(from, to, dplyr::everything())
  }
  out
}


# Concept lattices ####

#' Modifying networks into concept lattices
#' @name modif_concepts
#' @description
#'   `to_concepts()` projects a network into its concept lattice,
#'   also known as its Galois lattice.
#'   Each node of the lattice is a concept:
#'   a set of nodes together with the set of affiliations they all share,
#'   where neither set can grow without the other shrinking.
#'   Each tie points from a concept to a concept directly beneath it,
#'   one that holds fewer nodes but more shared affiliations.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods("to_concepts")
#'   ```
#'
#'   In a two-mode network, the first node set (e.g. the rows) supplies the
#'   concepts' members, their _extent_,
#'   and the second node set (e.g. the columns) their shared affiliations,
#'   their _intent_.
#'   The lattice is read from top to bottom.
#'   The concept at the top holds every node of the first set,
#'   and those affiliations, if any, that all of them share.
#'   The concept at the bottom holds every affiliation,
#'   and those nodes, if any, that have all of them.
#'   Moving down the lattice, each concept holds fewer nodes,
#'   who share more affiliations.
#'   Two nodes are in a concept together exactly where they share every one of
#'   its affiliations, and so the lattice records every membership of the
#'   original network.
#'
#'   A one-mode network is treated as the incidence of its nodes on their
#'   out-neighbours.
#'   Each concept is then a set of nodes together with every node
#'   that all of them send ties to,
#'   a maximal biclique of senders and receivers.
#'
#'   A valued or signed network is dichotomised first,
#'   so that every positive value counts as a tie,
#'   with a warning.
#' @section Labels:
#'   Each concept is named with a "reduced" label,
#'   listing only those nodes and affiliations that first appear at it.
#'   A node of the first set is named at the lowest concept whose extent
#'   holds it, and it is in the extent of every concept above that one too.
#'   An affiliation, written in braces, is named at the highest concept whose
#'   intent holds it, and it is in the intent of every concept below that one.
#'   A concept at which nothing first appears is named "C" and its position,
#'   counting from the top.
#'   Each label thus appears exactly once,
#'   and the full sets are kept in the `extent` and `intent` node attributes,
#'   with their sizes in `extent_size` and `intent_size`.
#' @section Projection without loss:
#'   Projecting a two-mode network with `to_mode1()` or `to_mode2()` records
#'   how much two nodes share,
#'   but not what they share or with whom else they share it.
#'   The concept lattice keeps both.
#'   Freeman and White (1993) propose it for this reason,
#'   and Freeman (2003) uses it to find the groups among the Southern Women.
#'   It grows quickly with the density of the network, however,
#'   and a lattice of more than a few dozen concepts is hard to read.
#'   Where a network has more than 1000 concepts,
#'   the function warns about this.
#' @template param_data
#' @template fam_modif
#' @family projections
#' @concept Galois lattice
#' @concept concept lattice
#' @concept formal concept analysis
#' @references
#' ## On Galois lattices
#'   Freeman, Linton C., and Douglas R. White. 1993.
#'   "Using Galois lattices to represent network data".
#'   _Sociological Methodology_ 23: 127-145.
#'   \doi{10.2307/271008}
#'
#'   Freeman, Linton C. 2003.
#'   "Finding social groups: A meta-analysis of the southern women data".
#'   In _Dynamic Social Network Modeling and Analysis_, 39-97.
#'   Washington, DC: The National Academies Press.
#'
#' ## On formal concept analysis
#'   Wille, Rudolf. 1982.
#'   "Restructuring lattice theory: An approach based on hierarchies of concepts".
#'   In _Ordered Sets_, 445-470. Dordrecht: Reidel.
#'   \doi{10.1007/978-94-009-7798-3_15}
#'
#'   Ganter, Bernhard, and Rudolf Wille. 1999.
#'   _Formal Concept Analysis: Mathematical Foundations_.
#'   Berlin: Springer.
#'   \doi{10.1007/978-3-642-59830-2}
#' @examples
#' to_concepts(ison_southern_women)
#' # autograph::graphr(to_concepts(ison_southern_women), "layered")
#' @export
to_concepts <- function(.data) UseMethod("to_concepts")

#' @export
to_concepts.default <- function(.data){
  as_input(.data, to_concepts)
}

#' @export
to_concepts.tbl_graph <- function(.data){
  lat <- .concept_lattice(.data)
  nodes <- dplyr::tibble(name = lat$names,
                         extent = lat$extent,
                         intent = lat$intent,
                         extent_size = lengths(lat$extent),
                         intent_size = lengths(lat$intent))
  out <- tidygraph::tbl_graph(nodes = nodes, edges = lat$ties, directed = TRUE)
  if(!is.null(net_name(.data))) out <- out |>
      add_info(name = net_name(.data, prefix = "Concept lattice of"))
  out |> add_info(nodes = "concepts") |>
    .record_transformation("projection", "concept lattice")
}

#' @export
to_concepts.igraph <- function(.data){
  as_igraph(to_concepts(as_tidygraph(.data)))
}

#' @export
to_concepts.stocnet <- function(.data){
  as_stocnet(to_concepts(as_tidygraph(.data)))
}

#' @export
to_concepts.network <- function(.data){
  as_network(to_concepts(as_tidygraph(.data)))
}

#' @export
to_concepts.data.frame <- function(.data){
  as_edgelist(to_concepts(as_tidygraph(.data)))
}

#' @export
to_concepts.matrix <- function(.data){
  lat <- .concept_lattice(.data)
  out <- matrix(0, length(lat$names), length(lat$names),
                dimnames = list(lat$names, lat$names))
  out[as.matrix(lat$ties)] <- 1
  out
}


# Helper functions ------------------

# Resolves the `mode` argument of `to_mode()` to the index 1 or 2.
# A name is matched loosely, since the point of naming a mode is that the
# user need not recall how it was written when the network was collected.
.infer_mode <- function(.data, mode){
  # stated in terms of the network rather than as a literal 1 or 2, so that a
  # third mode needs no change here once `to_mode()` can project one
  nmodes <- net_modes(.data)
  if(is.numeric(mode)){
    if(length(mode) != 1 || is.na(mode) || !mode %in% seq_len(nmodes))
      snet_abort("{.arg mode} must be {.or {seq_len(nmodes)}},",
                 "or the name of a mode.")
    return(as.integer(mode))
  }
  if(!is.character(mode) || length(mode) != 1)
    snet_abort("{.arg mode} must be a single index or a single mode name.")
  nms <- mode_names(.data)
  if(length(nms) != nmodes)
    snet_abort("The modes of this network are not named.",
               "Please select a mode by index, {.or {seq_len(nmodes)}},",
               "or name the modes with {.fun add_info}.")
  matches <- which(vapply(nms, .mode_matches, logical(1), query = mode))
  # a word can name more than one mode, as "events" does where the modes are
  # "social events" and "work events", so say so rather than report no match
  if(length(matches) > 1)
    snet_abort("{.arg mode} matches more than one mode:",
               "{phrase(nms[matches])}.",
               "Please give a name that tells them apart, or an index.")
  if(length(matches) == 0)
    snet_abort("{.arg mode} must be an index,",
               "or match {.or {nms}}.")
  as.integer(matches)
}

# Does the query name this mode? The whole name matches, and so does any one
# of the words in it, ignoring case and plurals.
.mode_matches <- function(name, query){
  norm <- function(x) singularize(tolower(x))
  words <- unlist(strsplit(name, "[^[:alnum:]]+"))
  words <- words[nzchar(words)]
  norm(query) %in% norm(c(name, words))
}

# The measures `to_mode1()` and `to_mode2()` accept, in the order they are
# offered to the user: the five that predate this list first, so that
# partial matching resolves as it always has.
.proj_measures <- c("count","jaccard","rand","pearson","yule",
                    "match","overlap","crossmin","maxcrossmin","sqdiff",
                    "covariance","bonacich","ochiai","ochiai2",
                    "czekanowski","sokalsneath","hamann","rogerstanimoto",
                    "euclidean","manhattan","hamming","cosine",
                    "spearman","kendall","ruzicka")

# Those that can return a negative value, and so need the projection's ties
# labelled with a sign as well as a weight.
.proj_signed <- c("pearson","yule","covariance","hamann",
                  "spearman","kendall","cosine")

# Those defined for binary data only, since they count cells in which both,
# one, or neither node is present. A valued network is dichotomised for these.
.proj_binary <- c("jaccard","rand","hamann","rogerstanimoto","czekanowski",
                  "ochiai","ochiai2","sokalsneath","yule","bonacich",
                  "hamming")

# The co-occurrence counts: cells where both nodes are present (a), where just
# one is (b and c), and where neither is (d). Computed only where the chosen
# measure needs them, since each is a matrix product.
.proj_abcd <- function(X){
  a <- X %*% t(X)
  b <- X %*% (1 - t(X))
  c <- (1 - X) %*% t(X)
  list(a = a, b = b, c = c, d = ncol(X) - a - b - c)
}

# Counts the cells in which two nodes hold exactly the same value, including
# joint absence. Summing an indicator product over the distinct values is
# equivalent to comparing every pair of rows, but avoids the pairwise loop.
.proj_match <- function(X){
  out <- matrix(0, nrow(X), nrow(X))
  for(v in sort(unique(as.vector(X)))){
    I <- (X == v) * 1
    out <- out + I %*% t(I)
  }
  out
}

# Sums the smaller of each pair of tie values, using the identity
# min(x,y) = (x + y - |x - y|)/2 so that the sum over the other mode is the
# row totals less the Manhattan distance.
.proj_crossmin <- function(X){
  R <- rowSums(X)
  (outer(R, R, "+") - as.matrix(stats::dist(X, method = "manhattan")))/2
}

# Takes the largest of those minima rather than their sum. The largest value
# v for which both nodes reach v is found by sweeping the distinct values
# upwards, since max(min(x_ik, x_jk)) >= v exactly when some k has both at v.
.proj_maxcrossmin <- function(X){
  out <- matrix(0, nrow(X), nrow(X))
  for(v in sort(unique(as.vector(X)))){
    if(v <= 0) next
    I <- (X >= v) * 1
    out[(I %*% t(I)) > 0] <- v
  }
  out
}

# Projects the matrix `X`, whose rows are the mode being projected, so that
# `to_mode2()` can share this by passing the transpose.
.project <- function(X, similarity){
  if(similarity %in% .proj_binary && any(X != 0 & X != 1, na.rm = TRUE)){
    snet_warn(paste0("The {.val {similarity}} measure is defined for binary ",
                     "data only, so tie values have been dichotomised at 0. ",
                     "Consider {.val ruzicka}, {.val crossmin}, or ",
                     "{.val overlap} to retain them."))
    X <- (X > 0) * 1
  }
  out <- switch(similarity,
                "count" = X %*% t(X),
                "pearson" = stats::cor(t(X)),
                "covariance" = stats::cov(t(X)),
                "match" = .proj_match(X),
                "crossmin" = .proj_crossmin(X),
                "maxcrossmin" = .proj_maxcrossmin(X),
                # inverted, so that larger means more alike as for every other
                # measure here; UCINET's raw sum of squared differences is 1/x - 1
                "sqdiff" = 1/(1 + as.matrix(stats::dist(X))^2),
                # the distances are inverted on the same principle, and their
                # raw values are likewise recoverable as 1/x - 1
                "euclidean" = 1/(1 + as.matrix(stats::dist(X))),
                "manhattan" = ,
                # `hamming` reaches here already dichotomised, where the
                # Manhattan distance counts the cells the two rows differ in
                "hamming" = 1/(1 + as.matrix(stats::dist(X,
                                                     method = "manhattan"))),
                "spearman" = stats::cor(t(X), method = "spearman"),
                "kendall" = stats::cor(t(X), method = "kendall"),
                # the cosine of the angle between two rows. A row of zeroes has
                # no direction, so its length is left at 1 to give 0 rather than
                # NaN against every other row
                "cosine" = {
                  len <- sqrt(rowSums(X^2))
                  len[len == 0] <- 1
                  (X %*% t(X))/outer(len, len)
                },
                # the Szymkiewicz-Simpson coefficient: the shared tie strength
                # over the smaller of the two row totals. xUCINET takes the
                # cross-product as the numerator, which agrees for binary data,
                # but for valued data exceeds 1 and is no longer the coefficient
                "overlap" = {
                  R <- rowSums(X)
                  .proj_crossmin(X)/outer(R, R, pmin)
                },
                # Ruzicka's weighted Jaccard: the shared tie strength over the
                # combined tie strength, which for binary data is "jaccard"
                "ruzicka" = {
                  R <- rowSums(X)
                  cm <- .proj_crossmin(X)
                  cm/(outer(R, R, "+") - cm)
                },
                # the remaining measures are all functions of the counts
                {
                  cnt <- .proj_abcd(X)
                  a <- cnt$a; b <- cnt$b; c <- cnt$c; d <- cnt$d
                  switch(similarity,
                         "jaccard" = a/(a + b + c),
                         "czekanowski" = 2*a/(2*a + b + c),
                         "sokalsneath" = a/(a + 2*(b + c)),
                         "rand" = (a + d)/(a + b + c + d),
                         "hamann" = (a - (b + c) + d)/(a + b + c + d),
                         "rogerstanimoto" = (a + d)/(a + 2*(b + c) + d),
                         "ochiai" = a/sqrt((a + b)*(a + c)),
                         "ochiai2" = a*d/sqrt((a + b)*(a + c)*(d + b)*(d + c)),
                         "yule" = (a*d - b*c)/(a*d + b*c),
                         # algebraically identical to Bonacich's
                         # (X - sqrt(XY))/(X - Y) for X = ad and Y = bc,
                         # including its X == Y case, but without the loop
                         "bonacich" = sqrt(a*d)/(sqrt(a*d) + sqrt(b*c)))
                })
  dimnames(out) <- list(rownames(X), rownames(X))
  diag(out) <- 0
  out
}

# Enumerates the concepts of a network and the covering relation among them,
# for `to_concepts()`. The concepts are returned from the top down: by extent
# size, largest first, and then by intent size, smallest first.
.concept_lattice <- function(.data){
  X <- as_matrix(.data)
  # a cognitive social structure is a matrix for each reporter
  if(length(dim(X)) != 2)
    snet_abort("{.fn to_concepts} needs a single network,",
               "but this one holds a report from each of several reporters.",
               "Select one report with {.fn to_reporter},",
               "or combine them with {.fn to_aggregated}, first.")
  if(is_twomode(.data)){
    rows <- rownames(X) %||% as.character(seq_len(nrow(X)))
    cols <- colnames(X) %||% as.character(nrow(X) + seq_len(ncol(X)))
  } else {
    snet_info("This network is one-mode, so each concept is a set of nodes",
              "together with the nodes that all of them send ties to.")
    rows <- rownames(X) %||% as.character(seq_len(nrow(X)))
    cols <- colnames(X) %||% rows
  }
  if(any(is.na(X) | (X != 0 & X != 1)))
    snet_warn(paste0("A concept lattice is defined for binary data only, ",
                     "so tie values have been dichotomised at 0, ",
                     "with missing values counted as absent."))
  X <- !is.na(X) & X > 0
  I <- .concept_intents(X)
  # An object holds every attribute of a concept's intent exactly where the
  # number of those attributes it holds is the size of the intent.
  E <- (X * 1) %*% t(I * 1) == 
    matrix(rowSums(I), nrow(X), nrow(I), byrow = TRUE)
  ord <- order(-colSums(E), rowSums(I))
  I <- I[ord, , drop = FALSE]
  E <- E[, ord, drop = FALSE]
  if(nrow(I) > 1000)
    snet_warn("This network has {nrow(I)} concepts,",
              "which will be hard to read as a lattice.")
  list(names = .concept_labels(X, E, I, rows, cols),
       extent = lapply(seq_len(ncol(E)), function(k) rows[E[, k]]),
       intent = lapply(seq_len(nrow(I)), function(k) cols[I[k, ]]),
       ties = .concept_covers(X, E, I))
}

# Every intent of a binary incidence matrix is an intersection of some of its
# rows, or else the set of all attributes, which is the intent of the empty
# extent. Adding the rows one at a time, and intersecting each with every
# intent found so far, therefore finds them all (Norris 1978).
.concept_intents <- function(X){
  I <- matrix(TRUE, 1, ncol(X))
  for(i in seq_len(nrow(X))){
    new <- I & matrix(X[i, ], nrow(I), ncol(X), byrow = TRUE)
    I <- unique(rbind(I, new))
  }
  I
}

# Keys each column of the logical matrix `M`, read as a set of its rows, so
# that two columns share a key exactly where they hold the same set. Given `Y`
# too, it keys the intersection of every column of `M` with every column of
# `Y`, as a matrix product, in the column-major order of an
# `ncol(M)` by `ncol(Y)` matrix. Each run of 30 rows is coded as the integer
# its bits spell, which a double holds exactly.
.set_keys <- function(M, Y = NULL){
  runs <- split(seq_len(nrow(M)), (seq_len(nrow(M)) - 1) %/% 30)
  codes <- lapply(runs, function(r){
    bits <- M[r, , drop = FALSE] * 2^(seq_along(r) - 1)
    code <- if(is.null(Y)) colSums(bits) else
      crossprod(bits, Y[r, , drop = FALSE] * 1)
    as.integer(code)
  })
  if(!length(codes)) return(rep("", ncol(M) * (if(is.null(Y)) 1 else ncol(Y))))
  do.call(paste, c(unname(codes), sep = "."))
}

# The ties from each concept to those directly beneath it (Lindig 2000).
# Removing one more attribute j from a concept's extent A gives A and j's
# extent in common, which is itself the extent of some concept d beneath it.
# That d is directly beneath exactly where every attribute d adds to the
# concept's intent leads to it, so the count of attributes leading to d is
# the difference in the two intents' sizes. This takes one matrix product
# rather than comparing every pair of concepts.
.concept_covers <- function(X, E, I){
  k <- ncol(E)
  d <- match(.set_keys(E, X), .set_keys(E))
  free <- !as.vector(I)
  pairs <- (rep(seq_len(k), ncol(X))[free] - 1) * k + d[free]
  runs <- rle(sort(pairs))
  from <- (runs$values - 1) %/% k + 1
  to <- (runs$values - 1) %% k + 1
  size <- rowSums(I)
  keep <- runs$lengths == size[to] - size[from]
  data.frame(from = from[keep], to = to[keep])
}

# The reduced labels: each object at the lowest concept whose extent holds it,
# which is the concept whose intent is that object's row, and each attribute,
# in braces, at the highest concept whose intent holds it, which is the concept
# whose extent is that attribute's column.
.concept_labels <- function(X, E, I, rows, cols){
  at_obj <- match(.set_keys(t(X)), .set_keys(t(I)))
  at_att <- match(.set_keys(X), .set_keys(E))
  out <- vapply(seq_len(nrow(I)), function(k){
    obj <- rows[at_obj == k]
    att <- cols[at_att == k]
    paste(c(if(length(obj)) paste(obj, collapse = ", "),
            if(length(att)) paste0("{", paste(att, collapse = ", "), "}")),
          collapse = " ")
  }, character(1))
  empty <- out == ""
  out[empty] <- paste0("C", which(empty))
  out
}
