# Projecting ####

#' Modifying networks projection
#' @name modif_project
#' @description
#'   These functions project network data:
#'
#'   - `to_mode()` projects a network onto one of its modes.
#'   Two nodes of that mode get a tie when they have ties to the same nodes
#'   of another mode.
#'   Give the mode as an index or as a name.
#'   It also projects a network of three or more modes.
#'   - `to_mode1()` projects a two-mode network onto its first mode,
#'   for example the rows.
#'   - `to_mode2()` projects a two-mode network onto its second mode,
#'   for example the columns.
#'   - `to_linegraph()` makes the line graph of a network.
#'   Each tie becomes a node.
#'   Two of these nodes get a tie when their ties share a node.
#'   - `to_hypergraph()` makes a hypergraph,
#'   where one tie can connect more than two nodes.
#'   - `to_simplicial()` makes a simplicial complex,
#'   such as the maximal cliques of a one-mode network.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(mode[0-9]|linegraph|hypergraph|simplicial)"))
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
#' | | Other similarity measures | 31 in all, see `similarity` | no | code manually |
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
#' @param similarity How to measure how alike two nodes are.
#'   The default is "count".
#'   Each measure compares the ties of two nodes to the other mode.
#'
#'   The measures are in groups below.
#'   The measures in one group put the pairs of nodes in the same order,
#'   and differ only in their scale.
#'   So the choice of group is more important than the choice in a group.
#'
#'   Measures for valued and binary data:
#'
#'   - "count" counts the shared ties, as \eqn{XX'}.
#'   For valued data, it adds the products of the tie weights.
#'   It is the default, and the fastest.
#'   - "match" counts the nodes of the other mode where the two nodes
#'   have the same value.
#'   This includes the nodes that neither of them has a tie to.
#'   For binary data, it is "rand" times the number of nodes
#'   in the other mode.
#'   Use it when the level of each tie is important.
#'   - "crossmin" adds the smaller of the two weights for each shared node.
#'   It shows what two nodes can give together.
#'   For binary data, it is the same as "count".
#'   - "overlap" divides "crossmin" by the smaller of the two nodes' totals.
#'   This is the Szymkiewicz-Simpson coefficient.
#'   For data with no negative values, it is from 0 to 1.
#'   Use it when one node is much more active than the other.
#'   For valued data, xUCINET divides the count and not "crossmin",
#'   so its values can be more than 1.
#'   - "ruzicka" divides "crossmin" by the sum of the two nodes' totals
#'   minus "crossmin".
#'   This is Ruzicka's weighted Jaccard coefficient, from 0 to 1.
#'   For binary data, it is the same as "jaccard".
#'   Use it when you want "jaccard" but must keep the tie weights.
#'   - "maxcrossmin" takes the largest of these smaller weights,
#'   not their sum.
#'   So one strong shared tie is enough.
#'   For binary data, it only shows if two nodes share any tie.
#'   - "sqdiff" adds the squared differences of the weights, as \eqn{d}.
#'   It gives \eqn{1/(1+d)}, so that a larger value means more alike.
#'   Two nodes with the same ties get 1.
#'   To get \eqn{d} back, calculate \eqn{1/x - 1}.
#'   - "pearson" is the correlation of the two nodes' ties.
#'   "covariance" is the same, but not standardised.
#'   Use "covariance" when the variance of the ties is important.
#'   - "spearman" and "kendall" are rank correlations.
#'   Use them when the order of the weights is reliable,
#'   but the spacing is not.
#'   An example is ordinal ratings.
#'   - "cosine" is the cosine of the angle between the two nodes' ties.
#'   It is like "pearson", but it does not centre the ties first.
#'   So two nodes are alike when their ties are proportional.
#'   For binary data, it is the same as "ochiai".
#'   For a node with no ties, "cosine" gives 0,
#'   but "ochiai" divides by zero.
#'   - "euclidean" and "manhattan" use the distance \eqn{d} between
#'   the two nodes' ties, and give \eqn{1/(1+d)}, as "sqdiff" does.
#'   "manhattan" adds the absolute differences.
#'   "euclidean" gives more weight to one large difference
#'   than to many small differences.
#'   To get \eqn{d} back, calculate \eqn{1/x - 1}.
#'
#'   Measures for binary data only.
#'   These measures change valued data to binary data first,
#'   and give a warning.
#'   For two nodes, \eqn{a} is the number of nodes in the other mode
#'   that both have a tie to.
#'   \eqn{b} and \eqn{c} are the numbers that only one of them has a tie to.
#'   \eqn{d} is the number that neither has a tie to.
#'
#'   - Measures that do not use \eqn{d}.
#'   Use them when two missing ties tell you nothing,
#'   for example in a sparse network with many events.
#'   "jaccard" is \eqn{a/(a+b+c)}.
#'   "czekanowski" (also Dice or Sorensen) is \eqn{2a/(2a+b+c)},
#'   which gives more weight to \eqn{a}.
#'   "sokalsneath" is \eqn{a/(a+2(b+c))},
#'   which gives more weight to \eqn{b} and \eqn{c}.
#'   For valued data, use "ruzicka" in place of "jaccard".
#'   - Measures that count \eqn{d} as a match.
#'   Use them when a missing tie tells you as much as a tie.
#'   "rand" (simple matching) is \eqn{(a+d)/(a+b+c+d)}.
#'   "hamann" is \eqn{((a+d)-(b+c))/(a+b+c+d)}, from -1 to 1.
#'   It is positive when there are more matches than differences.
#'   "rogerstanimoto" is \eqn{(a+d)/(a+2(b+c)+d)}.
#'   - Measures from the odds ratio \eqn{ad/bc}.
#'   "yule" is Yule's Q, \eqn{(ad-bc)/(ad+bc)}.
#'   "bonacich" is \eqn{\sqrt{ad}/(\sqrt{ad}+\sqrt{bc})},
#'   which is Yule's Y from 0 to 1.
#'   Both give 1 when \eqn{b} or \eqn{c} is 0.
#'   - Geometric measures.
#'   "ochiai" is \eqn{a/\sqrt{(a+b)(a+c)}}, the cosine for binary data.
#'   "ochiai2" (Sokal and Sneath's fifth measure) is
#'   \eqn{ad/\sqrt{(a+b)(a+c)(d+b)(d+c)}}, which also uses \eqn{d}.
#'   These two do not put pairs in the same order as any other group,
#'   so try them too.
#'   - "hamming" counts the differences, \eqn{b+c}, as the Hamming distance.
#'   It gives \eqn{1/(1+b+c)}.
#'   It puts pairs in the same order as "rand".
#'
#'   Measures that change "count" for how active each node is:
#'
#'   - "activity" divides "count" by the average number of ties of the
#'   two nodes.
#'   For binary data, it is the same as "czekanowski".
#'   - "association" divides "count" by the count that chance would give.
#'   A value above 1 means that two nodes share more than chance would give.
#'   This is the association strength of van Eck and Waltman (2009).
#'   - "fractional" shares each node of the other mode among its members.
#'   For example, a work with \eqn{n} authors adds \eqn{1/(n-1)}
#'   to each pair of its authors.
#'   Each author then gets a total of 1 from each work.
#'   A work with one author adds nothing.
#'   This is the fractional counting of Perianes-Rodriguez et al. (2016).
#'
#'   Measures that read the sign of each tie:
#'   "agreement", "disagreement", and "signed".
#'   See the section on weighted and signed networks below.
#'
#'   These six measures count every tie,
#'   also when a node has more than one tie to the same node.
#' @section Weighted and signed networks:
#'   Most measures read the weight of a tie as how strong the tie is.
#'   The measures for binary data change each weight to 1 first,
#'   and give a warning.
#'
#'   Three measures read the sign of a tie instead.
#'   The sign shows if a node is for (1) or against (-1)
#'   a node of the other mode.
#'   For example, in `irps_nuclear`,
#'   each speaker supports or opposes each concept.
#'
#'   - "agreement" counts the pairs of ties with the same sign.
#'   Two speakers who both support a concept agree.
#'   Two speakers who both oppose a concept also agree.
#'   - "disagreement" counts the pairs of ties with opposite signs.
#'   - "signed" is "agreement" minus "disagreement".
#'   The result is a signed network.
#'   A negative tie means that two nodes disagree more often than they agree.
#'
#'   If a speaker makes two statements about the same concept,
#'   both statements count.
#'   To keep agreement and disagreement in one network,
#'   join the two networks with `from_layers()`.
#'
#'   The weights can also be on a scale, for example from -2 to 2.
#'   Then each pair of ties adds a part of 1 to "agreement",
#'   and the rest to "disagreement".
#'   Two ties with the same value add 1 to "agreement".
#'   Two ties at the two ends of the scale add 1 to "disagreement".
#'   In general, "disagreement" adds \eqn{|s_1 - s_2|/r},
#'   where \eqn{r} is the range of the scale.
#'
#'   To weight a two-mode network of documents and terms
#'   before the projection, use `to_normalised(rule = "tfidf")`.
#' @seealso [to_proximity()], which uses the same measures on a one-mode
#'   network.
#'   It compares nodes by their ties to each other,
#'   not by their ties to another mode.
#'
#'   [to_cosine()], which gives the cosine over the columns of a matrix
#'   without a projection.
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
#'
#'   van Eck, Nees Jan, and Ludo Waltman. 2009.
#'   "How to normalize cooccurrence data? An analysis of some well-known
#'   similarity measures".
#'   _Journal of the American Society for Information Science and Technology_
#'   60(8): 1635-1651.
#'   \doi{10.1002/asi.21075}
#'
#'   Perianes-Rodriguez, Antonio, Ludo Waltman, and Nees Jan van Eck. 2016.
#'   "Constructing bibliometric networks: A comparison between full and
#'   fractional counting".
#'   _Journal of Informetrics_ 10(4): 1178-1195.
#'   \doi{10.1016/j.joi.2016.10.006}
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  # a hypergraph is projected through its hyperedges, as a two-mode network
  # is through the nodes of its other mode
  if(inherits(.data, "stocnet") && is_hypergraph(.data))
    return(to_mode1(.hyper_to_twomode(.data), similarity))
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")){
  as_input(.data, to_mode1, similarity = similarity)
}

#' @export
to_mode1.stocnet <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")){
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  # the rows are already the mode being projected
  .project(.data, match.arg(similarity))
}

#' @export
to_mode1.igraph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite_projection(.data)$proj1 else {
    if(!is_labelled(.data)){
      nind <- seq_len(net_nodes(.data))
      temp <- .data |> mutate_nodes(name = paste0("x", nind))
      out <- temp |> .project_graph(1L, similarity) |> as_igraph() |> 
        join_nodes(object2 = temp, join_type = "left",
                   .by = dplyr::join_by(name)) |> 
        mutate_nodes(name = NULL)
    } else out <- as_igraph(.project_graph(.data, 1L, similarity)) |> 
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
 as_network(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @export
to_mode1.data.frame <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  as_edgelist(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @rdname modif_project
#' @export
to_mode2 <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  # a hypergraph is projected through its hyperedges, as a two-mode network
  # is through the nodes of its other mode
  if(inherits(.data, "stocnet") && is_hypergraph(.data))
    return(to_mode2(.hyper_to_twomode(.data), similarity))
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")){
  as_input(.data, to_mode2, similarity = similarity)
}

#' @export
to_mode2.stocnet <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")){
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  # transposed so that the columns become the rows being projected
  .project(t(.data), match.arg(similarity))
}

#' @export
to_mode2.igraph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite_projection(.data)$proj2 else {
    if(!is_labelled(.data)){
      nind <- seq_len(net_nodes(.data))
      temp <- .data |> mutate_nodes(name = paste0("x", nind))
      out <- temp |> .project_graph(2L, similarity) |> as_igraph() |> 
        join_nodes(object2 = temp, join_type = "left",
                   .by = dplyr::join_by(name)) |> 
        mutate_nodes(name = NULL)
    } else out <- as_igraph(.project_graph(.data, 2L, similarity)) |> 
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
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
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  as_network(to_mode2(as_tidygraph(.data), similarity)) 
}

#' @export
to_mode2.data.frame <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional")) {
  as_edgelist(to_mode2(as_tidygraph(.data), similarity))
}

#' @rdname modif_project
#' @param mode The mode to project the network onto.
#'   Give an index, such as 1 or 2, or the name of the mode.
#'   For a two-mode network, `FALSE` is the first mode and `TRUE` the second.
#'   The default is 1, the first mode, for example the rows.
#'
#'   A name is compared with the names of the modes from `mode_names()`.
#'   Set these names with `add_info()`.
#'   The comparison ignores case, plurals, and other words in the name.
#'   For example, if the modes are "women" and "social events",
#'   then "social events", "events", and "event" all give the second mode.
#'   If a name matches more than one mode, the function stops.
#'   Then give a name that tells the modes apart, or give an index.
#'
#' @param through The modes to project `mode` through,
#'   as indices or names, as for `mode`.
#'   The default is all the other modes.
#'   This makes a difference only for a network of three or more modes.
#'   The nodes of all the modes in `through` then count as one mode.
#'   Ties within one mode, and ties to other modes, are not used.
#'
#'   For example, take a network of authors, works, and keywords.
#'   `to_mode(.data, "authors", through = "works")` ties two authors
#'   who wrote a work together.
#'   `to_mode(.data, "works")` ties two works that share an author
#'   or a keyword.
#'   To keep two modes, for example to tie authors to keywords,
#'   use `to_twomode(.data, through = "works")`.
#' @examples
#' to_mode(ison_southern_women, 2)
#' to_mode(ison_southern_women, "events")
#' to_mode(irps_nuclear, "speakers", "signed")
#' @export
to_mode <- function(.data, mode = 1,
                    similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto", "euclidean", "manhattan",
                                 "hamming", "cosine", "spearman", "kendall", "ruzicka",
                                 "agreement", "disagreement", "signed",
                                 "activity", "association", "fractional"),
                    through = NULL) {
  similarity <- match.arg(similarity)
  # a hypergraph is projected through its hyperedges, which are its second
  # mode here, so that either can be named or numbered as a mode can
  if(inherits(.data, "stocnet") && is_hypergraph(.data))
    .data <- .hyper_to_twomode(.data)
  # a network of three or more modes would otherwise fall through the one-mode
  # no-op below and be returned unchanged, since it is not two-mode either
  if(net_modes(.data) > 2)
    return(.project_through(.data, mode, through, similarity))
  # projecting a network that is already one-mode is a no-op
  if(!is_twomode(.data)) return(.data)
  if(!is.null(through) &&
     any(vapply(through, function(m) .infer_mode(.data, m), integer(1)) ==
         .infer_mode(.data, mode)))
    snet_abort("{.arg through} must name modes other than {.arg mode}.")
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
#'   In a hypergraph, one tie can connect any number of nodes.
#'   This tie is a hyperedge.
#'   Examples are the people at a meeting, or the authors of a paper.
#'   `to_hypergraph()` returns the class it was given.
#'   Nothing is lost.
#'
#'   For a two-mode network, each node of the second mode becomes
#'   a hyperedge.
#'   The hyperedge connects the nodes of the first mode
#'   that have ties to that node.
#'   The attributes of the second-mode nodes become attributes
#'   of the hyperedges.
#'   The attributes of the ties, such as weights, stay with each member.
#'   `to_twomode()` gives the two-mode network back.
#'
#'   For a one-mode network, each tie becomes a hyperedge of its two nodes,
#'   with its attributes.
#'   If the ties are directed, the hyperedges are directed too.
#'   The sender is on the 'from' side, and the receiver on the 'to' side.
#'   `to_mode()` gives the one-mode network back.
#'
#'   A 'stocnet' object keeps each hyperedge as one row of its ties.
#'   The `with` column of that row lists the members of the hyperedge
#'   (see the Hypergraphs section of [make_stocnet()]).
#'   Other classes have no hyperedges.
#'   They keep each hyperedge as a node of a second mode, with a mark.
#'   With this mark, [is_hypergraph()] finds the hypergraph,
#'   and coercion back to a 'stocnet' object gives the hyperedges back.
#'   A 'network' object is different, because `{network}` has hyperedges.
#' @examples
#' to_hypergraph(ison_southern_women)
#' @export
to_hypergraph <- function(.data) UseMethod("to_hypergraph")

#' @export
to_hypergraph.default <- function(.data){
  as_input(.data, to_hypergraph)
}

#' @export
to_hypergraph.igraph <- function(.data){
  if(is_hypergraph(.data)) return(.data)
  as_igraph(to_hypergraph(as_stocnet(.data)))
}

#' @export
to_hypergraph.tbl_graph <- function(.data){
  if(is_hypergraph(.data)) return(.data)
  as_tidygraph(to_hypergraph(as_stocnet(.data)))
}

#' @export
to_hypergraph.network <- function(.data){
  if(is_hypergraph(.data)) return(.data)
  as_network(to_hypergraph(as_stocnet(.data)))
}

#' @export
to_hypergraph.stocnet <- function(.data) {
  if(is_hypergraph(.data)) return(.data)
  if(net_modes(.data) > 2)
    snet_abort("{.fun to_hypergraph} takes the nodes of the second of two",
               "modes as hyperedges, but this network has",
               "{net_modes(.data)} modes: {phrase(mode_names(.data))}.")
  if(is_twomode(.data))
    return(.twomode_to_hyper(.data, as.logical(node_is_mode(.data))))
  .dyads_to_hyper(.data)
}

# Every tie of a network as a hyperedge: of its two nodes where it is a dyad,
# and as it is where it is a hyperedge already.
.dyads_to_hyper <- function(.data){
  out <- .data
  ties <- out$ties
  if(is.null(ties)) ties <- dplyr::tibble(from = integer(0), to = integer(0))
  hyper <- .hyper_rows(ties)
  if(!"with" %in% names(ties)) ties$with <- vector("list", nrow(ties))
  # a loop ties one node, so its hyperedge has the one member
  ties$with[!hyper] <- Map(function(f, t) unique(c(f, t)),
                           ties$from[!hyper], ties$to[!hyper])
  # The dyads have sides where they are directed, which the hyperedges of
  # the network can only then have too.
  if(is_directed(.data) && (!any(hyper) || "with_direction" %in% names(ties))){
    if(!"with_direction" %in% names(ties))
      ties$with_direction <- vector("list", nrow(ties))
    ties$with_direction[!hyper] <- lapply(ties$with[!hyper], function(x)
      c("from", "to")[seq_along(x)])
  }
  for(col in setdiff(.with_cols(ties), "with_direction"))
    ties[[col]][!hyper] <- lapply(ties$with[!hyper], function(x)
      rep(NA, length(x)))
  ties$from <- ties$to <- NA_integer_
  out$ties <- dplyr::select(ties, "from", "to", "with", dplyr::everything())
  if("missings" %in% names(out)) out["missings"] <- list(NULL)
  validate_stocnet(out)
}

#' @rdname modif_project
#' @section Simplicial complexes:
#'   A simplicial complex is a hypergraph where each subset of a hyperedge
#'   is also a hyperedge.
#'   So its largest hyperedges, the faces, describe all of it.
#'   `to_simplicial()` finds these faces,
#'   and marks the result as a simplicial complex.
#'
#'   For a one-mode network, the faces are the maximal cliques,
#'   because each subset of a clique is also a clique.
#'   The direction, weights, and other attributes of the ties are lost.
#'   `to_mode()` gives the one-mode network that the cliques cover.
#'
#'   For a two-mode network or a hypergraph, the faces are the hyperedges
#'   that are not a subset of another hyperedge.
#' @examples
#' to_simplicial(ison_adolescents)
#' @export
to_simplicial <- function(.data) UseMethod("to_simplicial")

#' @export
to_simplicial.default <- function(.data){
  as_input(.data, to_simplicial)
}

#' @export
to_simplicial.igraph <- function(.data){
  as_igraph(to_simplicial(as_stocnet(.data)))
}

#' @export
to_simplicial.tbl_graph <- function(.data){
  as_tidygraph(to_simplicial(as_stocnet(.data)))
}

# Which faces lie within another: a larger one, or an earlier one of the same
# nodes, that holds all of their nodes. A face holds all of another's nodes
# where the two share as many nodes as the other has, so the nodes each pair
# of faces share are counted at once, from the faces each node is in. An empty
# face shares no node, but lies within any other face all the same.
.faces_within <- function(faces){
  sizes <- lengths(faces)
  k <- length(faces)
  face <- rep(seq_len(k), sizes)
  node <- match(unlist(faces, use.names = FALSE),
                unique(unlist(faces, use.names = FALSE)))
  by_node <- split(face, node)
  i <- unlist(lapply(by_node, function(f) rep(f, length(f))), use.names = FALSE)
  j <- unlist(lapply(by_node, function(f) rep(f, each = length(f))),
              use.names = FALSE)
  apart <- i != j
  shared <- rle(sort((i[apart] - 1) * k + j[apart]))
  i <- (shared$values - 1) %/% k + 1
  j <- (shared$values - 1) %% k + 1
  holds <- shared$lengths == sizes[i] &
    (sizes[j] > sizes[i] | (sizes[j] == sizes[i] & j < i))
  out <- rep(FALSE, k)
  out[i[holds]] <- TRUE
  empty <- which(sizes == 0)
  out[empty] <- any(sizes > 0) | seq_along(empty) > 1
  out
}

#' @export
to_simplicial.network <- function(.data){
  as_network(to_simplicial(as_stocnet(.data)))
}

#' @export
to_simplicial.stocnet <- function(.data) {
  if(is_twomode(.data) || is_hypergraph(.data)){
    out <- .dyads_to_hyper(to_hypergraph(.data))
    faces <- lapply(out$ties$with, function(x) sort(unique(x)))
    within <- .faces_within(faces)
    largest <- keep_ties(out, which(!within))
    largest$info$simplicial <- TRUE
    return(.record_exclusion(largest, out, "faces of a larger hyperedge",
                             "ties"))
  }
  # as_undirected() avoids both an igraph warning, since directions are ignored
  # in finding maximal cliques anyway, and a segfault in igraph 2.3.3 when
  # max_cliques() is called on a directed graph after any_multiple()
  cliques <- igraph::max_cliques(
    igraph::as_undirected(as_igraph(.data), mode = "collapse"))
  out <- .data
  out$ties <- dplyr::tibble(from = NA_integer_, to = NA_integer_,
                            with = lapply(cliques,
                                          function(x) sort(as.integer(x))),
                            label = paste0("h", seq_along(cliques)))
  if("missings" %in% names(out)) out["missings"] <- list(NULL)
  # The cliques are of the network as a whole, so they belong to no layer and
  # have no direction, sign, or moment of their own.
  out$info[c("layers", "signed", "update", "observation", "focal")] <- NULL
  out$info$directed <- FALSE
  out$info$simplicial <- TRUE
  out <- validate_stocnet(out)
  .record_transformation(out, "projection", "maximal cliques as hyperedges")
}

# Incidence ####

# A hypergraph can also be held as a two-mode network, in which each hyperedge
# is a node of a further mode and each membership a tie to it. This is how the
# classes without hyperedges of their own hold one, and what the projections
# and matrices of a hypergraph are calculated from. The hyperedges follow the
# nodes, so no node changes its index, and the two functions below undo each
# other.
.hyper_to_twomode <- function(.data){
  ties <- .data$ties
  hyper <- .hyper_rows(ties)
  hy <- ties[hyper, , drop = FALSE]
  wcols <- .with_cols(ties)
  n <- net_nodes(.data)
  m <- nrow(hy)
  sizes <- lengths(hy$with)
  directed <- .hyper_directed(.data)
  actors <- .data$nodes
  if(is.null(actors)) actors <- dplyr::tibble(.rows = n)
  he_mode <- .data$info$hyperedges %||% "hyperedges"
  actor_modes <- if("mode" %in% names(actors)) mode_names(.data) else
    .data$info$modes[1] %||% "nodes"
  if(he_mode %in% actor_modes)
    snet_abort("The hyperedges are named {.val {he_mode}}, as a mode of the",
               "nodes is. Rename one with {.fun add_info}.")
  if(!"mode" %in% names(actors)) actors$mode <- rep(actor_modes[1], n)
  # the hyperedges as nodes, with what each holds as their attributes
  hedges <- hy[setdiff(names(hy), c("from", "to", "with", wcols))]
  if("label" %in% names(actors)){
    if(!"label" %in% names(hedges)) hedges$label <- paste0("h", seq_len(m))
    if(any(hedges$label %in% actors$label)){
      snet_warn("Some hyperedges are labelled as nodes are, so they were",
                "renamed to tell them apart.")
      hedges$label <- make.unique(c(actors$label, hedges$label))[n + seq_len(m)]
    }
  } else if("label" %in% names(hedges))
    names(hedges)[names(hedges) == "label"] <- "hyperedge"
  hedges$mode <- rep(he_mode, m)
  nodes <- .bind_parts(actors, hedges)
  nodes <- dplyr::select(nodes, dplyr::any_of(c("label", "mode")),
                         dplyr::everything())
  # the memberships as ties, with what each holds as their attributes
  inc <- dplyr::tibble(from = as.integer(unlist(hy$with)),
                       to = n + rep(seq_len(m), sizes))
  for(col in wcols) inc[[sub("^with_", "", col)]] <- unlist(hy[[col]])
  if("layer" %in% names(hy)) inc$layer <- rep(hy$layer, sizes)
  if(directed){
    back <- inc$direction %in% "to"
    inc[back, c("from", "to")] <- inc[back, c("to", "from")]
    inc$direction <- NULL
  }
  # the dyads, less the columns that only the hyperedges had values in
  dyads <- ties[!hyper, setdiff(names(ties), c("with", wcols)), drop = FALSE]
  dyads <- dyads[!vapply(dyads, function(x) all(is.na(x)), logical(1)) |
                   names(dyads) %in% c("from", "to")]
  out <- .data
  out$nodes <- nodes
  out$ties <- if(nrow(dyads)) .bind_parts(dyads, inc) else inc
  out$info$modes <- c(actor_modes, he_mode)
  out$info$hyperedges <- NULL
  if(length(actor_modes) == 1 && !directed && !nrow(dyads)){
    out$info$sender <- actor_modes
    out$info$receiver <- he_mode
  }
  out
}

# Bind the rows of two tables whose columns of the same name may differ in
# class, as where the nodes and the hyperedges both have a 'time'.
.bind_parts <- function(x, y){
  for(col in intersect(names(x), names(y))){
    if(is.list(x[[col]]) || is.list(y[[col]])) next
    if(identical(class(x[[col]]), class(y[[col]]))) next
    if(is.numeric(x[[col]]) && is.numeric(y[[col]])) next
    x[[col]] <- as.character(x[[col]])
    y[[col]] <- as.character(y[[col]])
  }
  dplyr::bind_rows(x, y)
}

# The reverse: the nodes marked in `is_he` become hyperedges of the nodes they
# are tied to, and take their attributes with them. A tie between two of the
# other nodes stays the dyad it is.
.twomode_to_hyper <- function(.data, is_he){
  nodes <- .data$nodes
  ties <- .data$ties
  if(is.null(ties)) ties <- dplyr::tibble(from = integer(0), to = integer(0))
  # a node that is marked as neither, such as one added since, is a node
  is_he <- is_he %in% TRUE
  he_idx <- which(is_he)
  m <- length(he_idx)
  # with no node to take as a hyperedge, the network is as it should be
  if(m == 0) return(.data)
  map <- cumsum(!is_he)
  map[is_he] <- NA_integer_
  from_he <- ties$from %in% he_idx
  to_he <- ties$to %in% he_idx
  if(any(from_he & to_he))
    snet_abort("A hyperedge cannot be a member of a hyperedge, but some ties",
               "run between two nodes of the mode to take as hyperedges.")
  touch <- from_he | to_he
  inc <- ties[touch, , drop = FALSE]
  # The hyperedges have sides where the layers they are in are directed. A
  # layer of dyads may be directed without the hyperedges being so.
  directed <- .data$info$directed
  directed <- if(!is.null(names(directed)) && "layer" %in% names(inc))
    isTRUE(any(directed[unique(inc$layer)])) else isTRUE(any(directed))
  sent <- !from_he[touch]
  he <- ifelse(sent, inc$to, inc$from)
  member <- map[ifelse(sent, inc$from, inc$to)]
  side <- ifelse(sent, "from", "to")
  # a node is a member of a hyperedge once, or once on each side of it
  first <- !duplicated(paste(he, member, if(directed) side))
  inc <- inc[first, , drop = FALSE]
  f <- factor(he[first], levels = he_idx)
  parts <- function(x) unname(split(x, f))
  # the hyperedges, with the attributes their nodes held
  hedges <- nodes[is_he, setdiff(names(nodes), "mode"), drop = FALSE]
  hedges <- hedges[!vapply(hedges, function(x) all(is.na(x)), logical(1))]
  if(!"label" %in% names(hedges) && "hyperedge" %in% names(hedges))
    names(hedges)[names(hedges) == "hyperedge"] <- "label"
  hy <- dplyr::tibble(from = rep(NA_integer_, m), to = NA_integer_,
                      with = parts(as.integer(member[first])))
  layered <- "layer" %in% names(inc)
  if(layered && !"layer" %in% names(hedges))
    hedges$layer <- vapply(parts(inc$layer), function(x)
      if(length(x)) x[1] else NA_character_, character(1))
  hy <- dplyr::bind_cols(hy, hedges[setdiff(names(hedges), names(hy))])
  for(col in setdiff(names(inc), c("from", "to", "layer"))){
    if(all(is.na(inc[[col]]))) next
    hy[[paste0("with_", col)]] <- parts(inc[[col]])
  }
  if(directed) hy$with_direction <- parts(side[first])
  dyads <- ties[!touch, , drop = FALSE]
  dyads$from <- as.integer(map[dyads$from])
  dyads$to <- as.integer(map[dyads$to])
  dyads <- dyads[!vapply(dyads, function(x) all(is.na(x)), logical(1)) |
                   names(dyads) %in% c("from", "to")]
  # the nodes that remain, less the attributes only the hyperedges had
  actors <- nodes[!is_he, , drop = FALSE]
  if(m > 0){
    theirs <- vapply(actors, function(x) all(is.na(x)), logical(1)) &
      names(actors) %in% c(names(hedges), "hyperedge")
    actors <- actors[!theirs]
  }
  out <- .data
  info <- out$info
  # The name of the mode that the hyperedges were. A class that marks the
  # modes and does not name them leaves their names in the info, in order.
  marks <- unique(nodes[["mode"]])
  he_mode <- unique(nodes[["mode"]][is_he])
  actor_modes <- unique(actors[["mode"]])
  named <- !is.null(info$modes)
  if(named && length(info$modes) == length(marks) &&
     !all(marks %in% info$modes))
    he_mode <- info$modes[if(is.logical(marks)) he_mode + 1 else
      match(he_mode, marks)]
  if(length(actor_modes) <= 1) actors$mode <- NULL
  # What the hyperedges are called is kept beside what the nodes are called.
  # 'nodes' and 'hyperedges' are what they are called where they have no name.
  info$hyperedges <- NULL
  if(named){
    at <- match("modes", names(info))
    left <- setdiff(info$modes, c(he_mode, "nodes"))
    called <- if(length(he_mode) == 1 && he_mode != "hyperedges")
      list(hyperedges = he_mode)
    info <- c(info[seq_len(at - 1)], if(length(left)) list(modes = left),
              called, info[-seq_len(at)])
  }
  if(any(c(info$sender, info$receiver) %in% he_mode))
    info$sender <- info$receiver <- NULL
  out$info <- info
  out$nodes <- actors
  out$ties <- if(nrow(dyads)) dplyr::bind_rows(dyads, hy) else hy
  out$ties <- dplyr::select(out$ties, "from", "to", "with",
                            dplyr::everything())
  # A change to a node that is now a hyperedge has no node to apply to. Where
  # the changes say when each such node became active, and nothing else of
  # them, that is the moment of the hyperedge, as for the dates of events.
  if(!is.null(out$changes) && nrow(out$changes)){
    theirs <- out$changes$node %in% he_idx
    moved <- out$changes[theirs, , drop = FALSE]
    if(nrow(moved)){
      begun <- all(moved$var == "active") && !anyDuplicated(moved$node) &&
        all(vapply(moved$value, isTRUE, logical(1))) &&
        !"time" %in% names(out$ties)
      if(begun){
        # indexed rather than combined, so that a date stays a date
        time <- moved$time[c(rep(NA_integer_, nrow(dyads)),
                             match(he_idx, moved$node))]
        dim(time) <- NULL
        out$ties$time <- time
      } else snet_warn("The changes recorded about the nodes that are now",
                       "hyperedges were dropped, since a hyperedge is a tie.")
    }
    changes <- out$changes[!theirs, , drop = FALSE]
    changes$node <- as.integer(map[changes$node])
    out["changes"] <- list(if(nrow(changes)) changes)
  }
  if(!is.null(out$missings))
    out["missings"] <- list(.remap_tie_nodes(out$missings, map))
  validate_stocnet(out)
}


# Concept lattices ####

#' Modifying networks into concept lattices
#' @name modif_concepts
#' @description
#'   `to_concepts()` projects a network into its concept lattice,
#'   also known as its Galois lattice.
#'   Each node of the lattice is a concept.
#'   A concept is a set of nodes and the set of affiliations
#'   that all these nodes share.
#'   One set cannot get larger unless the other set gets smaller.
#'   Each tie goes from a concept to a concept directly below it.
#'   The lower concept has fewer nodes, but more shared affiliations.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods("to_concepts")
#'   ```
#'
#'   In a two-mode network, the nodes of the first mode (for example the rows)
#'   are the members of the concepts.
#'   This set is the _extent_.
#'   The nodes of the second mode (for example the columns)
#'   are the shared affiliations.
#'   This set is the _intent_.
#'
#'   Read the lattice from top to bottom.
#'   The top concept has all the nodes of the first mode,
#'   and the affiliations that all of them share, if there are any.
#'   The bottom concept has all the affiliations,
#'   and the nodes that have all of them, if there are any.
#'   Lower concepts have fewer nodes, which share more affiliations.
#'   Two nodes are in a concept together only when they share
#'   all its affiliations.
#'   So the lattice keeps all the ties of the original network.
#'
#'   For a one-mode network, each node is affiliated to the nodes
#'   that it sends ties to.
#'   Each concept is then a set of nodes,
#'   and all the nodes that each of them sends ties to.
#'   This is a maximal biclique of senders and receivers.
#'
#'   A valued or signed network is changed to binary data first,
#'   with a warning.
#'   Each positive value is then a tie.
#' @section Labels:
#'   Each concept has a "reduced" label.
#'   The label shows only the nodes and affiliations
#'   that first appear at that concept.
#'   A node of the first mode is named at the lowest concept that has it.
#'   It is also in all the concepts above that concept.
#'   An affiliation, in braces, is named at the highest concept that has it.
#'   It is also in all the concepts below that concept.
#'   A concept where nothing first appears is named "C"
#'   and its position from the top.
#'   So each label appears only once.
#'   The full sets are in the `extent` and `intent` node attributes,
#'   and their sizes are in `extent_size` and `intent_size`.
#' @section Projection without loss:
#'   `to_mode1()` and `to_mode2()` record how much two nodes share.
#'   They do not record what the two nodes share,
#'   or who else shares it.
#'   The concept lattice keeps both.
#'   For this reason, Freeman and White (1993) propose the lattice,
#'   and Freeman (2003) uses it to find the groups among the Southern Women.
#'   But the lattice gets large quickly in a dense network,
#'   and a lattice of more than some dozens of concepts is hard to read.
#'   When a network has more than 1000 concepts, the function gives a warning.
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

# Projects a network of three or more modes. The nodes of `through` count as
# one mode, so that with the nodes of `mode` and the ties between the two they
# make a two-mode network, which is then projected. `through` is all the other
# modes where it is NULL.
# The network is made a stocnet for this, as only it holds more than two
# modes, and returned in the class it was given.
.project_through <- function(.data, mode, through, similarity){
  if(!inherits(.data, "stocnet")){
    out <- .project_through(as_stocnet(.data), mode, through, similarity)
    if(inherits(.data, "tbl_graph")) return(as_tidygraph(out))
    if(inherits(.data, "igraph")) return(as_igraph(out))
    if(inherits(.data, "network")) return(as_network(out))
    return(out)
  }
  nms <- mode_names(.data)
  projected <- nms[.infer_mode(.data, mode)]
  if(is.null(through)) shared <- setdiff(nms, projected) else
    shared <- unique(nms[vapply(through, function(m) .infer_mode(.data, m),
                                integer(1))])
  if(projected %in% shared)
    snet_abort("{.arg through} must name modes other than {.arg mode}.")
  # the modes are named as they will be once the other modes' nodes are gone
  # and the modes of `through` are one, which is named for the modes that
  # `mode` has ties to
  ends <- .data$nodes$mode
  from <- ends[.data$ties$from]
  to <- ends[.data$ties$to]
  used <- shared[shared %in% c(to[from %in% projected], from[to %in% projected])]
  joined <- paste(if(length(used)) used else shared, collapse = " and ")
  kept <- which(.data$nodes$mode %in% c(projected, shared))
  .data$nodes$mode[.data$nodes$mode %in% shared] <- joined
  .data$info$modes <- unique(.data$nodes$mode[kept])
  .data$info$sender <- .data$info$receiver <- NULL
  out <- keep_nodes(.data, kept)
  # a tie within one mode is not a tie to the other mode, and a hyperedge
  # has no two ends to tell this from
  ends <- out$nodes$mode
  across <- !is.na(out$ties$from) & !is.na(out$ties$to) &
    ends[out$ties$from] != ends[out$ties$to]
  if(any(!across)){
    out$ties <- out$ties[across, , drop = FALSE]
    if("layer" %in% names(out$ties))
      out$info <- .prune_layer_info(out$info,
                                    intersect(out$info$layers,
                                              unique(out$ties$layer)))
  }
  out <- validate_stocnet(out)
  to_mode(out, projected, similarity)
}

# Resolves the `mode` argument of `to_mode()` to the index of a mode.
# A name is matched loosely, since the point of naming a mode is that the
# user need not recall how it was written when the network was collected.
.infer_mode <- function(.data, mode){
  # stated in terms of the network rather than as a literal 1 or 2, so that
  # it resolves a mode of a network of three or more modes too
  nmodes <- net_modes(.data)
  # A logical picks out a mode as the 'type' of an igraph does, FALSE for the
  # first and TRUE for the second, and so only where there are two.
  forms <- if(nmodes == 2) "{.or {seq_len(nmodes)}}, FALSE or TRUE," else
    "{.or {seq_len(nmodes)}},"
  if(is.logical(mode) && nmodes == 2 && length(mode) == 1 && !is.na(mode))
    return(as.integer(mode) + 1L)
  if(is.numeric(mode)){
    if(length(mode) != 1 || is.na(mode) || !mode %in% seq_len(nmodes))
      snet_abort("{.arg mode} must be", forms, "or the name of a mode.")
    return(as.integer(mode))
  }
  if(!is.character(mode) || length(mode) != 1)
    snet_abort("{.arg mode} must be a single index,", forms,
               "or a single mode name.")
  nms <- mode_names(.data)
  # a stocnet records two modes without names as "FALSE" and "TRUE"
  if(length(nms) != nmodes || setequal(nms, c("FALSE", "TRUE")))
    snet_abort("The modes of this network are not named.",
               "Please select a mode by index,", forms,
               "or name the modes with {.fun add_info}.")
  matches <- which(vapply(nms, .mode_matches, logical(1), query = mode))
  # a word can name more than one mode, as "events" does where the modes are
  # "social events" and "work events", so say so rather than report no match
  if(length(matches) > 1)
    snet_abort("{.arg mode} matches more than one mode:",
               "{phrase(nms[matches])}.",
               "Please give a name that tells them apart, or an index.")
  if(length(matches) == 0)
    snet_abort("{.arg mode} must be an index,", forms,
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

# The rules that read every tie of a two-mode network, rather than one value
# for each cell of its matrix, since a node can be tied to the same node of the
# other mode more than once, as a speaker can make several statements about one
# concept. They are rules of a projection only, and so `to_proximity()` takes
# them for a two-mode network and not for a one-mode network.
.proj_rules <- c("agreement","disagreement","signed",
                 "activity","association","fractional")

# The rules that read the value of each tie as a stance.
.proj_stance <- c("agreement","disagreement","signed")

# Those that can return a negative value, and so need the projection's ties
# labelled with a sign as well as a weight.
.proj_signed <- c("pearson","yule","covariance","hamann",
                  "spearman","kendall","cosine","signed")

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

# The ties of a two-mode network by their value. Each distinct value is a level,
# and `counts` holds, for each level, how many ties of that value each node of
# the projected mode (the rows) has to each node of the other mode (the
# columns). `X` sums the absolute values of the ties in each cell, which for
# ties of 1 or -1 is the number of ties. A matrix holds one value in each cell,
# so each cell of it that is not zero is one tie.
.proj_levels_matrix <- function(X){
  X[is.na(X)] <- 0
  values <- sort(unique(X[X != 0]))
  list(values = values,
       counts = lapply(values, function(v) (X == v) * 1),
       X = abs(X))
}

# The same from the ties of an igraph, so that repeated ties count each time.
# `which` is the mode projected: 1 for the nodes whose type is FALSE.
.proj_levels_ties <- function(.data, which){
  type <- igraph::V(.data)$type
  rows <- which(if(which == 1L) !type else type)
  cols <- which(if(which == 1L) type else !type)
  el <- igraph::as_edgelist(.data, names = FALSE)
  value <- igraph::edge_attr(.data, "weight")
  if(is.null(value)) value <- rep(1, nrow(el))
  if(anyNA(value)){
    snet_warn("{sum(is.na(value))} tie{?s} with a missing value {?was/were}",
              "left out of the projection, since the rule reads every value.")
    el <- el[!is.na(value), , drop = FALSE]
    value <- value[!is.na(value)]
  }
  # each tie runs between the two modes, in either order
  row_end <- ifelse(el[,1] %in% rows, el[,1], el[,2])
  col_end <- ifelse(el[,1] %in% rows, el[,2], el[,1])
  r <- match(row_end, rows)
  k <- match(col_end, cols)
  cell <- (k - 1L) * length(rows) + r
  tally <- function(w){
    out <- matrix(0, length(rows), length(cols))
    if(length(w)){
      sums <- rowsum(as.numeric(w), cell)
      out[as.integer(rownames(sums))] <- sums[, 1]
    }
    out
  }
  values <- sort(unique(value))
  nms <- igraph::V(.data)$name[rows]
  out <- list(values = values,
              counts = lapply(values, function(v) tally((value == v) * 1)),
              X = tally(abs(value)))
  out$counts <- lapply(out$counts, function(m){ rownames(m) <- nms; m })
  rownames(out$X) <- nms
  out
}

# Projects the levels of a two-mode network by one of `.proj_rules`.
.project_levels <- function(lv, similarity){
  X <- lv$X
  out <- switch(similarity,
                # each pair of statements about the
                # same concept adds the proximity, or for "disagreement" the
                # distance, of their two stances, on a scale from the lowest
                # stance to the highest. For stances of 1 and -1 this counts
                # the pairs that agree, or that disagree.
                "agreement" = , "disagreement" = , "signed" = {
                  v <- lv$values
                  span <- if(length(v) > 1) max(v) - min(v) else 1
                  agree <- disagree <- matrix(0, nrow(X), nrow(X))
                  for(a in seq_along(v)) for(b in seq_along(v)){
                    joint <- lv$counts[[a]] %*% t(lv$counts[[b]])
                    distance <- abs(v[a] - v[b])/span
                    agree <- agree + joint * (1 - distance)
                    disagree <- disagree + joint * distance
                  }
                  switch(similarity, "agreement" = agree,
                         "disagreement" = disagree, "signed" = agree - disagree)
                },
                # the count over the average number of ties of the two nodes
                "activity" = {
                  s <- rowSums(X)
                  (X %*% t(X)) / (outer(s, s, "+")/2)
                },
                # van Eck and Waltman's association strength: the count over the
                # count expected where the two nodes were tied independently,
                # so that 1 is what independence would give
                "association" = {
                  s <- rowSums(X)
                  ncol(X) * (X %*% t(X)) / outer(s, s)
                },
                # fractional counting: each node of the other mode, such as a
                # work, adds 1/(n - 1) to each pair of its n members, so that
                # each work gives each of its members a total of 1
                "fractional" = {
                  B <- (X > 0) * 1
                  n <- colSums(B)
                  share <- ifelse(n > 1, 1/(n - 1), 0)
                  B %*% (t(B) * share)
                })
  out[!is.finite(out)] <- 0
  dimnames(out) <- list(rownames(X), rownames(X))
  diag(out) <- 0
  out
}

# Projects an igraph two-mode network to the matrix of the mode `which`.
# The rules that read every tie are found from its ties, and the rest from its
# matrix, which holds one value for each cell.
.project_graph <- function(.data, which, similarity){
  if(similarity %in% .proj_rules)
    return(.project_levels(.proj_levels_ties(.data, which), similarity))
  if(which == 1L) to_mode1(as_matrix(.data), similarity) else
    to_mode2(as_matrix(.data), similarity)
}

# Projects the matrix `X`, whose rows are the mode being projected, so that
# `to_mode2()` can share this by passing the transpose.
.project <- function(X, similarity){
  if(similarity %in% .proj_rules){
    out <- .project_levels(.proj_levels_matrix(X), similarity)
    dimnames(out) <- list(rownames(X), rownames(X))
    return(out)
  }
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
