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
# #'   - `to_galois()` projects a network to its Galois derivation.
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
#' | | Other similarity measures | 18 in all, see `similarity` | no | code manually |
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
#'   - "overlap" divides the count by the smaller of the two nodes' total tie
#'   strength, the Szymkiewicz-Simpson coefficient. Use it where one node is
#'   much more active than the other, and the less active node's rarity should
#'   not depress the score.
#'   - "crossmin" sums the smaller of each pair of tie strengths.
#'   Interpret it as the capacity two nodes could jointly bring to bear.
#'   For binary data it reduces to "count".
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
#'   many events.
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
#' @seealso [to_cosine()], which takes the cosine over the columns of a
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
                                 "hamann", "rogerstanimoto")) {
  # projecting a network that is already one-mode is a no-op
  if(!is_twomode(.data)) return(.data)
  UseMethod("to_mode1")
}

#' @export
to_mode1.default <- function(.data, 
                             similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")){
  as_input(.data, to_mode1, similarity = similarity)
}

#' @export
to_mode1.matrix <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
  # the rows are already the mode being projected
  .project(.data, match.arg(similarity))
}

#' @export
to_mode1.igraph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
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
                                 "hamann", "rogerstanimoto")) {
  similarity <- match.arg(similarity)
  out <- as_tidygraph(to_mode1(as_igraph(.data), similarity = similarity))
  if(similarity %in% c("pearson","yule","covariance","hamann")){
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
  out |> add_info(transform = paste0("mode-1 projection (", similarity, ")"))
}

#' @export
to_mode1.network <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
 as_network(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @export
to_mode1.data.frame <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
  as_edgelist(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @rdname modif_project
#' @export
to_mode2 <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
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
                                 "hamann", "rogerstanimoto")){
  as_input(.data, to_mode2, similarity = similarity)
}

#' @export
to_mode2.matrix <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
  # transposed so that the columns become the rows being projected
  .project(t(.data), match.arg(similarity))
}

#' @export
to_mode2.igraph <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
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
                                 "hamann", "rogerstanimoto")) {
  similarity <- match.arg(similarity)
  out <- as_tidygraph(to_mode2(as_igraph(.data), similarity = similarity))
  if(similarity %in% c("pearson","yule","covariance","hamann")){
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
  out |> add_info(transform = paste0("mode-2 projection (", similarity, ")"))
}

#' @export
to_mode2.network <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
  as_network(to_mode2(as_tidygraph(.data), similarity)) 
}

#' @export
to_mode2.data.frame <- function(.data, similarity = c("count", "jaccard", "rand", "pearson", "yule",
                                 "match", "overlap", "crossmin", "maxcrossmin",
                                 "sqdiff", "covariance", "bonacich", "ochiai",
                                 "ochiai2", "czekanowski", "sokalsneath",
                                 "hamann", "rogerstanimoto")) {
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
                                 "hamann", "rogerstanimoto")) {
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



# #' @rdname manip_project
# #' @section Galois lattices: 
# #'   Note that the output from `to_galois()` is very busy at the moment.
# #' @export
# to_galois <- function(.data) {
#   x <- as_matrix(.data)
#   thisRequires("multiplex")
#   out <- multiplex::galois(x, labeling = "reduced")
#   out <- multiplex::partial.order(out, type = "galois")
#   class(out) <- c("matrix", class(out))
#   rownames(out)[!startsWith(rownames(out), "{")] <- ""
#   colnames(out)[!startsWith(colnames(out), "{")] <- ""
#   out
# }


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
                    "czekanowski","sokalsneath","hamann","rogerstanimoto")

# Those defined for binary data only, since they count cells in which both,
# one, or neither node is present. A valued network is dichotomised for these.
.proj_binary <- c("jaccard","rand","hamann","rogerstanimoto","czekanowski",
                  "ochiai","ochiai2","sokalsneath","yule","bonacich")

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
                     "Consider {.val count}, {.val crossmin}, or ",
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
                # xUCINET documents this denominator as the sum of the pairwise
                # minima but computes the smaller of the two row totals, which
                # is the Szymkiewicz-Simpson coefficient; the code is followed
                # here, since the documented version returns 1 throughout for
                # binary data
                "overlap" = {
                  R <- rowSums(X)
                  (X %*% t(X))/outer(R, R, pmin)
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
