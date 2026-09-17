# Proximity ####

#' Node proximity
#' @name modif_proximity
#' @description
#'   These functions construct a square, node-by-node matrix of how alike
#'   the nodes of a one-mode network are:
#'
#'   - `to_proximity()` compares each pair of nodes on their ties,
#'   using any of the measures that `to_mode1()` offers.
#'   Given a two-mode network, or a node-by-feature profile matrix such as a
#'   motif census, it compares the rows or the columns as they lie.
#'   - `to_correlation()` performs a Pearson pairwise correlation,
#'   choosing how to treat the diagonal and the reciprocal ties from the
#'   network's format.
#'   - `to_cosine()` takes the cosine of the angle between the columns of a
#'   matrix.
#'
#'   Where `to_mode1()` compares nodes on their affiliations to a second mode,
#'   these functions compare nodes on their ties to one another.
#'   The two share one measure vocabulary and differ only in the profile
#'   compared. For a two-mode network there is no difference, and
#'   `to_proximity()` returns what `to_mode1()` or `to_mode2()` would.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_(proximity|correlation|cosine)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_proximity
#' @inheritParams to_mode1
#' @template param_across
#' @param dyad How the cells within the compared pair's own dyad are treated.
#'
#'   Four of the cells in any comparison are not like the others: \eqn{i}'s tie
#'   to \eqn{j}, \eqn{j}'s tie to \eqn{i}, and each node's self-tie.
#'   These cannot be compared where they lie the way the rest can.
#'   In column \eqn{j}, node \eqn{i}'s entry is a tie to \eqn{j},
#'   but node \eqn{j}'s entry is its own self-tie: two different things in the
#'   same position. Two managers who happen to be tied to one another are not
#'   less structurally equivalent for it, but comparing those cells where they
#'   lie makes them look that way.
#'
#'   - "exclude" drops all four cells, so that nodes are compared only on
#'   their ties to third parties. This is the strictest reading of structural
#'   equivalence, and the default for an undirected network.
#'   - "reciprocal" drops the self-ties, but compares \eqn{i}'s tie to
#'   \eqn{j} against \eqn{j}'s tie to \eqn{i}, so that a reciprocated pair
#'   counts as alike there. This is the default for a directed network.
#'   - "complex" does the same, and additionally compares the two nodes'
#'   self-ties against each other. This is the default where the network has
#'   self-ties, since there they carry information.
#'   - "include" compares every cell where it lies, with no special treatment.
#'   Use it only where the diagonal genuinely holds comparable values.
#'   It is also the quickest, since it needs no pairwise walk.
#'
#'   By default the appropriate treatment is chosen from the network's format,
#'   as described above.
#'
#'   A two-mode network has no dyad of this kind, so only `NULL` or "include"
#'   is accepted for one. Pass "include" for a profile matrix whose rows are
#'   nodes and whose columns are something else, such as a motif census.
#'   A square census with no dimnames otherwise reads as a one-mode network,
#'   and would have its cells dropped as if they were ties.
#' @seealso [to_mode1()], which applies the same measures to a two-mode
#'   network, comparing nodes on their affiliations rather than on their ties.
#' @references
#' ## On structural equivalence
#'   Lorrain, Francois, and Harrison C. White. 1971.
#'   "Structural equivalence of individuals in social networks".
#'   _The Journal of Mathematical Sociology_ 1(1): 49-80.
#'   \doi{10.1080/0022250X.1971.9989788}
#' @examples
#' to_proximity(ison_algebra, "pearson")
#' to_proximity(ison_adolescents, "jaccard")
#' to_proximity(ison_southern_women, "ruzicka", across = "columns")
#' @export
to_proximity <- function(.data, similarity = .proj_measures,
                         across = c("rows", "columns", "both"),
                         dyad = NULL){
  # a two-mode network's profile is its biadjacency matrix, so there is no
  # dyad to treat and its nodes are compared as the projections compare them
  if(is_twomode(.data))
    return(.proximity_twomode(.data, match.arg(similarity),
                              match.arg(across), dyad))
  UseMethod("to_proximity")
}

#' @export
to_proximity.default <- function(.data, similarity = .proj_measures,
                                 across = c("rows", "columns", "both"),
                                 dyad = NULL){
  as_input(.data, to_proximity, similarity = similarity,
           across = across, dyad = dyad)
}

#' @export
to_proximity.stocnet <- function(.data, similarity = .proj_measures,
                                 across = c("rows", "columns", "both"),
                                 dyad = NULL){
  # The tidygraph method is called directly rather than through `as_input()`,
  # as `to_mode1.stocnet()` does.
  as_stocnet(to_proximity(as_tidygraph(.data), match.arg(similarity),
                          match.arg(across), dyad))
}

#' @export
to_proximity.matrix <- function(.data, similarity = .proj_measures,
                                across = c("rows", "columns", "both"),
                                dyad = NULL){
  .proximity(.data, match.arg(similarity), match.arg(across), dyad)
}

#' @export
to_proximity.igraph <- function(.data, similarity = .proj_measures,
                                across = c("rows", "columns", "both"),
                                dyad = NULL){
  as_igraph(to_proximity(as_matrix(.data), match.arg(similarity),
                         match.arg(across), dyad))
}

#' @export
to_proximity.tbl_graph <- function(.data, similarity = .proj_measures,
                                   across = c("rows", "columns", "both"),
                                   dyad = NULL){
  similarity <- match.arg(similarity)
  out <- as_tidygraph(to_proximity(as_matrix(.data), similarity,
                                   match.arg(across), dyad))
  if(similarity %in% .proj_signed){
    # an isolate gives NaN under several measures, and `NaN < 0` is NA,
    # which would otherwise leave the sign missing rather than positive
    wt <- tie_weights(out)
    out <- out |> mutate_ties(sign = dplyr::if_else(!is.na(wt) & wt < 0, -1, 1))
  }
  out <- bind_node_attributes(out, .data)
  if(!is.null(net_name(.data))) out <- out |>
      add_info(name = net_name(.data, prefix = "Proximities of"))
  out |> .record_transformation("projection",
                                paste0("proximity (", similarity, ")"))
}

#' @export
to_proximity.network <- function(.data, similarity = .proj_measures,
                                 across = c("rows", "columns", "both"),
                                 dyad = NULL){
  as_network(to_proximity(as_tidygraph(.data), match.arg(similarity),
                          match.arg(across), dyad))
}

#' @export
to_proximity.data.frame <- function(.data, similarity = .proj_measures,
                                    across = c("rows", "columns", "both"),
                                    dyad = NULL){
  as_edgelist(to_proximity(as_tidygraph(.data), match.arg(similarity),
                           match.arg(across), dyad))
}

# Compares each pair of nodes on the profile named by `across`, treating the
# cells of the pair's own dyad as `dyad` directs.
.proximity <- function(A, similarity, across, dyad){
  # a two-mode network never reaches here, since the generic sends it to
  # `.proximity_twomode()`
  A <- as_matrix(A)
  dyad <- if(is.null(dyad)) .infer_dyad(A) else
    match.arg(dyad, c("exclude", "reciprocal", "complex", "include"))
  n <- nrow(A)
  # each block of the profile matrix keeps node j in column j, so that the
  # dyad's cells stay findable by index however many blocks there are
  P <- switch(across, rows = A, columns = t(A), both = cbind(A, t(A)))
  if(dyad == "include") return(.zero_missing(.project(P, similarity)))
  .pairwise_project(P, similarity, dyad, n)
}

# A two-mode network is compared on its biadjacency matrix as it lies, which
# is what the projections already do, so they are reused for every class.
.proximity_twomode <- function(.data, similarity, across, dyad){
  if(!is.null(dyad) &&
     match.arg(dyad, c("exclude", "reciprocal", "complex", "include")) != "include")
    snet_abort(paste0("A two-mode network has no dyad within which cells ",
                      "could be dropped or swapped, so {.arg dyad} can only ",
                      "be {.val include} or left as {.code NULL} here."))
  if(across == "both")
    snet_abort(paste0("The rows and columns of a two-mode network are ",
                      "different nodes, so they cannot be compared together. ",
                      "Use {.val rows} or {.val columns} instead."))
  # A stocnet reads a tie of missing value as a tie it did not observe, and a
  # network object drops the value, so both are compared as a tidygraph, where
  # an undefined value can still be told apart and dropped, and converted back.
  # As `to_mode1.stocnet()` and `to_mode2.stocnet()` do, a stocnet's changes
  # are kept only for the nodes of the mode being compared.
  if(inherits(.data, "stocnet")){
    kept <- if(across == "rows") which(!node_is_mode(.data)) else
      which(node_is_mode(.data))
    return(as_stocnet(.proximity_twomode(
      as_tidygraph(.project_changes(.data, kept)), similarity, across, dyad)))
  }
  if(inherits(.data, "network"))
    return(as_network(.proximity_twomode(as_tidygraph(.data), similarity,
                                         across, dyad)))
  out <- switch(across,
                rows = to_mode1(.data, similarity),
                columns = to_mode2(.data, similarity))
  if(is.matrix(out)) .zero_missing(out) else .drop_undefined_ties(out)
}

# The same for a projected network: a similarity of zero is no tie, so a tie
# whose similarity is undefined is dropped rather than kept without a value.
.drop_undefined_ties <- function(out){
  if(is.data.frame(out)){
    if(!"weight" %in% names(out)) return(out)
    return(out[!is.na(out$weight), , drop = FALSE])
  }
  if(!inherits(out, "igraph")) return(out)
  value <- igraph::edge_attr(out, "weight")
  if(is.null(value) || !anyNA(value)) return(out)
  delete_ties(out, which(is.na(value)))
}

# A node with no ties leaves some measures dividing by zero. It is reported
# as no more similar to another node than any other, as the pairwise walk
# already reports it.
.zero_missing <- function(out){
  out[is.na(out)] <- 0
  out
}

# Mirrors the default that `to_correlation()` has always taken, so that the
# two functions read a network's format the same way.
.infer_dyad <- function(A){
  if(is_complex(A)) "complex" else if(is_directed(A)) "reciprocal" else "exclude"
}

# Walks the pairs, since which cells are compared depends on which pair is
# being compared and so cannot be done in one matrix operation. Each pair's
# two vectors are handed back to `.project()` as a two-row matrix, so that
# every measure is computed by the same code as for the vectorised path.
.pairwise_project <- function(P, similarity, dyad, n){
  if(similarity %in% .proj_binary && any(P != 0 & P != 1, na.rm = TRUE)){
    snet_warn(paste0("The {.val {similarity}} measure is defined for binary ",
                     "data only, so tie values have been dichotomised at 0. ",
                     "Consider {.val ruzicka}, {.val crossmin}, or ",
                     "{.val overlap} to retain them."))
    P <- (P > 0) * 1
  }
  blocks <- seq_len(ncol(P)/n) - 1L
  out <- matrix(0, n, n, dimnames = list(rownames(P), rownames(P)))
  if(n < 2L) return(out)
  pairs <- utils::combn(n, 2L)
  vals <- apply(pairs, 2, function(ij){
    i <- ij[1]; j <- ij[2]
    # the pair's own columns in every block, dropped from the bulk comparison
    own <- c(i, j) + rep(blocks * n, each = 2L)
    x <- P[i, -own]; y <- P[j, -own]
    if(dyad != "exclude"){
      # i's tie to j against j's tie to i: the same column position in each
      # row, swapped between them
      x <- c(x, P[i, j + blocks * n]); y <- c(y, P[j, i + blocks * n])
    }
    if(dyad == "complex"){
      # and each node's self-tie against the other's
      x <- c(x, P[i, i + blocks * n]); y <- c(y, P[j, j + blocks * n])
    }
    suppressWarnings(.project(rbind(x, y), similarity)[1, 2])
  })
  out[lower.tri(out)] <- vals
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  out[is.na(out)] <- 0
  diag(out) <- 0
  out
}

#' @rdname modif_proximity
#' @param method How the diagonal and the reciprocal ties are treated.
#'   "all" includes all information, "diag" excludes the diagonal (self-ties),
#'   "recip" excludes the diagonal but compares pairs' reciprocal ties,
#'   and "complex" compares pairs' reciprocal ties and their self ties.
#'   By default the appropriate method is chosen based on the network format.
#'
#'   These correspond to `to_proximity()`'s `dyad` argument, except that
#'   "recip" and "complex" append each node's tie to the other to profiles
#'   otherwise taken over the columns. `to_proximity()` instead keeps the
#'   profile and the appended cells on the same margin.
#'   The two therefore agree for an undirected network but not for a
#'   directed one.
#' @section Node correlation:
#'   `to_correlation()` performs a Pearson pairwise correlation.
#'   It includes a switch: whereas for a two-mode network it performs a
#'   regular correlation, including all rows, for an undirected network it
#'   performs a correlation on a matrix with the diagonals removed, for a
#'   reciprocated network it includes the difference between reciprocated
#'   ties, and for complex networks it includes also the difference between
#'   the self ties in each pairwise calculation.
#'   This function runs in \eqn{O(mn^2)} complexity.
#' @export
to_correlation <- function(.data, method = NULL) UseMethod("to_correlation")

#' @export
to_correlation.default <- function(.data, method = NULL){
  as_input(.data, to_correlation, method = method)
}

#' @export
to_correlation.matrix <- function(.data, method = NULL){
  switch(method %||% .cor_method(.data),
         # the columns are correlated as they lie, since a two-mode network
         # has neither a diagonal nor a reciprocal tie to account for
         all = .corTwomode(.data),
         complex = .corComplex(.data),
         recip = .corRecip(.data),
         # a node is perfectly correlated with itself, so the diagonal is set
         # to 1, where `to_proximity()` leaves the 0 that every other network
         # in the package carries on its diagonal
         diag = {
           out <- to_proximity(.data, "pearson", across = "columns",
                               dyad = "exclude")
           diag(out) <- 1
           out
         })
}

#' @export
to_correlation.tbl_graph <- function(.data, method = NULL){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  to_correlation(as_matrix(.data), method)
}

.cor_method <- function(.data){
  if(is_twomode(.data)) "all" else if(is_complex(.data)) "complex" else
    if(is_directed(.data)) "recip" else "diag"
}

#' @rdname modif_proximity
#' @section Node cosine:
#'   `to_cosine()` takes the cosine of the angle between each pair of the
#'   matrix's columns, without projecting it.
#'   It is `to_proximity()`'s "cosine" measure over the columns rather than
#'   the rows, and so accepts a two-mode network, where it describes the
#'   second mode.
#' @export
to_cosine <- function(.data) UseMethod("to_cosine")

#' @export
to_cosine.default <- function(.data){
  as_input(.data, to_cosine)
}

#' @export
to_cosine.matrix <- function(.data){
  # `.project()` is called directly rather than through `to_proximity()`,
  # which compares the rows and would turn a two-mode network away
  out <- .project(t(as_matrix(.data)), "cosine")
  diag(out) <- 1
  out
}

# Helper functions ------------------

.corTwomode <- function(m0){
  stats::cor(m0)
}


# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2 times faster than corrColsRecipRLB()
.corRecip <- function(M){
  all.pairs <- utils::combn(1:ncol(M),2)
  corres <- apply(all.pairs, 2, function(i){
    x <- c(M[-i,i[1]], M[i[1],i[2]])
    y <- c(M[-i,i[2]], M[i[2],i[1]])
    suppressWarnings(stats::cor(x = x, y = y))
  })
  out <- matrix(1,nrow(M),ncol(M))
  out[lower.tri(out)] <- corres
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2.3 times faster than corrColsRecipUCI()
.corComplex <- function(M){
  all.pairs <- utils::combn(1:ncol(M),2)
  corres <- apply(all.pairs, 2, function(i){
    x <- c(M[-i,i[1]], M[i[1],i[2]], M[i[1],i[1]])
    y <- c(M[-i,i[2]], M[i[2],i[1]], M[i[2],i[2]])
    suppressWarnings(stats::cor(x = x, y = y))
  })
  out <- matrix(1,nrow(M),ncol(M))
  out[lower.tri(out)] <- corres
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

.makeSymm <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  m
}
