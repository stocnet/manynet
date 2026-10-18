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

# Which cells are compared depends on which pair is being compared. Most
# measures are functions of sums over the two profiles, though, and those sums
# are found for every pair at once by `.pairwise_sums()`. The rest walk the
# pairs, handing each pair's two vectors back to `.project()` as a two-row
# matrix, so that every measure is computed by the same code as for the
# vectorised path.
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
  # A missing cell would spread through a whole-row sum to pairs that do not
  # compare it, and ranks are not sums, so those pairs are walked instead.
  # Profiles of fewer than two cells are left to the walk too, as degenerate.
  if(!anyNA(P) && !similarity %in% c("spearman", "kendall", "maxcrossmin") &&
     .pair_length(P, dyad, n) >= 2){
    vals <- .pairwise_sums(P, similarity, dyad, n, blocks)
    # each pair is read with its earlier node first, as the walk reads it
    vals[lower.tri(vals)] <- t(vals)[lower.tri(vals)]
    out[] <- vals
    out[is.na(out)] <- 0
    diag(out) <- 0
    return(out)
  }
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

# How many cells each pair's profiles hold: every column but the pair's own
# two in each block, and then the cells that `dyad` adds back.
.pair_length <- function(P, dyad, n){
  k <- ncol(P)/n
  ncol(P) - 2*k + (dyad != "exclude")*k + (dyad == "complex")*k
}

# Computes the measure for every pair at once. Each sum over a pair's profiles
# is the sum over the two whole rows, less the cells of the pair's own columns
# in each block, plus the cells that `dyad` adds back: i's tie to j against
# j's tie to i, and for "complex" each node's self-tie against the other's.
# Every such cell is read off a block, its diagonal, or its transpose, so each
# correction is one elementwise operation on an n by n matrix. Cell [i, j] of
# each result holds the pair read with row i first.
.pairwise_sums <- function(P, similarity, dyad, n, blocks){
  len <- .pair_length(P, dyad, n)
  # The cells on each block's diagonal, the self-ties, are only ever taken
  # away again or, for "complex", added back, so they are kept aside here.
  on_diag <- lapply(blocks, function(b) cbind(seq_len(n), b*n + seq_len(n)))
  self <- lapply(on_diag, function(cells) P[cells])
  # Whole numbers are summed exactly, so their sums are taken as they are.
  # Other values would leave rounding behind wherever a large cell is taken
  # away again, so the self-ties are first set to the mean of the row's other
  # cells. A correlation or covariance, which no shift of a row changes, is
  # also taken on rows centred on that mean.
  whole <- all(P == round(P))
  if(!whole){
    others <- rowSums(P)
    for(cells in on_diag) others <- others - P[cells]
    others <- others/(ncol(P) - length(blocks))
    shift <- if(similarity %in% c("pearson", "covariance")) others else 0
    P <- P - shift
    self <- lapply(self, function(s) s - shift)
    for(cells in on_diag) P[cells] <- others - shift
  }
  # `full` sums f over the whole rows, and f(x, y) gives the cells of row i
  # and row j as matrices indexed by the pair. A sum of values that are not
  # whole numbers, that is no more than rounding of the cells it was found
  # from, is taken to be zero, as the sum over the pair's cells would be.
  pair_sum <- function(f, full, size = abs(full)){
    for(k in seq_along(blocks)){
      B <- P[, blocks[k]*n + seq_len(n), drop = FALSE]
      tB <- t(B)
      own_i <- matrix(diag(B), n, n)
      own_j <- matrix(diag(B), n, n, byrow = TRUE)
      parts <- list(-f(own_i, tB), -f(B, own_j))
      if(dyad != "exclude") parts <- c(parts, list(f(B, tB)))
      if(dyad == "complex")
        parts <- c(parts, list(f(matrix(self[[k]], n, n),
                                 matrix(self[[k]], n, n, byrow = TRUE))))
      for(part in parts){
        full <- full + part
        size <- size + abs(part)
      }
    }
    if(!whole) full[abs(full) <= 64*.Machine$double.eps*size] <- 0
    unname(full)
  }
  sx <- pair_sum(function(x, y) x, matrix(rowSums(P), n, n))
  sy <- t(sx)
  sxy <- function() pair_sum(`*`, tcrossprod(P))
  sqx <- function() pair_sum(function(x, y) x^2, matrix(rowSums(P^2), n, n))
  manhattan <- function()
    pair_sum(function(x, y) abs(x - y),
             as.matrix(stats::dist(P, method = "manhattan")))
  # the whole rows' sum of minima is found from their totals and distance,
  # and so is as rounded as those are
  crossmin <- function(){
    R <- rowSums(abs(P))
    pair_sum(pmin, .proj_crossmin(P), outer(R, R, "+"))
  }
  switch(similarity,
         "count" = sxy(),
         "covariance" = (sxy() - sx*sy/len)/(len - 1),
         "pearson" = {
           v <- len*sqx() - sx^2
           # a profile without variance has no correlation, though rounding
           # may leave a trace of variance where the rows were centred
           v[v < 1e-12*len*sqx()] <- 0
           out <- (len*sxy() - sx*sy)/sqrt(v*t(v))
           out[v == 0 | t(v) == 0] <- NA
           out
         },
         "cosine" = {
           l <- sqrt(sqx())
           l[l == 0] <- 1
           sxy()/(l*t(l))
         },
         "sqdiff" = ,
         "euclidean" = {
           full <- if(whole){
             q <- rowSums(P^2)
             outer(q, q, "+") - 2*tcrossprod(P)
           } else as.matrix(stats::dist(P))^2
           d2 <- pmax(pair_sum(function(x, y) (x - y)^2, full), 0)
           if(similarity == "sqdiff") 1/(1 + d2) else 1/(1 + sqrt(d2))
         },
         "manhattan" = ,
         "hamming" = 1/(1 + manhattan()),
         "crossmin" = crossmin(),
         "overlap" = crossmin()/pmin(sx, sy),
         "ruzicka" = {
           cm <- crossmin()
           cm/(sx + sy - cm)
         },
         "match" = pair_sum(function(x, y) (x == y)*1, .proj_match(P)),
         # the remaining measures are all functions of the counts, which for
         # binary profiles follow from the sums
         {
           a <- sxy()
           b <- sx - a
           c <- sy - a
           d <- len - a - b - c
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
                  "bonacich" = sqrt(a*d)/(sqrt(a*d) + sqrt(b*c)))
         })
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
#'   
#'   Like the other functions here, it returns the class of object it was 
#'   given, so that the correlations between the nodes of a network are
#'   returned as a weighted network of the same class.
#'   The correlations are calculated on a matrix, however, so a network is
#'   coerced to one and the result coerced back.
#'   Passing a matrix, e.g. `to_correlation(as_matrix(.data))`, spares both,
#'   and returns the matrix of correlations that further calculation most
#'   often wants.
#' @export
to_correlation <- function(.data, method = NULL) UseMethod("to_correlation")

#' @export
to_correlation.default <- function(.data, method = NULL){
  # a hypergraph is correlated as the two-mode network of its nodes and its
  # hyperedges, which is how every class but a 'stocnet' holds it anyway
  if(.holds_hyperedges(.data))
    return(to_correlation(to_twomode(.data), method = method))
  # the method follows the format of the network, which a matrix of it does
  # not always show, as where no tie of a directed network is unreciprocated
  as_input(.data, to_correlation, method = method %||% .cor_method(.data))
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


.corRecip <- function(M){
  if(anyNA(M)) .corRecipWalk(M) else .corDyadic(M, complex = FALSE)
}

.corComplex <- function(M){
  if(anyNA(M)) .corComplexWalk(M) else .corDyadic(M, complex = TRUE)
}

# Correlates every pair of columns at once. Pair i and j compare column i and
# column j without rows i and j, together with i's tie to j against j's tie to
# i, and for complex networks each node's self-tie against the other's. Each
# of the sums the correlation needs is therefore the sum over the whole
# columns corrected by a few cells, read off the matrix, its transpose, and
# its diagonal. Cell [i, j] of each sum holds the pair read with i first.
# A missing cell would spread through the whole-column sums, so matrices
# holding one are walked pair by pair instead.
.corDyadic <- function(M, complex){
  # Whole numbers are summed exactly. Otherwise every cell is shifted by the
  # mean of the ties between nodes, which leaves each correlation as it was,
  # so that the sums do not cancel. Without "complex" the self-ties are
  # compared by no pair, and are set to that mean so as to fall away.
  if(any(M != round(M))){
    M <- M - mean(M[row(M) != col(M)])
    if(!complex) diag(M) <- 0
  }
  d <- if(complex) 0 else diag(M)
  tM <- t(M)
  len <- nrow(M) - !complex
  sx <- colSums(M) - d - tM + M
  sqx <- colSums(M^2) - d^2 - tM^2 + M^2
  dM <- diag(M) * M
  sxy <- crossprod(M) - dM - t(dM) + M * tM +
    if(complex) outer(diag(M), diag(M)) else 0
  v <- len*sqx - sx^2
  v[v < 1e-12*len*sqx] <- 0
  corres <- (len*sxy - sx*t(sx))/sqrt(v*t(v))
  corres[v == 0 | t(v) == 0] <- NA
  out <- matrix(1, nrow(M), ncol(M))
  out[lower.tri(out)] <- t(corres)[lower.tri(out)]
  out <- .makeSymm(out)
  out[is.na(out)] <- 0
  diag(out) <- 1
  rownames(out) <- rownames(M)
  colnames(out) <- colnames(M)
  out
}

# Though warnings need to be suppressed,
# this is bench::mark()ed at about 2 times faster than corrColsRecipRLB()
.corRecipWalk <- function(M){
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
.corComplexWalk <- function(M){
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
