#' Listing the motifs a network can contain
#' @name modif_motifs
#' @description
#'   `to_motifs()` returns a named list of small networks representing the
#'   motifs (subgraphs, or isomorphism classes) that a network of a given
#'   type could contain.
#'   Unlike most other `to_*()` functions, it does not modify and return the
#'   network passed to it, but instead uses that network only to work out which
#'   motifs are relevant, returning the whole reference set of motifs as a list.
#'   It is meant to help interpret the results of motif counts,
#'   such as `netrics::net_x_triads()`.
#'
#'   Either `.data` or `n` (or both) can be given.
#'   When a network is passed to `.data`, the direction, signedness, and mode(s)
#'   of the motifs are inferred from it, as is the number of nodes `n` unless
#'   `n` is also given explicitly.
#'   Passing `n` alongside a network lets you choose, say, the dyadic, triadic,
#'   or tetradic motifs of that kind of network.
#'   When only `n` is given (e.g. `to_motifs(n = 3)`, or simply `to_motifs(3)`),
#'   the `directed`/`signed` arguments are respected instead,
#'   which is handy for teaching or illustration.
#'
#'   For one-mode networks, motifs are implemented undirected for `n = 2` to
#'   `n = 4`, directed for `n = 2` and `n = 3`, signed undirected for `n = 2`
#'   and `n = 3`, and signed directed for `n = 2`.
#'   Where more nodes are requested (or inferred from a larger network) than
#'   are implemented, the largest available motif set is returned.
#'
#'   For two-mode networks, the seven bipartite motifs up to four nodes are
#'   returned, labelled by their `bmotif` dictionary IDs (Simmons et al. 2019):
#'   motif 1 (a single tie), motifs 2-3 (three nodes), and motifs 4-7 (four
#'   nodes, namely the two stars, the 2x2 path, and the 2x2 four-cycle).
#' @section Signed directed dyads:
#'   The six signed directed dyads (`n = 2`, `directed`, `signed`) are the
#'   Holland-Leinhardt dyad census (Null, Asymmetric, Mutual) refined by the
#'   sign of each arc. They correspond to the dyadic reciprocity motifs of
#'   Gallo et al. (2025) as follows:
#'
#'   | `to_motifs()` label | Structure | Gallo et al. (2025) |
#'   | --- | --- | --- |
#'   | `Null` | no ties | (empty dyad) |
#'   | `Asymmetric+` | one positive arc | \eqn{L^{+}_{\rightarrow}} (single positive) |
#'   | `Asymmetric-` | one negative arc | \eqn{L^{-}_{\rightarrow}} (single negative) |
#'   | `Mutual++` | reciprocated, both positive | \eqn{L^{+}_{\leftrightarrow}} (reciprocated positive) |
#'   | `Mutual--` | reciprocated, both negative | \eqn{L^{-}_{\leftrightarrow}} (reciprocated negative) |
#'   | `Mutual+-` | reciprocated, discordant signs | \eqn{L^{\pm}_{\leftrightarrow}} (reciprocated mixed) |
#' @param .data An optional `{manynet}`-consistent network
#'   (matrix, edgelist, igraph, tidygraph, network, or stocnet object),
#'   from which the direction, signedness, mode(s), and (unless `n` is given)
#'   number of nodes of the motifs are inferred.
#'   Either `.data` or `n` must be provided.
#' @param n An optional number of nodes the motifs should contain, as a single
#'   integer (one-mode) or a length-two integer vector (two-mode).
#'   If omitted, it is inferred from `.data`; if given alongside `.data` it
#'   overrides the network's own size (e.g. to list the triadic motifs of a
#'   larger network). Either `.data` or `n` must be provided.
#' @param directed Logical whether the motifs should be directed.
#'   By default FALSE. Ignored (and inferred instead) when `.data` is a network.
#' @param signed Logical whether the motifs should be signed.
#'   By default FALSE. Ignored (and inferred instead) when `.data` is a network.
#'   Currently available for undirected `n = 2` and `n = 3`, and directed
#'   `n = 2` (see the "Signed directed dyads" section).
#' @family modifications
#' @return A named list of `{manynet}`-compatible networks, one per motif.
#' @examples
#'   to_motifs(3)
#'   to_motifs(n = 2, directed = TRUE)
#'   to_motifs(create_ring(8), n = 3)
#'   to_motifs(3, signed = TRUE)
#'   to_motifs(2, directed = TRUE, signed = TRUE)
#' @references
#'   Simmons, Benno I., Michelle J.M. Sweering, Maybritt Schillinger,
#'   Lynn V. Dicks, William J. Sutherland, and Riccardo Di Clemente. 2019.
#'   "bmotif: A package for motif analyses of bipartite networks."
#'   _Methods in Ecology and Evolution_ 10(5): 695-701.
#'   \doi{10.1111/2041-210X.13149}
#'
#'   Gallo, Anna, Fabio Saracco, Renaud Lambiotte, Diego Garlaschelli, and
#'   Tiziano Squartini. 2025.
#'   "Patterns of link reciprocity in directed, signed networks."
#'   _Physical Review E_ 111(2): 024312.
#'   \doi{10.1103/PhysRevE.111.024312}
#' @export
to_motifs <- function(.data = NULL, n = NULL, directed = FALSE, signed = FALSE){
  # A bare number given (positionally) as `.data` is taken to mean `n`,
  # so that e.g. `to_motifs(3)` still lists the three-node motifs.
  if(is.numeric(.data)){
    if(is.null(n)) n <- .data
    .data <- NULL
  }
  if(!is.null(.data)){
    .data <- as_tidygraph(.data)
    directed <- infer_directed(.data, directed)
    signed <- infer_signed(.data, signed)
  }
  n <- infer_n(n, .data)
  if(length(n) > 1){ # two-mode ####
    if(signed)
      return(snet_unavailable("Signed motifs are not yet available for two-mode networks."))
    # The bipartite motifs up to four nodes (Simmons et al. 2019, `bmotif`),
    # labelled by their `bmotif` dictionary IDs. Rows are one mode, columns the
    # other; the 2x2 four-cycle (motif 6) needs `twomode` to disambiguate it
    # from a one-mode adjacency matrix.
    return(list(`1` = .motif_bipartite(matrix(1, 1, 1,
                          dimnames = list("A", "B"))),
                `2` = .motif_bipartite(matrix(c(1, 1), 1, 2,
                          dimnames = list("A", c("B", "C")))),
                `3` = .motif_bipartite(matrix(c(1, 1), 2, 1,
                          dimnames = list(c("A", "B"), "C"))),
                `4` = .motif_bipartite(matrix(c(1, 1, 1), 1, 3,
                          dimnames = list("A", c("B", "C", "D")))),
                `5` = .motif_bipartite(matrix(c(1, 0, 1, 1), 2, 2,
                          dimnames = list(c("A", "B"), c("C", "D")))),
                `6` = .motif_bipartite(matrix(1, 2, 2,
                          dimnames = list(c("A", "B"), c("C", "D")))),
                `7` = .motif_bipartite(matrix(c(1, 1, 1), 3, 1,
                          dimnames = list(c("A", "B", "C"), "D")))))
  }
  if(signed){
    # 2, directed, signed ####
    if(directed){
      if(n>2){
        n <- 2
        snet_info("Signed directed motifs are only available for n=2. Returning n=2 motifs.")
      }
      # The six signed directed dyads: the Holland-Leinhardt dyad census
      # (Null, Asymmetric, Mutual) refined by sign. These map onto the dyadic
      # motifs of Gallo et al. (2025): Asymmetric+/- are their single
      # (anti-reciprocal) L+> / L-> ; Mutual++/--/+- their reciprocated
      # L+<> / L-<> / L(+-)<> .
      return(list(Null = mutate_nodes(create_empty(2, directed = TRUE),
                                      name = c("A","B")),
                  `Asymmetric+` = mutate_ties(create_explicit(A-+B), sign = 1),
                  `Asymmetric-` = mutate_ties(create_explicit(A-+B), sign = -1),
                  `Mutual++` = mutate_ties(create_explicit(A++B), sign = c(1, 1)),
                  `Mutual--` = mutate_ties(create_explicit(A++B), sign = c(-1, -1)),
                  `Mutual+-` = mutate_ties(create_explicit(A++B), sign = c(1, -1))))
    }
    if(n>3){
      n <- 3
      snet_info("Signed motifs are only available for n=2 and n=3. Returning n=3 motifs.")
    }
    # 2, undirected, signed ####
    if(n==2){
      return(list(Null = mutate_nodes(create_empty(2), name = c("A","B")),
                  `+` = mutate_ties(create_explicit(A--B), sign = 1),
                  `-` = mutate_ties(create_explicit(A--B), sign = -1)))
    } else if(n==3){
    # 3, undirected, signed ####
      return(list(Empty = mutate_nodes(create_empty(3), name = c("A","B","C")),
                  `+` = mutate_ties(create_explicit(A--B, C), sign = 1),
                  `-` = mutate_ties(create_explicit(A--B, C), sign = -1),
                  `++` = mutate_ties(create_explicit(A--B--C), sign = c(1, 1)),
                  `+-` = mutate_ties(create_explicit(A--B--C), sign = c(1, -1)),
                  `--` = mutate_ties(create_explicit(A--B--C), sign = c(-1, -1)),
                  `+++` = mutate_ties(create_explicit(A--B--C--A), sign = c(1, 1, 1)),
                  `++-` = mutate_ties(create_explicit(A--B--C--A), sign = c(1, 1, -1)),
                  `+--` = mutate_ties(create_explicit(A--B--C--A), sign = c(1, -1, -1)),
                  `---` = mutate_ties(create_explicit(A--B--C--A), sign = c(-1, -1, -1))))
    } else
      return(snet_unavailable("Signed motifs not yet available for that kind of network."))
  }

  if(n>3 && directed){
    n <- 3
    snet_info("Motifs for directed networks are only available for n=2 and n=3. Returning n=3 motifs.")
  }
  if(n>4 && !directed){
    n <- 4
    snet_info("Motifs for undirected networks are only available for n=2, n=3, and n=4. Returning n=4 motifs.")
  }
  if(!directed & n==2){ # 2, undirected, unsigned ####
    return(list(Null = mutate_nodes(create_empty(2),
                                    name = c("A","B")),
                M = create_explicit(A--B)))
  } else if(directed & n==2){ # 2, directed, unsigned ####
    return(list(Null = mutate_nodes(create_empty(2, directed = TRUE),
                                    name = c("A","B")),
                Asymmetric = create_explicit(A-+B),
                Mutual = create_explicit(A++B)))
  } else if(!directed & n==3){ # 3, undirected, unsigned ####
    return(list(Empty = mutate_nodes(create_empty(3),
                                     name = c("A","B","C")),
                Edge = create_explicit(A--B, C),
                Path = create_explicit(A--B--C),
                Triangle = create_explicit(A--B--C--A)))
  } else if(directed & n==3){ # 3, directed, unsigned ####
    return(list(`003` = mutate_nodes(create_empty(3, directed = TRUE),
                                     name = c("A","B","C")),
                `012` = create_explicit(A-+B, C),
                `102` = create_explicit(A++B, C),
                `021D` = create_explicit(A-+B, A-+C),
                `021U` = create_explicit(A+-B, A+-C),
                `021C` = create_explicit(A-+B, B-+C),
                `111D` = create_explicit(A++B, C-+B),
                `111U` = create_explicit(A++B, B-+C),
                `030T` = create_explicit(A-+B, A-+C, B-+C),
                `030C` = create_explicit(A-+B, B-+C, C-+A),
                `201` = create_explicit(A++B, B++C),
                `120D` = create_explicit(A++B, C-+A:B),
                `120U` = create_explicit(A++B, A:B-+C),
                `120C` = create_explicit(A++B, A-+C-+B),
                `210` = create_explicit(A++B, B++C, A-+C),
                `300` = create_explicit(A++B++C++A)))
  } else if(!directed & n==4){ # 4, undirected, unsigned ####
    return(list(E4 = mutate_nodes(create_empty(4),
                                  name = c("A","B","C","D")),
                I4 = create_explicit(A--B, C, D),
                H4 = create_explicit(A--B, C--D),
                L4 = create_explicit(A--B--C, D),
                D4 = create_explicit(A--B--C--A, D),
                U4 = create_explicit(A--B--C--D),
                Y4 = create_explicit(A--B--C, B--D),
                P4 = create_explicit(A--B--C, B--D--C),
                C4 = create_explicit(A--B--C--D--A),
                Z4 = create_explicit(A--B--C--D--A--C),
                X4 = create_explicit(A--B--C--D--A--C, B--D)))
  } else
    return(snet_unavailable("Motifs not yet available for that kind of network."))
}

#' @rdname modif_motifs
#' @export
create_motifs <- to_motifs

# Build a two-mode motif from an incidence (biadjacency) matrix, forcing the
# two-mode interpretation so that square incidence matrices (e.g. the 2x2
# four-cycle) are not mistaken for one-mode adjacency matrices.
.motif_bipartite <- function(incidence){
  as_tidygraph(as_igraph(incidence, twomode = TRUE))
}
