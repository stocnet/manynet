# Projecting ####

#' Modifying networks projection
#' @name modif_project
#' @description
#'   These functions offer tools for projecting manynet-consistent data:
#' 
#'   - `to_mode1()` projects a two-mode network to a one-mode network
#'   of the first node set's (e.g. rows) joint affiliations to nodes in the second node set (columns). 
#'   - `to_mode2()` projects a two-mode network to a one-mode network
#'   of the second node set's (e.g. columns) joint affiliations to nodes in the first node set (rows).
#'   - `to_ties()` projects a network to one where the ties become nodes and incident nodes become their ties.
# #'   - `to_galois()` projects a network to its Galois derivation.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(mode[0-9]|ties|galois)"))
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
#' @param similarity Method for establishing ties,
#'   currently "count" (default), "jaccard", or "rand".
#'   
#'   - "count" calculates the number of coinciding ties,
#'   and can be interpreted as indicating the degree of opportunities
#'   between nodes.
#'   - "jaccard" uses this count as the numerator in a proportion,
#'   where the denominator consists of any cell where either node has a tie.
#'   It can be interpreted as opportunity weighted by participation.
#'   - "rand", or the Simple Matching Coefficient,
#'   is a proportion where the numerator consists of the count of cells where
#'   both nodes are present or both are absent,
#'   over all possible cells.
#'   It can be interpreted as the (weighted) degree of behavioral mirroring
#'   between two nodes.
#'   - "pearson" (Pearson's coefficient) and "yule" (Yule's Q)
#'   produce correlations for valued and binary data, respectively.
#'   Note that Yule's Q has a straightforward interpretation related to the odds ratio.
#' @importFrom igraph bipartite_projection
#' @importFrom stats cor
#' @examples
#' to_mode1(ison_southern_women)
#' to_mode2(ison_southern_women)
#' @export
to_mode1 <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode1")

#' @export
to_mode1.default <- function(.data, 
                             similarity = c("count","jaccard","rand","pearson","yule")){
  as_input(.data, to_mode1, similarity = similarity)
}

#' @export
to_mode1.matrix <- function(.data, 
                            similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- .data %*% t(.data)
  b <- .data %*% (1 - t(.data))
  c <- (1 - .data) %*% t(.data)
  d <- ncol(.data) - a - b - c
  out <- switch(similarity,
         "count" = a,
         "jaccard" = a/(a + b + c),
         "rand" = (a + d)/(a + b + c + d),
         "sokalsneath1" = a/(a + 2 * (b + c)),
         "sokalsneath2" = a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),
         "gowerlegendre" = (a - (b + c) + d)/(a + b + c + d),
         "rogerstanimoto" = (a + d)/(a + 2 * (b + c) + d),
         "czekanowski" = 2*a/(2 * a + b + c),
         "ochiai" = a/sqrt((a+b)*(a+c)),
         "pearson" = stats::cor(t(.data)),
         "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode1.igraph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
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
to_mode1.tbl_graph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  out <- as_tidygraph(to_mode1(as_igraph(.data), similarity = similarity))
  if(match.arg(similarity) %in% c("pearson","yule")){
    out <- out |> mutate_ties(sign = dplyr::if_else(tie_weights(out)<0, -1, 1))
  }
  if(!is.null(net_name(.data))) out <- out |> 
      add_info(name = net_name(.data, prefix = "Projection of"))
  if(!is.null(layer_names(.data))) out <- out |> 
      add_info(ties = paste0("co-", layer_names(.data)))
  if(!is.null(mode_names(.data))) out <- out |> 
      add_info(nodes = mode_names(.data)[1],
               ties = paste0("co-", mode_names(.data)[2]))
  out
}

#' @export
to_mode1.network <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
 as_network(to_mode1(as_tidygraph(.data), similarity)) 
}

#' @rdname modif_project
#' @export
to_mode2 <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode2")

#' @export
to_mode2.default <- function(.data, 
                             similarity = c("count","jaccard","rand","pearson","yule")){
  as_input(.data, to_mode2, similarity = similarity)
}

#' @export
to_mode2.matrix <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- t(.data) %*% .data
  b <- t(.data) %*% (1 - .data)
  c <- (1 - t(.data)) %*% .data
  d <- nrow(.data) - a - b - c
  out <- switch(similarity,
                "count" = a,
                "jaccard" = a/(a + b + c),
                "rand" = (a + d)/(a + b + c + d),
                "sokalsneath1" = a/(a + 2 * (b + c)),
                "sokalsneath2" = a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),
                "gowerlegendre" = (a - (b + c) + d)/(a + b + c + d),
                "rogerstanimoto" = (a + d)/(a + 2 * (b + c) + d),
                "czekanowski" = 2*a/(2 * a + b + c),
                "ochiai" = a/sqrt((a+b)*(a+c)),
                "pearson" = stats::cor(.data),
                "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode2.igraph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
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
to_mode2.tbl_graph <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  out <- as_tidygraph(to_mode2(as_igraph(.data), similarity))
  if(match.arg(similarity) %in% c("pearson","yule")){
    out <- out |> mutate_ties(sign = dplyr::if_else(tie_weights(out)<0, -1, 1))
  }
  if(!is.null(net_name(.data))) out <- out |> 
      add_info(name = net_name(.data, prefix = "Projection of"))
  if(!is.null(layer_names(.data))) out <- out |> 
      add_info(ties = paste0("co-", layer_names(.data)))
  if(!is.null(mode_names(.data))) out <- out |> 
      add_info(nodes = mode_names(.data)[2],
               ties = paste0("co-", mode_names(.data)[1]))
  out
}

#' @export
to_mode2.network <- function(.data, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode2(as_tidygraph(.data), similarity)) 
}

#' @rdname modif_project
#' @importFrom igraph make_line_graph E
#' @examples
#' to_ties(ison_adolescents)
#' @export
to_ties <- function(.data) UseMethod("to_ties")

#' @export
to_ties.default <- function(.data){
  as_input(.data, to_ties)
}

#' @export
to_ties.igraph <- function(.data){
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

