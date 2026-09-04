# nocov start
make_node_mark <- function(out, .data) {
  class(out) <- c("node_mark", class(out))
  if (is.null(names(out)) && is_labelled(.data))
    names(out) <- node_labels(.data)
  attr(out, "mode") <- node_is_mode(.data)
  out
}

make_tie_mark <- function(out, .data) {
  class(out) <- c("tie_mark", class(out))
  .name_ties(out, .data)
}

# A tie has no name of its own, so it is named by the pair of nodes it joins.
# Every tie class names its ties this way, so the three constructors share
# this one function.
.name_ties <- function(out, .data) {
  # A network without ties has no tie names to give the object either.
  if(length(out) == 0) return(out)
  # A stocnet holds its ties in its own table, and coercion may reciprocate
  # its undirected layers, so name from that table rather than a coerced copy.
  if(inherits(.data, "stocnet")){
    from <- .data$ties$from
    to <- .data$ties$to
    if(is_labelled(.data)){
      from <- .data$nodes$label[from]
      to <- .data$nodes$label[to]
    }
    names(out) <- paste0(from, if(is_directed(.data)) "->" else "-", to)
    return(out)
  }
  if(is_labelled(.data)){
    tie_names <- attr(igraph::E(as_igraph(.data)), "vnames")
    if(is_directed(.data))
      names(out) <- gsub("\\|", "->", tie_names) else
        names(out) <- gsub("\\|", "-", tie_names)
  } else {
    ties <- as_edgelist(.data)[,1:2]
    if(is_directed(.data))
      names(out) <- paste0(ties$from, "->", ties$to) else
        names(out) <- paste0(ties$from, "-", ties$to)
  }
  out
}

#' @export
print.node_mark <- function(x, ..., n = NULL) {
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      print_tblvec(y = as.logical(x)[attr(x, "mode") == m],
                   names = list(names(x)[attr(x, "mode") == m]),
                   n = n)
      if (!m) cat("\n")
    }
  } else {
    print_tblvec(y = as.logical(x),
                 names = list(names(x)), n = n)
  }
  invisible(x)
}

#' @export
print.tie_mark <- function(x, ..., n = NULL) {
  print_tblvec(y = as.logical(x),
               names = list(names(x)),
               n = n)
  invisible(x)
}

# make tblvec ####
#' @importFrom pillar tbl_format_setup tbl_format_body style_subtle
print_tblvec <- function(y, names, n){
  mat <- matrix(y, dimnames = names)
  mat <- t(mat)
  out <- as.data.frame(mat)
  tibs <- dplyr::tibble(out, .name_repair = "minimal")
  setup <- pillar::tbl_format_setup(tibs, width = n)
  body <- pillar::tbl_format_body(tibs, setup)[c(TRUE, FALSE, TRUE)]
  if(setup$extra_cols_total > 0){
    print(body)
    cat(pillar::style_subtle(paste("# ... and",
                                   setup$extra_cols_total,
                                   "more values from this nodeset.",
                                   "Use `print_all(...)` to print all values.")))
  } else print(body)
}
# nocov end