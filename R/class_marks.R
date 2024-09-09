make_node_mark <- function(out, .data) {
  class(out) <- c("node_mark", class(out))
  if (is.null(names(out)) && is_labelled(.data))
    names(out) <- node_names(.data)
  attr(out, "mode") <- node_is_mode(.data)
  out
}

make_tie_mark <- function(out, .data) {
  class(out) <- c("tie_mark", class(out))
  if(is_labelled(.data)){
    tie_names <- attr(igraph::E(.data), "vnames")
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
}

#' @export
print.tie_mark <- function(x, ..., n = NULL) {
  print_tblvec(y = as.logical(x),
               names = list(names(x)),
               n = n)
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
    cat(pillar::style_subtle(paste("# ... with",
                                   setup$extra_cols_total,
                                   "more values from this nodeset unprinted.",
                                   "Use `print(..., n = Inf)` to print all values.")))
  } else print(body)
}
