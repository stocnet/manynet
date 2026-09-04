# nocov start
make_node_member <- function(out, .data) {
  if(is.numeric(out))
    out <- MORELETTERS[out]
  if (is_labelled(.data)) names(out) <- node_labels(.data)
  class(out) <- c("node_member", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

# Some algorithms group the ties of a network rather than its nodes, such as
# the link communities of Ahn, Bagrow and Lehmann (2010). Such a partition
# names one group for each tie, and is named by the pair of nodes each tie
# joins. A tie belongs to one group only, so there is no equivalent of the
# 'mode' attribute a node partition carries.
make_tie_member <- function(out, .data) {
  if(is.numeric(out))
    out <- MORELETTERS[out]
  class(out) <- c("tie_member", class(out))
  .name_ties(out, .data)
}

MORELETTERS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

#' @export
print.node_member <- function(x, ..., n = NULL) {
  
  cat(pillar::style_subtle(paste(length(unique(x)), "groups\n")))
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      suppressWarnings(print_tblvec(y = x[attr(x, "mode") == m], 
                   names = list(names(x)[attr(x, "mode") == m]),
                   n = n))
      if(!m) cat("\n")
    }
  } else {
    suppressWarnings(print_tblvec(y = x, 
                 names = list(names(x)),
                 n = n))
  }
  invisible(x)
}

#' @export
print.tie_member <- function(x, ..., n = NULL) {
  cat(pillar::style_subtle(paste(length(unique(x)), "groups\n")))
  suppressWarnings(print_tblvec(y = x,
                                names = list(names(x)),
                                n = n))
  invisible(x)
}

#' @export
summary.tie_member <- function(object, ...,
                               n = 6,
                               digits = 3) {
  for (i in names(table(object))) {
    cat(pillar::style_subtle(paste0("Class ", i, ":")))
    # An object built by `make_tie_member()` names each tie by the pair of
    # nodes it joins. One built another way may hold no names, and its ties
    # are then listed by their place in the network instead.
    if (!is.null(names(object)))
      y <- paste(names(object[object == i]), collapse = ", ")
    else
      y <- paste(which(object == i), collapse = ", ")
    cat(" ", y)
    if (i != names(table(object))[length(table(object))]) cat("\n")
  }
}

#' @export
summary.node_member <- function(object, ...,
                               n = 6,
                               digits = 3) {
  if (any(attr(object, "mode"))) {
    for (i in names(table(object))) {
      if (i == names(table(object))[1]) cat(i, "\n")
      else cat("\n", i, "\n")
      if (!is.null(names(object))) {
        y <- paste(names(object[object == i & attr(object, "mode")]), collapse = ", ")
        z <- paste(names(object[object == i & !attr(object, "mode")]), collapse = ", ")
      } else {
        y <- paste(which(object == i & attr(object, "mode")), collapse = ", ")
        z <- paste(which(object == i & !attr(object, "mode")), collapse = ", ")
      }
      cat("  ", y, "\n")
      cat("  ", z)
    }
  } else {
    for (i in names(table(object))) {
      cat(pillar::style_subtle(paste0("Class ", i, ":")))
      if (!is.null(names(object)))
        y <- paste(names(object[object == i]), collapse = ", ")
      else
        y <- paste(which(object == i), collapse = ", ")
      cat(" ", y)
      if (i != names(table(object))[length(table(object))]) cat("\n")
    }
  }
}

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
# nocov end