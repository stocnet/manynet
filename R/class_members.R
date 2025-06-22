make_node_member <- function(out, .data) {
  if(is.numeric(out))
    out <- MORELETTERS[out]
  if (is_labelled(.data)) names(out) <- node_names(.data)
  class(out) <- c("node_member", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
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
