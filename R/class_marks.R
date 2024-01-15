
make_tie_mark <- function(out, .data) {
  class(out) <- c("tie_mark", class(out))
  out
}


#' @export
print.tie_mark <- function(x, ..., n = NULL) {
  print_tblvec(y = as.logical(x),
               names = list(names(x)),
               n = n)
}

