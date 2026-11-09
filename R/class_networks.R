#' @rdname make_stocnet
#' @export
print_all <- function(x, ...) {
  print(x, n = Inf)
}

is_grand <- function(.data){
  !is.null(igraph::graph_attr(.data, "grand"))
}
