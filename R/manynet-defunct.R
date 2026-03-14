# nocov start
#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
pkg_data <- function(pkg = "manynet") {
  .Deprecated("table_data", package = "migraph",
              old = "pkg_data")
  table_data(pkg = pkg)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_nodes <- function(.data) {
  .Deprecated("net_nodes", package = "manynet",
              old = "network_nodes")
  net_nodes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_ties <- function(.data) {
  .Deprecated("net_ties", package = "manynet",
              old = "network_ties")
  net_ties(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims <- function(.data) {
  .Deprecated("net_dims", package = "manynet",
              old = "network_dims")
  net_dims(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.data.frame <- function(.data) {
  .Deprecated("net_dims.data.frame", package = "manynet",
              old = "network_dims.data.frame")
  net_dims.data.frame(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.matrix <- function(.data) {
  .Deprecated("net_dims.matrix", package = "manynet",
              old = "network_dims.matrix")
  net_dims.matrix(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.igraph <- function(.data) {
  .Deprecated("net_dims.igraph", package = "manynet",
              old = "network_dims.igraph")
  net_dims.igraph(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.network <- function(.data) {
  .Deprecated("net_dims.network", package = "manynet",
              old = "network_dims.network")
  net_dims.network(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_node_attributes <- function(.data) {
  .Deprecated("net_node_attributes", package = "manynet",
              old = "network_node_attributes")
  net_node_attributes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_tie_attributes <- function(.data) {
  .Deprecated("net_tie_attributes", package = "manynet",
              old = "network_tie_attributes")
  net_tie_attributes(.data)
}

# nocov end