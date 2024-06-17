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

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
node_mode <- function(.data) {
  .Deprecated("node_is_mode", package = "migraph",
              old = "node_mode")
  node_is_mode(.data)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
autographr <- function(.data, layout, labels = TRUE,
    node_color, node_shape, node_size, node_group,
    edge_color, edge_size, ...) {
  .Deprecated("graphr", package = "migraph",
              old = "autographr")
  graphr(.data, layout, labels,
         node_color, node_shape, node_size, node_group,
         edge_color, edge_size, ...)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
autographs <- function(netlist, waves, based_on = c("first", "last", "both"), ...) {
  .Deprecated("graphs", package = "migraph",
              old = "autographs")
  graphs(netlist, waves, based_on, ...)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
autographd <- function(tlist, layout, labels = TRUE,
    node_color, node_shape, node_size, edge_color, edge_size,
    keep_isolates = TRUE, ...) {
  .Deprecated("grapht", package = "migraph",
              old = "autographd")
  grapht(tlist, layout, labels,
         node_color, node_shape, node_size, edge_color, edge_size,
         keep_isolates, ...)
}
