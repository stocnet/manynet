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