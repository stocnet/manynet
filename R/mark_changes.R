#' Marking networks change formats
#' @name mark_format_change
#' @description
#'   These functions implement logical tests for various network properties.
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#'   
#'   - `is_longitudinal()` marks networks TRUE if they contain multiple waves of data.
#'   - `is_dynamic()` marks networks TRUE if they contain time-stamped data.
#'   - `is_changing()` marks networks TRUE if they contain any nodal changes.
#' @template param_data
#' @eval detail_avail("is_(longitudinal|dynamic|changing)")
#' @family marks
#' @family changes
NULL

#' @rdname mark_format_change
#' @examples
#' is_longitudinal(create_tree(5, 3))
#' @export
is_longitudinal <- function(.data) UseMethod("is_longitudinal")

#' @export
is_longitudinal.default <- function(.data) {
  is_longitudinal(as_igraph(.data))
}

#' @export
is_longitudinal.igraph <- function(.data) {
  "wave" %in% net_tie_attributes(.data) | 
    "panel" %in% net_tie_attributes(.data)
}

#' @export
is_longitudinal.list <- function(.data) {
  if(is_list(.data)){
    all(lapply(.data, net_nodes)==net_nodes(.data[[1]]))
  } else FALSE
}

#' @rdname mark_format_change
#' @examples 
#' is_dynamic(create_tree(3))
#' @export
is_dynamic <- function(.data) UseMethod("is_dynamic")

#' @export
is_dynamic.default <- function(.data) {
  is_dynamic(as_igraph(.data))
}

#' @export
is_dynamic.igraph <- function(.data) {
  atts <- net_tie_attributes(.data)
  ("time" %in% atts && !all(is.na(tie_attribute(.data, "time")))) | 
    "beg" %in% atts | "begin" %in% atts | "start" %in% atts
}

#' @rdname mark_format_change
#' @examples 
#' is_changing(fict_starwars)
#' @export
is_changing <- function(.data) UseMethod("is_changing")

#' @export
is_changing.default <- function(.data) {
  is_changing(as_igraph(.data))
}

#' @export
is_changing.igraph <- function(.data) {
  "changes" %in% igraph::graph_attr_names(.data)
}

#' @export
is_changing.stocnet <- function(.data) {
  "changes" %in% names(.data) &&
    nrow(.data$changes) > 0
}

#' @export
is_changing.diff_model <- function(.data) {
  is_changing.igraph(as_igraph(.data))
}

