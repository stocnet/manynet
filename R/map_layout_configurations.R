#' Layout algorithms based on configurational positions
#' 
#' @description
#'   Configurational layouts locate nodes at symmetric coordinates
#'   to help illustrate the particular layouts.
#'   Currently "triad" and "quad" layouts are available.
#'   The "configuration" layout will choose the appropriate configurational
#'   layout automatically.
#' 
#' @name configuration_layouts
#' @family mapping
#' @inheritParams partition_layouts
NULL

#' @rdname configuration_layouts
#' @export
layout_tbl_graph_configuration <- function(.data,
                                   circular = FALSE, times = 1000){
  if (net_nodes(.data) == 3) {
    layout_tbl_graph_triad(.data, circular = circular, times = times)
  } else if (net_nodes(.data) == 4) {
    layout_tbl_graph_quad(.data, circular = circular, times = times)
}}

#' @rdname configuration_layouts
#' @export
layout_tbl_graph_triad <- function(.data,
                                   circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  2,3.5,
                  4,0), 3, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname configuration_layouts
#' @export
layout_tbl_graph_quad <- function(.data,
                                   circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  0,1,
                  1,0,
                  1,1), 4, 2, byrow = TRUE)
  .to_lo(res)  
}
