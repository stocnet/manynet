#' Layout algorithms based on configurational positions
#' 
#' @name configuration_layouts
#' @family mapping
#' @inheritParams partition_layouts
NULL

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
