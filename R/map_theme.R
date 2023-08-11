#' Theming ggraph
#' 
#' @description
#'   These functions enable graphs to be easily and quickly themed, 
#'   e.g. changing the default colour of the graph's vertices and edges.
#'   Note that, unlike typical `{ggplot2}` theming, these functions
#'   are passed the current plot with `%>%` rather than `+`.
#'   For example, `autographr(ison_konigsberg) %>% theme_iheid()`.
#' @name themes
#' @param ggraph A ggraph object, e.g. created using `autographr()`
#' @return Themes the current ggraph to current IHEID guidelines.
NULL

#' @rdname themes
#' @export
theme_iheid <- function(ggraph) {
  ggraph[[2]][[1]]$geom$default_aes$colour <- iheid_palette("IHEID")["IHEIDBlack"] # Text colour
  ggraph[[2]][[2]]$geom$default_aes$edge_colour <- iheid_palette("IHEID")["IHEIDGrey"] # Edge colour
  ggraph[[2]][[3]]$geom$default_aes$colour <- iheid_palette("IHEID")["IHEIDRed"] # Vertex colour
  ggraph
}
