#' Theming ggraph
#' 
#' @name themes
#' @param ggraph A ggraph object, e.g. created using `autographr()`
#' @return Themes the current ggraph to current IHEID guidelines.
#' @examples
#'  autographr(ison_konigsberg) %>% theme_iheid()
NULL

#' @rdname themes
#' @export
theme_iheid <- function(ggraph) {
  
  gg[[2]][[1]]$geom$default_aes$colour <- iheid_palette("IHEID")["IHEIDBlack"] # Text colour
  gg[[2]][[2]]$geom$default_aes$edge_colour <- iheid_palette("IHEID")["IHEIDGrey"] # Edge colour
  gg[[2]][[3]]$geom$default_aes$colour <- iheid_palette("IHEID")["IHEIDRed"] # Vertex colour
  gg
}
