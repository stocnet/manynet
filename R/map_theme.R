#' Many themes
#' 
#' @description
#'   These functions enable graphs to be easily and quickly themed, 
#'   e.g. changing the default colour of the graph's vertices and edges.
#' @name themes
#' @param base_size Font size, by default 12.
#' @param base_family Font family, by default "sans".
#' @return Themes the current ggraph to current IHEID guidelines.
NULL

#' @rdname themes
#' @examples
#' # autographr(to_mentoring(ison_brandes)) + 
#' # labs(title = "Who leads and who follows?") +
#' # theme_iheid()
#' @export
theme_iheid <- function(base_size = 12, base_family = "sans") {
  colors <-  iheid_corp_palette("IHEID")
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      line = ggplot2::element_line(colors["IHEIDBlack"]),
      rect = ggplot2::element_rect(fill = colors["IHEIDGrey"],
                          linetype = 0, colour = NA),
      title = element_text(colour = colors["IHEIDRed"], size = base_size,
                           face = "bold"),
      plot.subtitle = element_text(colors["IHEIDGrey"],
                                   size = base_size*0.85, family = "Times",
                                   face = "bold"),
      plot.caption = element_text(colors["IHEIDBlack"], size = base_size*0.75,
                                  family = "Times",
                                  face = "italic"),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5), face = "bold"),
      strip.background = ggplot2::element_rect()))
}

iheid_corp_color <- function(...) {
  iheid_corp_colors <- c(`IHEIDRed` = "#E20020", `IHEIDBlack` = "#5c666f",
                         `IHEIDGrey` = "#6f7072", `AHCD` = "#622550",
                         `CFFD` = "#0094D8", `CIES` = "#268D2B",
                         `CTEI` = "#008F92", `CGEN` = "#820C2B",
                         `CCDP` = "#3E2682", `GLGC` = "#006564",
                         `GLHC` = "#A8086E", `GLMC` = "#006EAA",
                         `NoPoverty` = "#e5243b", `ZeroHunger` = "#DDA63A",
                         `GoodHealth` = "#4C9F38",
                         `QualityEducation` = "#C5192D",
                         `GenderEquality` = "#FF3A21", `CleanWater` = "#26BDE2",
                         `CleanEnergy` = "#FCC30B",
                         `EconomicGrowth` = "#A21942",
                         `Innovation` = "#FD6925",
                         `ReducedInequalities` = "#DD1367",
                         `SustainableCities` = "#FD9D24", 
                         `ResponsibleConsumption` = "#BF8B2E",
                         `ClimateAction` = "#3F7E44", `BelowWater` = "#0A97D9",
                         `OnLand` = "#56C02B", `StrongInstitutions` = "#00689D",
                         `GoalPartnerships` = "#19486A")
  cols <- c(...)
  if (is.null(cols))
    return (iheid_corp_colors)
  iheid_corp_colors[cols]
}

iheid_corp_palette <- function(palette = "IHEID", ...) {
  iheid_corp_palettes <- list(`IHEID` = iheid_corp_color("IHEIDRed",
                                                         "IHEIDBlack",
                                                         "IHEIDGrey"),
                              `Centres` = iheid_corp_color("AHCD", "CFFD", 
                                                           "CIES", "CTEI",
                                                           "CGEN", "CCDP",
                                                           "GLGC", "GLHC",
                                                           "GLMC"),
                              `SDGs` = iheid_corp_color("NoPoverty",
                                                        "ZeroHunger",
                                                        "GoodHealth", 
                                                        "QualityEducation",
                                                        "GenderEquality", 
                                                        "CleanWater",
                                                        "CleanEnergy",
                                                        "EconomicGrowth",
                                                        "Innovation", 
                                                        "ReducedInequalities",
                                                        "SustainableCities",
                                                        "ResponsibleConsumption",
                                                        "ClimateAction", 
                                                        "BelowWater", 
                                                        "OnLand",
                                                        "StrongInstitutions",
                                                        "GoalPartnerships"))
  iheid_corp_palettes[[palette]]
}

palette_gen <- function(palette = "IHEID", direction = 1) {
  function(n) {
    if (n > length(iheid_corp_palette(palette)))
      warning("Not enough colors in this palette!")
    else {
      all_colors <- iheid_corp_palette(palette)
      all_colors <- unname(unlist(all_colors))
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      color_list <- all_colors[1:n]
    }
  }
}

#' @rdname themes
#' @param palette Name of the palette. Current choices are:
#'   \code{IHEID}, \code{Centres}, and \code{SDGs}.
#' @param direction Direction for using palette colors.
#' @param ... Extra arguments passed to `ggplot2::discrete_scale()`.
#' @examples
#' #ison_brandes %>%
#' #mutate(core = migraph::node_is_core(ison_brandes)) %>%
#' #autographr(node_color = "core") +
#' #scale_color_iheid()
#' @export
scale_fill_iheid <- function(palette = "IHEID", direction = 1, ...) {
  ggplot2::discrete_scale("fill", "IHEID",
                          palette_gen(palette, direction), ...)
}

#' @rdname themes
#' @export
scale_colour_iheid <- function(palette = "IHEID", direction = 1, ...) {
  ggplot2::discrete_scale("colour", "IHEID",
                          palette_gen(palette, direction), ...)
}

#' @rdname themes
#' @export
scale_color_iheid <- scale_colour_iheid

#' Centres color scales
#' 
#' @rdname themes
#' @export
scale_fill_centres <- function(palette = "Centres", direction = 1, ...) {
  ggplot2::discrete_scale("fill", "Centres",
                          palette_gen(palette, direction), ...)
}

#' @rdname themes
#' @export
scale_colour_centres <- function(palette = "Centres", direction = 1, ...) {
  ggplot2::discrete_scale("colour", "Centres",
                          palette_gen(palette, direction), ...)
}

#' @rdname themes
#' @export
scale_color_centres <- scale_colour_centres

#' SDGs color scales
#' 
#' @rdname themes
#' @export
scale_fill_sdgs <- function(palette = "SDGs", direction = 1, ...) {
  ggplot2::discrete_scale("fill", "SDGs",
                          palette_gen(palette, direction), ...)
}

#' @rdname themes
#' @export
scale_colour_sdgs <- function(palette = "SDGs", direction = 1, ...) {
  ggplot2::discrete_scale("colour", "SDGs",
                          palette_gen(palette, direction), ...)
}

#' @rdname themes
#' @export
scale_color_sdgs <- scale_colour_sdgs
