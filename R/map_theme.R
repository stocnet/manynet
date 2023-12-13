#' Many themes
#' 
#' @description
#'   These functions enable graphs to be easily and quickly themed, 
#'   e.g. changing the default colour of the graph's vertices and edges.
#' @name themes
#' @param base_size Font size, by default 12.
#' @param base_family Font family, by default "sans".
#' @examples
#' # autographr(to_mentoring(ison_brandes)) + 
#' # labs(title = "Who leads and who follows?") +
#' # theme_iheid()
NULL

#' @rdname themes
#' @export
theme_iheid <- function(base_size = 12, base_family = "Times") {
  colors <-  corp_palette("IHEID")
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      line = ggplot2::element_line(colors["IHEIDBlack"]),
      rect = ggplot2::element_rect(fill = colors["IHEIDGrey"],
                          linetype = 0, colour = NA),
      title = ggplot2::element_text(colour = colors["IHEIDRed"],
                                    size = base_size,
                           face = "bold"),
      plot.subtitle = ggplot2::element_text(colors["IHEIDGrey"],
                                   size = base_size*0.85, family = "Times",
                                   face = "bold"),
      plot.caption = ggplot2::element_text(colors["IHEIDBlack"],
                                           size = base_size*0.75,
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
      plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5),
                                         face = "bold"),
      strip.background = ggplot2::element_rect()))
}

#' @rdname themes
#' @export
theme_eth <- function(base_size = 12, base_family = "serif") {
  colors <-  corp_palette("ETH")
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      line = ggplot2::element_line(colors["ETH_Blue"]),
      rect = ggplot2::element_rect(fill = colors["ETH_Petrol"],
                                   linetype = 0, colour = NA),
      title = ggplot2::element_text(colour = colors["ETH_Green"],
                                    size = base_size,
                                    face = "bold"),
      plot.subtitle = ggplot2::element_text(colors["ETH_Bronze"],
                                            size = base_size*0.85,
                                            family = "serif",
                                            face = "bold"),
      plot.caption = ggplot2::element_text(colors["ETH_Red"],
                                           size = base_size*0.75,
                                           family = "serif",
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
      plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5),
                                         face = "bold"),
      strip.background = ggplot2::element_rect()))
}

#' @rdname themes
#' @export
theme_uzh <- function(base_size = 12, base_family = "sans") {
  colors <-  corp_palette("EZH")
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      line = ggplot2::element_line(colors["UZH_Blue"]),
      rect = ggplot2::element_rect(fill = colors["UZH_Grey"],
                                   linetype = 0, colour = NA),
      title = ggplot2::element_text(colour = colors["UZH_Orange"],
                                    size = base_size,
                                    face = "bold"),
      plot.subtitle = ggplot2::element_text(colors["UZH_Blue"],
                                            size = base_size*0.85,
                                            family = "sans",
                                            face = "bold"),
      plot.caption = ggplot2::element_text(colors["UZH_Grey"],
                                           size = base_size*0.75,
                                           family = "sans",
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
      plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5),
                                         face = "bold"),
      strip.background = ggplot2::element_rect()))
}

#' @rdname themes
#' @export
theme_ug <- function(base_size = 12, base_family = "mono") {
  colors <-  corp_palette("UG")
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      line = ggplot2::element_line(colors["UG_Red"]),
      rect = ggplot2::element_rect(fill = colors["UG_Black"],
                                   linetype = 0, colour = NA),
      title = ggplot2::element_text(colour = colors["UG_Red"],
                                    size = base_size,
                                    face = "bold"),
      plot.subtitle = ggplot2::element_text(colors["UG_Black"],
                                            size = base_size*0.85,
                                            family = "mono",
                                            face = "bold"),
      plot.caption = ggplot2::element_text(colors["UG_Red"],
                                           size = base_size*0.75,
                                           family = "mono",
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
      plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5),
                                         face = "bold"),
      strip.background = ggplot2::element_rect()))
}

#' Many scales
#' 
#' @description These functions enable to add color scales to be graphs.
#' @name scales
#' @param direction Direction for using palette colors.
#' @param ... Extra arguments passed to `ggplot2::discrete_scale()`.
#' @examples
#' #ison_brandes %>%
#' #mutate(core = migraph::node_is_core(ison_brandes)) %>%
#' #autographr(node_color = "core") +
#' #scale_color_iheid()
NULL

#' @rdname scales
#' @export
scale_fill_iheid <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill", "IHEID",
                          palette_gen(palette = "IHEID", direction), ...)
}

#' @rdname scales
#' @export
scale_colour_iheid <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour", "IHEID",
                          palette_gen(palette = "IHEID", direction), ...)
}

#' @rdname scales
#' @export
scale_color_iheid <- scale_colour_iheid

#' Centres color scales
#' 
#' @rdname scales
#' @export
scale_fill_centres <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill", "Centres",
                          palette_gen(palette = "Centres", direction), ...)
}

#' @rdname scales
#' @export
scale_colour_centres <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour", "Centres",
                          palette_gen(palette = "Centres", direction), ...)
}

#' @rdname scales
#' @export
scale_color_centres <- scale_colour_centres

#' SDGs color scales
#' 
#' @rdname scales
#' @export
scale_fill_sdgs <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill", "SDGs",
                          palette_gen(palette = "SDGs", direction), ...)
}

#' @rdname scales
#' @export
scale_colour_sdgs <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour", "SDGs",
                          palette_gen(palette = "SDGs", direction), ...)
}

#' @rdname scales
#' @export
scale_color_sdgs <- scale_colour_sdgs

#' ETH color scales
#' 
#' @rdname scales
#' @export
scale_fill_eth <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill", "ETH",
                          palette_gen(palette = "ETH", direction), ...)
}

#' @rdname scales
#' @export
scale_colour_eth <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour", "ETH",
                          palette_gen(palette = "ETH", direction), ...)
}

#' @rdname scales
#' @export
scale_color_eth <- scale_colour_eth

#' UZH color scales
#' 
#' @rdname scales
#' @export
scale_fill_uzh <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill", "UZH",
                          palette_gen(palette = "UZH", direction), ...)
}

#' @rdname scales
#' @export
scale_colour_uzh <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour", "UZH",
                          palette_gen(palette = "UZH", direction), ...)
}

#' @rdname scales
#' @export
scale_color_uzh <- scale_colour_uzh

#' UG color scales
#' 
#' @rdname scales
#' @export
scale_fill_ug <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill", "UG",
                          palette_gen(palette = "UG", direction), ...)
}

#' @rdname scales
#' @export
scale_colour_ug <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour", "UG",
                          palette_gen(palette = "UG", direction), ...)
}

#' @rdname scales
#' @export
scale_color_ug <- scale_colour_ug

# Helper functions
corp_color <- function(...) {
  corp_colors <- c(`IHEIDRed` = "#E20020", `IHEIDBlack` = "#5c666f",
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
                   `GoalPartnerships` = "#19486A",
                   `ETH_Blue` = "#215CAF", `ETH_Petrol` = "#007894",
                   `ETH_Green` = "#627313", `ETH_Bronze` = "#8E6713",
                   `ETH_Red`=	"#B7352D", `ETH_Purple` = "#A7117A",
                   `ETH_Grey`	= "#6F6F6F", `UZH_Blue` = "#0028a5",
                   `UZH_Grey` = "#a3adb7", `UZH_Orange` = "#dc6027",
                   `UG_Red` = "#dc002d", `UG_White` = "#FFFFFF",
                   `UG_Black` = "#000000")
  cols <- c(...)
  if (is.null(cols))
    return (corp_colors)
  corp_colors[cols]
}

corp_palette <- function(palette, ...) {
  corp_palettes <- list(`IHEID` = corp_color("IHEIDRed",
                                             "IHEIDBlack",
                                             "IHEIDGrey"),
                        `Centres` = corp_color("AHCD", "CFFD", 
                                               "CIES", "CTEI",
                                               "CGEN", "CCDP",
                                               "GLGC", "GLHC",
                                               "GLMC"),
                        `SDGs` = corp_color("NoPoverty",
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
                                            "GoalPartnerships"),
                        `ETH` = corp_color("ETH_Blue", "ETH_Petrol",
                                           "ETH_Green", "ETH_Bronze",
                                           "ETH_Red", "ETH_Purple", "ETH_Grey"),
                        `UZH` = corp_color("UZH_Blue", "UZH_Grey", "UZH_Orange"),
                        `UG` = corp_color("UG_Red", "UG_White", "UG_Black"))
  corp_palettes[[palette]]
}

palette_gen <- function(palette, direction = 1) {
  function(n) {
    if (n > length(corp_palette(palette)))
      warning("Not enough colors in this palette!")
    else {
      all_colors <- corp_palette(palette)
      all_colors <- unname(unlist(all_colors))
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      color_list <- all_colors[1:n]
    }
  }
}
