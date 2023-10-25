# #' Complete list of palettes
# #'
# #' Use \code{\link{iheid_palette}} to construct palettes of desired length.
iheid_palettes <- list("IHEID" = c("IHEIDRed" = "#E20020",
                                   "IHEIDBlack" = "#5c666f",
                                   "IHEIDGrey" = "#6f7072"),
                       "Centres" = c("AHCD" = "#622550",
                                     "CFFD" = "#0094D8",
                                     "CIES" = "#268D2B",
                                     "CTEI" = "#008F92",
                                     "CGEN" = "#820C2B",
                                     "CCDP" = "#3E2682",
                                     "GLGC" = "#006564",
                                     "GLHC" = "#A8086E",
                                     "GLMC" = "#006EAA"),
                       "SDGs" = c("NoPoverty" = "#e5243b",
                                  "ZeroHunger" = "#DDA63A",
                                  "GoodHealth" = "#4C9F38",
                                  "QualityEducation" = "#C5192D",
                                  "GenderEquality" = "#FF3A21",
                                  "CleanWater" = "#26BDE2",
                                  "CleanEnergy" = "#FCC30B",
                                  "EconomicGrowth" = "#A21942",
                                  "Innovation" = "#FD6925",
                                  "ReducedInequalities" = "#DD1367",
                                  "SustainableCities" = "#FD9D24",
                                  "ResponsibleConsumption" = "#BF8B2E",
                                  "ClimateAction" = "#3F7E44",
                                  "BelowWater" = "#0A97D9",
                                  "OnLand" = "#56C02B",
                                  "StrongInstitutions" = "#00689D",
                                  "GoalPartnerships" = "#19486A"))

# An IHEID palette generator

# These are a few color palettes useful for members of the  Geneva Graduate Institute.
# This function calls one of three official palettes in
# \code{\link{iheid_palette}}: for the Institute, for the Centres, and for the
# SDGs.

# #' @param n Number of colors desired. If omitted, uses all colours.
# #' @param name Name of desired palette. Current choices are:
# #'   \code{IHEID}, \code{Centres}, and \code{SDGs}.
# #' @param type Either "continuous" or "discrete". Use continuous if you want
# #'   to automatically interpolate between colours.
# #' @importFrom graphics rect par image text
# #' @return A vector of colours.
# #' @source Adapted from
# \url{https://github.com/karthik/wesanderson/blob/master/R/colors.R}
# #' @keywords colors
# #' @examples
# \donttest{
#iheid_palette("IHEID")
#iheid_palette("Centres")
#iheid_palette("SDGs")
# If you need more colours than normally found in a palette, you
# can use a continuous palette to interpolate between existing colours
#pal <- iheid_palette(21, name = "Centres", type = "continuous")
#image(volcano, col = pal)
# }
iheid_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  pal <- iheid_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  if (missing(n)) {
    n <- length(pal)
  }
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  thisRequires("grDevices")
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

print.palette <- function(x, ...) {
  thisRequires("grDevices")
  thisRequires("graphics")
  n <- length(x)
  old <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(graphics::par(old))
  graphics::image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  graphics::rect(0, 0.9, n + 1, 1.1, col = grDevices::rgb(1, 1, 1, 0.8),
                 border = NA)
  graphics::text((n + 1) / 2, 1, labels = attr(x, "name"),
                 cex = 1, family = "serif")
}

colorsafe_palette <- c("#d73027", "#4575b4", "#1B9E77","#D95F02","#7570B3",
                       "#E7298A", "#66A61E","#E6AB02","#A6761D","#666666")
# colorsafe_palette <- c('#000000','#d73027','#4575b4',
#                            '#f46d43','#74add1','#fee090','#e0f3f8',
#                            '#a50026','#313695',
#                            '#fdae61','#abd9e9')

