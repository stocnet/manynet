#' An IHEID palette generator
#' 
#' These are a few color palettes useful for members of the  Geneva Graduate Institute.
#' This function calls palettes for the Institute, for the Centres, and for the SDGs.
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param palette Name of desired palette. Current choices are:
#'   \code{IHEID}, \code{Centres}, and \code{SDGs}.
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#' @importFrom graphics rect par image text
#' @return A vector of colours.
#' @source Adapted from \url{https://github.com/karthik/wesanderson/blob/master/R/colors.R}
#' @keywords colors
#' @examples
#' #iheid_palette("IHEID")
#' @export
iheid_palette <- function(palette, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  pal <- iheid_corp_palette(palette)
  if (is.null(pal))
    stop("Palette not found.")
  if (missing(n)) {
    n <- length(pal)
  }
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  #thisRequires("grDevices")
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = palette)
}

print.palette <- function(x, ...) {
  #thisRequires("grDevices")
  #thisRequires("graphics")
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
