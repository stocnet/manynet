#' Many palettes generator
#'
#' @param palette Name of desired palette. Current choices are:
#'   \code{IHEID}, \code{Centres}, \code{SDGs}, \code{ETHZ}, \code{RUG},
#'   and \code{UZH}.
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#' @return A graphic display of colours in palette.
#' @name map_palettes
#' @source Adapted from \url{https://github.com/karthik/wesanderson/blob/master/R/colors.R}
#' @examples
#' many_palettes()
#' #many_palettes("IHEID")
#' @export
many_palettes <- function(palette, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  if (missing(palette)) {
    pal <- corp_palette(c("IHEID", "SDGs", "Centres", "ETHZ", "RUG", "UZH"))
  } else pal <- corp_palette(palette)
  if (is.null(pal))
    cli::cli_abort("Palette not found.")
  if (missing(n)) {
    n <- length(pal)
  }
  if (type == "discrete" && n > length(pal)) {
    cli::cli_abort("Number of requested colors greater than what palette can offer")
  }
  x <- switch(type, continuous = grDevices::colorRampPalette(pal)(n),
              discrete = pal[1:n])
  old <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(graphics::par(old))
  graphics::image(1:n, 1, as.matrix(1:n), col = x,
                  ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  graphics::rect(0, 0.9, n + 1, 1.1, col = grDevices::rgb(1, 1, 1, 0.8),
                 border = NA)
  graphics::text(c(seq_len(n)), 1, labels = attr(x, "name"),
                 cex = 0.9, family = "serif", srt = 90)
}

colorsafe_palette <- c("#d73027", "#4575b4", "#1B9E77","#D95F02","#7570B3",
                       "#E7298A", "#66A61E","#E6AB02","#A6761D","#666666")
