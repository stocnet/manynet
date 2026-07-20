# Rebuild the manynet cheat sheet from cheatsheet.tex.
#
# Requires a LaTeX distribution (pdflatex, with the tcolorbox, fontawesome5,
# inconsolata, tikz, and extsizes packages) and Ghostscript (`gs`) for the PNGs.
# Run from the package root or this directory:
#   Rscript data-raw/cheatsheet/build.R
#
# It compiles the two-page PDF, rasterises one PNG per page (cheatsheet.png and
# cheatsheet2.png), and copies them into the locations used by the installed
# package, the README, and the pkgdown site.

# Locate this folder whether run from the package root or from here.
here <- if (file.exists("cheatsheet.tex")) "." else "data-raw/cheatsheet"
stopifnot(file.exists(file.path(here, "cheatsheet.tex")))
root <- normalizePath(file.path(here, "..", ".."))

owd <- setwd(here)
on.exit(setwd(owd))

# 1. Compile the PDF (twice, so any layout references settle).
for (i in 1:2)
  system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error",
                        "cheatsheet.tex"), stdout = FALSE)

# 2. Rasterise one PNG per page (150 dpi -> ~1650px wide).
pages <- c("cheatsheet.png" = 1, "cheatsheet2.png" = 2)
for (nm in names(pages))
  system2("gs", c("-dSAFER", "-dBATCH", "-dNOPAUSE", "-sDEVICE=png16m",
                  "-r150", "-dGraphicsAlphaBits=4", "-dTextAlphaBits=4",
                  paste0("-dFirstPage=", pages[[nm]]),
                  paste0("-dLastPage=", pages[[nm]]),
                  paste0("-sOutputFile=", nm), "cheatsheet.pdf"))

# 3. Distribute to the installed package, README, and pkgdown site.
#    The PDF (both pages) is the canonical download; the PNGs preview each page.
copies <- list(
  c("cheatsheet.pdf",  file.path(root, "inst", "figures", "cheatsheet.pdf")),
  c("cheatsheet.pdf",  file.path(root, "docs", "reference", "figures", "cheatsheet.pdf")),
  c("cheatsheet.png",  file.path(root, "man", "figures", "cheatsheet.png")),
  c("cheatsheet2.png", file.path(root, "man", "figures", "cheatsheet2.png")),
  c("cheatsheet.png",  file.path(root, "docs", "reference", "figures", "cheatsheet.png")),
  c("cheatsheet2.png", file.path(root, "docs", "reference", "figures", "cheatsheet2.png")))
for (cp in copies) {
  if (!dir.exists(dirname(cp[2]))) next
  file.copy(cp[1], cp[2], overwrite = TRUE)
}

# 4. Tidy LaTeX auxiliaries.
unlink(c("cheatsheet.aux", "cheatsheet.log", "build.log"))
message("Cheat sheet rebuilt and distributed.")
