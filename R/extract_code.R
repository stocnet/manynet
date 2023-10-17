#' Extracting code from tutorials
#'
#' @description
#' This is a helper function for extracting the code chunks from the tutorials.
#' 
#' @param package either "manynet" for tutorials 0, 1, and 2, or "migraph" for tutorial 3 onwards. 
#' @param number the number of the tutorial, eg. 4 for tutorial4
#' @param title a character string of the title of the tutorial, eg. "centrality" for tutorial 3.
#'
#' @return an R script in .R file that is saved to your working directory.
#' @examples
#' extract_code(package = "migraph", number = 4, title = "community")
#' 
#' @export
extract_code <- function(package,
                         number,
                         title) {
  pkg <-  as.character(package)
  n <- as.character(number)
  x <- as.character(title)
  knitr::purl(input = paste0(path.package(pkg),
                             "/tutorials/tutorial", n, "/", x, ".Rmd"),
              output = paste0("Tutorial", n, ".R"),
              documentation = 0)
}

