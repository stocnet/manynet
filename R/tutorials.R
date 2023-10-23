#' Open and extract code from tutorials
#' 
#' @description These functions assist students in using the tutorials
#' in the {manynet} and {migraph} packages.
#' `extract_tute()` extracts code chunks from the tutorials,
#' which is then saved in an R script in the working directory.
#' `run_tute()` is a wrapper function for learnr::run_tutorial().
#' If no argument is declared, function lists the available tutorials.
#' @param tute Name of the tutorial (e.g. "tutorial2").
#' @importFrom dplyr %>% as_tibble select
#' @name tutorials
NULL

#' @describeIn tutorials Extract code from tutorial
#' @examples
#' #extract_tute("tutorial2")
#' @export
extract_tute <- function(tute) {
  if (missing(tute)) {
    thisRequires("learnr")
    t1 <- dplyr::as_tibble(learnr::available_tutorials(package = "manynet"),
                           silent = TRUE) %>% dplyr::select(1:3)
    t2 <- dplyr::as_tibble(learnr::available_tutorials(package = "migraph"),
                           silent = TRUE) %>% dplyr::select(1:3)
    rbind(t1, t2)
  } else {
    thisRequires("knitr")
    pth <- file.path(path.package("manynet"), "tutorials", tute)
    if(!dir.exists(pth)) {
      thisRequires("migraph")
      pth <- gsub("manynet", "migraph", pth)
    }
    knitr::purl(file.path(pth, list.files(pth, pattern = "*.Rmd")),
                documentation = 1)
    utils::file.edit(gsub(".Rmd", ".R", list.files(pth, pattern = "*.Rmd")))
  }
}

#' @describeIn tutorials Open tutorial.
#' @examples
#' #run_tute("tutorial2")

#' @export
run_tute <- function(tute) {
  thisRequires("learnr")
  if (missing(tute)) {
    t1 <- dplyr::as_tibble(learnr::available_tutorials(package = "manynet"),
                           silent = TRUE) %>% dplyr::select(1:3)
    t2 <- dplyr::as_tibble(learnr::available_tutorials(package = "migraph"),
                           silent = TRUE) %>% dplyr::select(1:3)
    rbind(t1, t2)
  } else {
    try(learnr::run_tutorial(tute, "manynet"), silent = TRUE)
    try(learnr::run_tutorial(tute, "migraph"), silent = TRUE)
  }
}
