# Helper function for declaring available methods
available_methods <- function(fun_vctr) {
  out <- lapply(fun_vctr, function(f) regmatches(utils::.S3methods(f),
                                                 regexpr("\\.", utils::.S3methods(f)),
                                                 invert = TRUE))
  out <- out[lapply(out,length)>0]
  out <- t(as.data.frame(out))
  colnames(out) <- c("from","to")
  rownames(out) <- NULL
  out <- as.data.frame(out)
  as_matrix(out)
}

# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      stop(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}

thisRequiresBio <- function(pkgname) {
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from BioConductor?"))) {
  thisRequires("BiocManager")
  BiocManager::install(pkgname)
  }}
}

#' Open tutorials
#' 
#' @description This function is a wrapper function for learnr::run_tutorial.
#' @details If no argument is declared, function lists the available tutorials.
#' @param tute character string of the tutorial eg. "tutorial3"
#' @export
run_tute <- function(tute) {
  thisRequires("learnr")
  if (missing(tute)) {
    try(learnr::available_tutorials(package = "manynet"), silent = TRUE)
    try(learnr::available_tutorials(package = "migraph"), silent = TRUE)
  } else {
    try(learnr::run_tutorial(tute, "manynet"), silent = TRUE)
    try(learnr::run_tutorial(tute, "migraph"), silent = TRUE)
  }
}

#' Extract code from tutorials
#' 
#' @description This function extracts code chunks from the tutorials.
#' The code is then saved in an R script in the working directory.
#' @details If no argument is declared, function lists the available tutorials.
#' @param tute character string of the tutorial eg. "tutorial3"
#' @export
extract_tute <- function(tute) {
  if (missing(tute)) {
    thisRequires("learnr")
    try(learnr::available_tutorials(package = "manynet"), silent = TRUE)
    try(learnr::available_tutorials(package = "migraph"), silent = TRUE)
  } else {
    thisRequires("knitr")
    pth <- file.path(path.package("manynet"), "tutorials", tute)
    if(!dir.exists(pth)) {
      thisRequires("migraph")
      pth <- file.path(path.package("migraph"), "tutorials", tute)
    }
    knitr::purl(file.path(pth, list.files(pth, pattern = "*.Rmd")),
                documentation = 1)
    utils::file.edit(gsub(".Rmd", ".R", list.files(pth, pattern = "*.Rmd")))
  }
}
