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

# #' @export
# available_tutes <- function(){
#   mess <- "Available tutorials in `{manynet}`"
#   if(requireNamespace("migraph", quietly = TRUE)) mess <- paste(mess, "and `{migraph}`")
#   message(mess)
#   learnr::available_tutorials(package = "manynet")
#   if(requireNamespace("migraph", quietly = TRUE)) learnr::available_tutorials(package = "migraph")
# }

#' @export
run_tute <- function(tute){
  try(learnr::run_tutorial(tute, "manynet"), silent = TRUE)
  try(learnr::run_tutorial(tute, "migraph"), silent = TRUE)
}

#' @export
extract_tute <- function(tute){
  try(pth <- file.path(path.package("manynet"), "tutorials", tute), silent = TRUE)
  try(pth <- file.path(path.package("migraph"), "tutorials", tute), silent = TRUE)
  knitr::purl(file.path(pth, list.files(pth, pattern = "*.Rmd")), documentation = 1)
  file.edit(gsub(".Rmd", ".R", list.files(pth, pattern = "*.Rmd")))
}

