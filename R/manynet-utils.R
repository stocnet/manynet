#' @export
available_methods <- function(fun_vctr){
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
