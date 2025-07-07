# defining global variables more centrally
utils::globalVariables(c(".data", "obs",
                         "from", "to", "name", "weight","sign","wave",
                         "nodes","event","exposure",
                         "student","students","colleges",
                         "node","value","var","active","time",
                         "A","B","C","D"))

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
  if (!requireNamespace(pkgname, quietly = TRUE) & interactive()) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      snet_abort(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}

thisRequiresBio <- function(pkgname) {
  if (!requireNamespace(pkgname, quietly = TRUE) & interactive()) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from BioConductor?"))) {
  thisRequires("BiocManager")
  BiocManager::install(pkgname)
  }}
}

#' @export
`+.ggplot` <- function(e1, e2, ...) {
  if (inherits(e2, c("ggplot", "ggplot2::ggplot"))) {
    thisRequires("patchwork")
    patchwork::wrap_plots(e1, e2, ...)
  } else {
    NextMethod()
  }
}

seq_nodes <- function(.data){
  seq.int(net_nodes(.data))
}

# #' @export
# `%||%` <- function(x, y) {
#   if (is_null(x)) y else x
# }
# 
# # Reexport from base on newer versions of R to avoid conflict messages
# if (exists("%||%", envir = baseenv())) {
#   `%||%` <- get("%||%", envir = baseenv())
# }
# 
# `%|0|%` <- function(x, y) {
#   if (!length(x)) y else x
# }
