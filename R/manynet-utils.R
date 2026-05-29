# nocov start

# defining global variables more centrally
utils::globalVariables(c(".data", "obs",
                         "from", "to", "name", "weight","sign","wave","label",
                         "nodes","edges","event","exposure",
                         "student","students","colleges",
                         "node","value","var","active","time",
                         "sender","target","receiver","ego","alter",
                         "increment",
                         "A","B","C","D",
                         "type","id",
                         "n"))

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
  out <- as_matrix(out)
  out[out == 1] <- "*"
  out[out == 0] <- "" 
  as.data.frame(out)
}

collect_functions <- function(pattern, package = "manynet"){
  getNamespaceExports(package)[grepl(pattern, getNamespaceExports(package))]
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

interpolate <- function(values, positions, n, fill = NA) {
  out <- rep(fill, n) 
  out[positions] <- values
  out
}

is.scalar <- function(x) {
  is.atomic(x) && length(x) == 1L
}

preferred_classes <- c("stocnet","tbl_graph","igraph","network","matrix")

as_input <- function(.data, FUN, ...){
  if(!is_manynet(.data))
    snet_abort("{.var {substitute(.data)}} must be a manynet-compatible object.")
  out_class <- class(.data)[1]
  # snet_minor_info("{.var {substitute(.data)}} is of class {.var {out_class}}.")
  fun_label <- as.character(substitute(FUN))   # capture symbol
  avail_classes <- sapply(strsplit(suppressWarnings(utils::methods(fun_label)), 
                                   split = "\\."), "[[", 2)
  avail_class <- stats::na.omit(avail_classes[avail_classes %in% manynet_classes][order(manynet_classes)])[1]
  # snet_minor_info("{.fn {fun_label}} is available for {.var {avail_class}}.")
  out <- get(paste0("as_",avail_class))(.data)
  out <- FUN(out, ...)
  # snet_minor_info("Output is {.var {class(out)[1]}}.",
  #                 "Converting output to {.var {out_class}}.")
  snet_minor_info("Using {.var {avail_class}} method for {.fn {fun_label}} and coercing back to {.var {out_class}}.")
  get(paste0("as_",out_class))(out)
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

# nocov end