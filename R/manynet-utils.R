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