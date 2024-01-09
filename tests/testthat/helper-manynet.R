collect_functions <- function(pattern, package = "manynet"){
  getNamespaceExports(package)[grepl(pattern, getNamespaceExports(package))]
}
