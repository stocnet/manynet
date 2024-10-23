options(manynet_verbosity = "quiet")

collect_functions <- function(pattern, package = "manynet"){
  getNamespaceExports(package)[grepl(pattern, getNamespaceExports(package))]
}

expect_values <- function(object, ref) {
  # 1. Capture object and label
  # act <- quasi_label(rlang::enquo(object), arg = "object")
  act <- list(val = object, label = deparse(substitute(object)))
  
  # 2. Call expect()
  act$n <- unname(unlist(act$val))
  ref <- unname(unlist(ref))
  expect(
    act$n == ref,
    sprintf("%s has values %i, not values %i.", act$lab, act$n, ref)
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
}

top3 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:3]  
  } else unname(res)[1:3]
}

bot3 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-2):lr]
  } else unname(res)[(lr-2):lr]
}

top5 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:5]
  } else unname(res)[1:3]
}

bot5 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-4):lr]
  } else unname(res)[(lr-2):lr]
}
