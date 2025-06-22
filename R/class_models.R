make_diff_model <- function(events, report, .data) {
  class(report) <- c("diff_model", class(report))
  attr(report, "events") <- events
  if(is_list(.data)){
    attr(report, "mode") <- node_is_mode(.data[[1]])
    attr(report, "network") <- .data
  } else {
    attr(report, "mode") <- node_is_mode(.data)
    attr(report, "network") <- as_tidygraph(.data)
  }
  report
}

#' @export
print.diff_model <- function(x, ..., verbose = FALSE){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  if(!verbose){
    x$n <- NULL
    x$s <- NULL
    x$I_new <- NULL
    x$E_new <- NULL
    x$R_new <- NULL
  }
  print(dplyr::tibble(x, ...))
}

#' @export
summary.diff_model <- function(object, ...) {
  dplyr::tibble(attr(object, "events"), ...)
}

# learn_model ####
make_learn_model <- function(out, .data) {
  out <- as.data.frame(out)
  if(is_labelled(.data))
    names(out) <- node_names(.data)
  class(out) <- c("learn_model", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

#' @export
print.learn_model <- function(x, ...){
  print(dplyr::tibble(x))
}

#' @export
summary.learn_model <- function(object, ..., epsilon = 0.0005) {
  steps <- nrow(object)
  max_belief <- max(object[steps,])
  min_belief <- min(object[steps,])
  if(abs(max_belief - min_belief) < epsilon){
    cat(paste(steps-1, 
              "steps to convergence.\n"))
    cat("Final belief =", max_belief)
  } else 
    cat(paste("No convergence after",
              steps-1, "steps."))
}

