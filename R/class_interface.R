#' Console command line interface
#' @description
#'   These functions wrap `{cli}` functions and elements
#'   to build an attractive command line interface (CLI).
#'   
#'   If you wish to receive fewer messages in the console,
#'   run `options(snet_verbosity = 'quiet')`.
#' @name interface
NULL

#' @rdname interface
#' @export
snet_info <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_alert_info(paste(...), .envir = .envir)
}

#' @rdname interface
#' @export
snet_warn <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_alert_warning(paste(...), .envir = .envir)
}

#' @rdname interface
#' @export
snet_abort <- function(..., .envir = parent.frame()){
  # note that aborts cannot be silenced
  cli::cli_abort(paste(...), .envir = .envir)
}

#' @rdname interface
#' @export
snet_success <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_alert_success(paste(...), .envir = .envir)
}

#' @rdname interface
#' @export
snet_unavailable <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_abort(paste(..., 
                         "If you are interested in this feature,",
                         "please vote for it or raise it as an issue at", 
                         "{.url https://github.com/stocnet/manynet/issues}."), 
                   .envir = .envir)
}

#' @rdname interface
#' @export
snet_progress_step <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_progress_step(..., .envir = .envir)
}

#' @rdname interface
#' @export
snet_progress_along <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_progress_along(..., .envir = .envir)
}

#' @rdname interface
#' @export
snet_progress_seq <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet")
    cli::cli_progress_along(seq.int(...), .envir = .envir, 
                            total = ..., clear = TRUE)
}

#' @rdname interface
#' @export
snet_progress_nodes <- function(..., .envir = parent.frame()){
  if(getOption("snet_verbosity", default = "quiet")!="quiet" && interactive()){
    cli::cli_progress_along(seq.int(net_nodes(...)), .envir = .envir, 
                            total = ..., clear = TRUE)
  } else seq.int(net_nodes(...))
}

