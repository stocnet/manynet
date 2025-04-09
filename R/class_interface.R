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

manynet_console_theme <- function(){
  # dark <- detect_dark_theme(dark)
  list(h1 = list(`margin-top` = 1, `margin-bottom` = 0, color = "#D83127", 
                 fmt = function(x) cli::rule(x, line_col = "#D83127")), 
       h2 = list(`margin-top` = 1, `margin-bottom` = 0, color = "#199D77", 
                 fmt = function(x) paste0(cli::symbol$line, " ", x, " ", cli::symbol$line, cli::symbol$line)), 
       h3 = list(`margin-top` = 1, `margin-bottom` = 0, color = "#199D77"), 
       par = list(`margin-top` = 0, `margin-bottom` = 1), 
       `.alert-danger` = list(`background-color` = "#D83127", color = "white", 
                              before = function() paste0(cli::symbol$cross, " ")), 
       `.alert-warning` = list(color = "#E6AB02", `font-weight` = "bold", before = paste0("!", " ")), 
       `.alert-success` = list(before = function() paste0(col_mnet_green(cli::symbol$tick), " ")), 
       `.alert-info` = list(before = function() paste0(col_mnet_blue(cli::symbol$info), " ")), 
       `.alert-start` = list(before = function() paste0(cli::symbol$arrow_right, " ")), 
       span.pkg = list(color = "#199D77", `font-weight` = "bold"), 
       span.version = list(color = "#D83127"), 
       span.emph = list(color = "#D83127"), 
       span.strong = list(`font-weight` = "bold", `font-style` = "italic"), 
       span.fun = utils::modifyList(simple_theme_code(), 
                                    list(after = "()")), 
       span.fn = utils::modifyList(simple_theme_code(),
                                   list(after = "")), 
       span.arg = simple_theme_code(), 
       span.kbd = utils::modifyList(simple_theme_code(), 
                                    list(before = "<", after = ">")), 
       span.key = utils::modifyList(simple_theme_code(), 
                                    list(before = "<", after = ">")), 
       span.file = list(color = "#4576B5"), 
       span.path = list(color = "#4576B5"), 
       span.email = list(color = "#4576B5"), 
       span.url = utils::modifyList(list(color = "#4576B5"), list(before = "<", 
                                                                  after = ">")), 
       span.var = simple_theme_code(), 
       span.envvar = simple_theme_code(), 
       span.timestamp = list(before = "[", after = "]", color = "grey"))
}

simple_theme_code <- function(){
  # if (dark) { # Can't detect dark themes without rstudioapi dependency...
  #   list(`background-color` = "#232323", color = "#f0f0f0")
  # }
  # else {
  list(`background-color` = "#f8f8f8", color = "#202020")
  # }
}

col_mnet_green <- cli::make_ansi_style("#199D77")

col_mnet_blue <- cli::make_ansi_style("#4576B5")

.quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}
