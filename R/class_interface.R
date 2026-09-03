# Information ####
# nocov start
#' Console command line interface
#' @description
#'   These functions wrap `{cli}` functions and elements
#'   to build an attractive command line interface (CLI).
#'   They divide into those that change what a function does
#'   and those that only report what it did.
#'   
#'   A call that changes control flow always fires,
#'   whatever the verbosity, because silencing it would let the code it guards
#'   run on and return a wrong answer instead of an explanation:
#'   
#'   - `snet_abort()` for an error the user has to fix.
#'   - `snet_unavailable()` for a feature that is not yet available.
#'   - `snet_warn()` for a result the user should not trust without reading,
#'   such as a value that is dropped or a name that does not match.
#'   
#'   A call that only reports is silenced under the default verbosity:
#'   
#'   - `snet_info()` for what a function chose on the user's behalf.
#'   - `snet_success()` for the completion of a long task.
#'   - `snet_minor_info()` for detail that is useful only while debugging.
#'   - `snet_progress_step()` and the other `snet_progress_*()` functions
#'   for a progress bar.
#'   
#'   `snet_prompt()` asks the user something, so it always shows.
#' @section Verbosity:
#'   The `snet_verbosity` option takes three levels:
#'   
#'   - `'quiet'`, the default, reports nothing that is not an error,
#'   a warning, or a prompt.
#'   - `'normal'` adds `snet_info()` and `snet_success()`.
#'   - `'verbose'` adds `snet_minor_info()` and the progress bars.
#'   
#'   Set one with, for example, `options(snet_verbosity = 'verbose')`.
#' @param ... One or more character strings.
#'   For most of these functions, if multiple strings are passed these will be
#'   pasted together.
#' @param .envir This argument is just to inherit the parent frame in the 
#'   (likely) event that the function is used within another function.
#' @name interface
NULL

# The three verbosity levels, in order, so that a level can be compared with
# the level a message needs. An unrecognised value reads as 'normal', which is
# what every value other than 'quiet' meant before the levels were named.
.snet_levels <- c("quiet", "normal", "verbose")

.snet_verbose <- function(level = "normal"){
  set <- match(getOption("snet_verbosity", default = "quiet"), .snet_levels)
  if(is.na(set)) set <- 2L
  set >= match(level, .snet_levels)
}

#' @rdname interface
#' @export
snet_info <- function(..., .envir = parent.frame()){
  if(.snet_verbose("normal"))
    cli::cli_alert_info(paste(...), .envir = .envir)
}

#' @rdname interface
#' @export
snet_minor_info <- function(..., .envir = parent.frame()){
  if(.snet_verbose("verbose"))
    cli::cli_alert_info(cli::col_grey(paste(...)), .envir = .envir)
}

#' @rdname interface
#' @export
snet_warn <- function(..., .envir = parent.frame()){
  # A warning tells the user not to trust a result, so it raises a condition
  # they can catch or escalate, and it is not silenced by the verbosity.
  cli::cli_warn(paste(...), .envir = .envir)
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
  if(.snet_verbose("normal"))
    cli::cli_alert_success(paste(...), .envir = .envir)
}

#' @rdname interface
#' @export
snet_prompt <- function(..., .envir = parent.frame()){
  cli::cli_text(cli::style_italic(paste(...)), 
                 .envir = .envir)
}

#' @rdname interface
#' @export
snet_unavailable <- function(..., .envir = parent.frame()){
  # The guard has to abort whatever the verbosity, or the code it guards runs
  # on and returns a wrong answer. Only the invitation depends on the level.
  msg <- paste(...)
  if(!nzchar(msg)) msg <- "That is not yet available."
  if(.snet_verbose("normal"))
    msg <- paste(msg,
                 "If you are interested in this feature,",
                 "please vote for it or raise it as an issue at", 
                 "{.url https://github.com/stocnet/manynet/issues}.")
  cli::cli_abort(msg, .envir = .envir)
}

# Progress ####

#' Sequence and progress functions
#' @description
#'   These functions wrap `{cli}` functions and elements
#'   to build an attractive command line interface (CLI).
#'   
#'   - `snet_progress_step()` for progress steps.
#'   - `snet_progress_along()` for progress along a vector.
#'   - `snet_progress_seq()` for progress along a sequence.
#'   - `snet_progress_nodes()` for progress along the nodes of a network.
#'   
#'   A progress bar reports what a function did and not what it decided,
#'   so it shows only where `options(snet_verbosity = 'verbose')`.
#'   See the verbosity section of [interface].
#' @inheritParams interface
#' @template param_data
#' @name progress
NULL

#' @rdname progress
#' @export
snet_progress_step <- function(..., .envir = parent.frame()){
  if(.snet_verbose("verbose"))
    cli::cli_progress_step(..., .envir = .envir)
}

#' @rdname progress
#' @export
snet_progress_along <- function(..., .envir = parent.frame()){
  if(.snet_verbose("verbose"))
    cli::cli_progress_along(..., .envir = .envir)
}

#' @rdname progress
#' @export
snet_progress_seq <- function(..., .envir = parent.frame()){
  if(.snet_verbose("verbose"))
    cli::cli_progress_along(seq.int(...), .envir = .envir, 
                            total = ..., clear = TRUE)
}

#' @rdname progress
#' @export
snet_progress_nodes <- function(..., .envir = parent.frame()){
  if(.snet_verbose("verbose") && interactive()){
    cli::cli_progress_along(seq.int(net_nodes(...)), .envir = .envir, 
                            total = ..., clear = TRUE)
  } else seq.int(net_nodes(...))
}

#' @rdname progress
#' @export
seq_nodes <- function(.data){
  seq.int(net_nodes(.data))
}

#' @rdname progress
#' @export
seq_ties <- function(.data){
  seq.int(net_ties(.data))
}

# Console theme ####

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
       `.alert-warning` = list(color = "#e6298a", `font-weight` = "bold", before = paste0("!", " ")), 
       `.alert-success` = list(before = function() paste0(col_mnet_green(cli::symbol$tick), " ")), 
       `.alert-info` = list(before = function() paste0(col_mnet_blue(cli::symbol$info), " ")), 
       `.alert-start` = list(before = function() paste0(cli::symbol$arrow_right, " ")), 
       span.pkg = list(color = "#199D77", `font-weight` = "bold"), 
       span.mnet = list(color = "#fda030", `font-weight` = "bold"),
       span.tric = list(color = "#199D77", `font-weight` = "bold"),
       span.auto = list(color = "#d22a20", `font-weight` = "bold"),
       span.infr = list(color = "#4576B5", `font-weight` = "bold"),
       span.migr = list(color = "#e6298a", `font-weight` = "bold"),
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

col_mnet_yellow <- cli::make_ansi_style("#e6ab04")

col_mnet_green <- cli::make_ansi_style("#199D77")

col_mnet_blue <- cli::make_ansi_style("#4576B5")

col_mnet_red <- cli::make_ansi_style("#d22a20")

col_mnet_pink <- cli::make_ansi_style("#e6298a")

.quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink(), add = TRUE) 
  invisible(force(x)) 
}
# nocov end
