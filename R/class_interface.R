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
#'   - `snet_deprecated()` for an old name of a function, argument, or value.
#'   An old name that is only superseded is reported as `snet_minor_info()`.
#'   - `snet_moved()` for a function that moved to another package.
#'   - `snet_requires()` for a package that a function needs and that is not
#'   installed.
#'   
#'   A call that only reports is silenced under the default verbosity:
#'   
#'   - `snet_info()` for what a function chose on the user's behalf.
#'   - `snet_success()` for the completion of a long task.
#'   - `snet_progress_step()` and the other `snet_progress_*()` functions
#'   for a progress bar.
#'   - `snet_minor_info()` for the bookkeeping a function did on the way,
#'   such as which column it renamed.
#'   
#'   `snet_prompt()` asks the user something, so it always shows.
#' @section Verbosity:
#'   The `snet_verbosity` option takes four levels, each adding one thing to
#'   the one before it:
#'   
#'   - `'quiet'`, the default, reports nothing that is not an error,
#'   a warning, or a prompt.
#'   - `'normal'` adds `snet_info()` and `snet_success()`,
#'   so that a user learns what a function chose on their behalf.
#'   - `'verbose'` adds the progress bars,
#'   which reassure a user that a long task is running.
#'   - `'debug'` adds `snet_minor_info()`,
#'   which reports exhaustively what a function did on the way.
#'   
#'   Set one with, for example, `options(snet_verbosity = 'verbose')`.
#' @param ... One or more character strings.
#'   For most of these functions, if multiple strings are passed these will be
#'   pasted together.
#' @param .envir This argument is just to inherit the parent frame in the 
#'   (likely) event that the function is used within another function.
#' @name interface
NULL

# The verbosity levels, in order, so that a level can be compared with the
# level a message needs. An unrecognised value reads as 'normal', which is
# what every value other than 'quiet' meant before the levels were named.
.snet_levels <- c("quiet", "normal", "verbose", "debug")

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
  if(.snet_verbose("debug"))
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
#' @param old,new The old name of a function, argument, or value,
#'   and the name that replaces it.
#' @param stage How far the old name is on its way out:
#'   - `"superseded"` still works without a word, since a released package
#'   that cannot yet be released again may use it.
#'   It is reported only under the `'debug'` verbosity.
#'   - `"deprecated"` still works, but warns whatever the verbosity, with the
#'   class `"deprecatedWarning"` that [base::.Deprecated()] gives,
#'   so that the warning can be caught or silenced as that one can.
#'   - `"defunct"` no longer works, and aborts with the class
#'   `"defunctError"` that [base::.Defunct()] gives.
#' @param type What the old name names: a `"function"`, an `"argument"`,
#'   or a `"value"` of an argument.
#' @param why An optional sentence saying why the name changed,
#'   or how to get the same result with the new one.
#' @param msg A message to give instead of the one built from `old` and `new`.
#' @export
snet_deprecated <- function(old, new, stage = c("deprecated", "superseded", "defunct"),
                            type = c("function", "argument", "value"),
                            why = NULL, msg = NULL, .envir = parent.frame()){
  stage <- match.arg(stage)
  type <- match.arg(type)
  if(is.null(msg)){
    mark <- switch(type, "function" = "fn", "argument" = "arg", "value" = "code")
    what <- switch(type, "function" = "", "argument" = " argument", "value" = "")
    msg <- paste0("{.", mark, " ", old, "}", what, " is ", stage, ". ",
                  "Use {.", mark, " ", new, "} instead.")
  }
  if(stage == "superseded"){
    # A superseded name only reports, so the verbosity can silence it.
    snet_minor_info(msg, .envir = .envir)
    return(invisible(NULL))
  }
  bullets <- c(msg, if(!is.null(why)) c(i = why))
  if(stage == "defunct")
    cli::cli_abort(bullets, class = "defunctError", .envir = .envir)
  cli::cli_warn(c(bullets, i = "See {.code help('defunct', package = 'manynet')}."),
                class = "deprecatedWarning", .envir = .envir)
}

#' @rdname interface
#' @param new_pkg,old_pkg The package that a function moved to,
#'   and the package it moved from.
#' @param version The version of `old_pkg` in which the function moved.
#' @details
#'   `snet_moved()` tells the user that a function moved to another package,
#'   and how to install that package where it is missing.
#'   By default it then aborts, since the old name is defunct.
#'   With `stage = "deprecated"` it warns instead, so that a shim can go on to
#'   call the function in its new package.
#' @export
snet_moved <- function(old, new, new_pkg = "netrics", version = NULL,
                       old_pkg = "manynet",
                       stage = c("defunct", "deprecated"),
                       .envir = parent.frame()){
  stage <- match.arg(stage)
  since <- if(is.null(version)) paste0("out of {.pkg ", old_pkg, "}") else
    paste0("in {.pkg ", old_pkg, "} ", version)
  bullets <- c(paste0("{.fn ", old, "} moved ", since, " to {.pkg ", new_pkg,
                      "}."),
               i = paste0("Please use {.fn ", new_pkg, "::", new, "} instead."))
  if(!nzchar(system.file(package = new_pkg)))
    bullets <- c(bullets, i = paste0("Install {.pkg ", new_pkg, "} with ",
                                     "{.run install.packages(\"", new_pkg,
                                     "\")}."))
  if(stage == "defunct")
    cli::cli_abort(bullets, class = "defunctError", .envir = .envir)
  cli::cli_warn(bullets, class = "deprecatedWarning", .envir = .envir)
}

#' @rdname interface
#' @param pkg The name of a package that a function needs,
#'   usually one the package only suggests.
#' @param ask Whether to offer to install the package where it is missing.
#'   By default this is offered only in an interactive session,
#'   since a script would stall on the question.
#' @details
#'   `snet_requires()` checks that a package is installed before a function
#'   uses it, and aborts with the command to install it where it is not.
#' @export
snet_requires <- function(pkg, why = "to run this function",
                          ask = interactive(), .envir = parent.frame()){
  if(requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))
  need <- paste0("The {.pkg ", pkg, "} package is required ", why, ".")
  if(isTRUE(ask) &&
     isTRUE(utils::askYesNo(paste0("The ", pkg, " package is required ", why,
                                   ". Would you like to install it from CRAN?")))){
    utils::install.packages(pkg)
    if(requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))
  }
  cli::cli_abort(c(need, i = paste0("Install it with {.run install.packages(\"",
                                    pkg, "\")}.")), .envir = .envir)
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
#'   A progress bar reassures a user that a long task is running,
#'   so it shows from `options(snet_verbosity = 'verbose')` upwards.
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
