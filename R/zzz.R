#' @importFrom cli cli_div cli_inform cli_end
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()
  
  options(manynet_verbosity = getOption("manynet_verbosity", "verbose"))
  # options(manynet_theme = getOption("manynet_theme", "default"))
  options(cli.theme = manynet_console_theme())
  
  # pkgs <- as.data.frame(utils::available.packages(utils::contrib.url(getOption("repos"))))
  # 
  # cran_version <- pkgs[pkgs$Package == "manynet","Version"]

  local_version <- utils::packageVersion("manynet")
  cli::cli_inform("You are using {.pkg manynet} version {.version {local_version}}.", 
                  class = "packageStartupMessage")
  old.list <- as.data.frame(utils::old.packages())
  behind_cran <- "manynet" %in% old.list$Package
  
  greet_startup_cli <- function() {
    tips <- c(
      "i" = "There are lots of ways to contribute to {.pkg manynet} at {.url https://github.com/stocnet/manynet/}.",
      "i" = "Please let us know any bugs, issues, or feature requests at {.url https://github.com/stocnet/manynet/issues}. It's really helpful!",
      "i" = "To eliminate package startup messages, use: `suppressPackageStartupMessages(library({.pkg manynet}))`.",
      # "i" = "Changing the theme of all your graphs is straightforward with `set_manynet_theme()`",
      "i" = "If there are too many messages in the console, run `options(manynet_verbosity = 'quiet')`",
      "i" = "Visit the website to learn more: {.url https://stocnet.github.io/manynet/}.",
      "i" = "We recommend the 'Function Overview' page online to discover new analytic opportunities: {.url https://stocnet.github.io/manynet/reference/index.html}.",
      "i" = "Discover all the {.emph stocnet} R packages at {.url https://github.com/stocnet/}.",
      # "i" = "Star me at {.url https://github.com/users/follow?target=jhollway}.",
      "i" = "You can list all the tutorials available in {.pkg manynet} using {.fn run_tute}, and run them too!"
    )
    cli::cli_inform(sample(tips, 1), class = "packageStartupMessage")
  }

  if (interactive()) {
    if (behind_cran) {
      msg <- "A new version of manynet is available with bug fixes and new features."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::update.packages("manynet")
      }
    } else {
      greet_startup_cli()
      # packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }
  }

}

mnet_progress_step <- function(..., .envir = parent.frame()){
  if(getOption("manynet_verbosity", default = "quiet")!="quiet")
  cli::cli_progress_step(..., .envir = .envir)
}

mnet_progress_along <- function(..., .envir = parent.frame()){
  if(getOption("manynet_verbosity", default = "quiet")!="quiet")
    cli::cli_progress_along(..., .envir = .envir)
}

mnet_progress_seq <- function(..., .envir = parent.frame()){
  if(getOption("manynet_verbosity", default = "quiet")!="quiet")
    cli::cli_progress_along(seq.int(...), .envir = .envir)
}

mnet_info <- function(..., .envir = parent.frame()){
  if(getOption("manynet_verbosity", default = "quiet")!="quiet")
    cli::cli_alert_info(paste(...), .envir = .envir)
}

mnet_success <- function(..., .envir = parent.frame()){
  if(getOption("manynet_verbosity", default = "quiet")!="quiet")
    cli::cli_alert_success(paste(...), .envir = .envir)
}

mnet_unavailable <- function(..., .envir = parent.frame()){
  if(getOption("manynet_verbosity", default = "quiet")!="quiet")
    cli::cli_abort(paste(..., 
                           "If you are interested in this feature,",
                           "please vote for it or raise it as an issue at", 
                           "{.url https://github.com/stocnet/manynet/issues}."), 
                   .envir = .envir)
}

manynet_console_theme <- function(){
  # dark <- detect_dark_theme(dark)
  list(h1 = list(`margin-top` = 1, `margin-bottom` = 0, color = "#199D77", 
                 fmt = function(x) cli::rule(x, line_col = "#199D77")), 
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
  # if (dark) {
  #   list(`background-color` = "#232323", color = "#f0f0f0")
  # }
  # else {
    list(`background-color` = "#f8f8f8", color = "#202020")
  # }
}

col_mnet_green <- cli::make_ansi_style("#199D77")

col_mnet_blue <- cli::make_ansi_style("#4576B5")
