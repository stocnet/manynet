# nocov start
#' @importFrom cli cli_div cli_inform cli_end
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()
  
  options(snet_verbosity = getOption("snet_verbosity", "verbose"))
  options(manynet_theme = getOption("manynet_theme", "default"))
  options(cli.theme = manynet_console_theme())
  options(cli.progress_clear = TRUE)
  
  # pkgs <- as.data.frame(utils::available.packages(utils::contrib.url(getOption("repos"))))
  # 
  # cran_version <- pkgs[pkgs$Package == "manynet","Version"]

  local_version <- utils::packageVersion("manynet")
 snet_info("You are using {.mnet manynet} version {.version {local_version}}.")
  old.list <- as.data.frame(utils::old.packages())
  behind_cran <- "manynet" %in% old.list$Package
  
  greet_startup_cli <- function() {
    tips <- c(
      "i" = "There are lots of ways to contribute to {.pkg manynet} at {.url https://github.com/stocnet/manynet/}.",
      "i" = "Please share bugs, issues, or feature requests at {.url https://github.com/stocnet/manynet/issues}. It's really helpful!",
      # "i" = "To suppress package startup messages, use: `suppressPackageStartupMessages(library({.pkg manynet}))`.",
      # "i" = "Changing the theme of all your graphs is straightforward with `set_manynet_theme()`",
      "i" = "If too many messages appear in the console, run `options(snet_verbosity = 'quiet')`",
      "i" = "Explore changes since the last version with {.code news(package = 'manynet')}.",
      "i" = "Visit the website to learn more: {.url https://stocnet.github.io/manynet/}.",
      "i" = "The 'Function Overview' may suggest new analytic opportunities: {.url https://stocnet.github.io/manynet/reference/index.html}.",
      "i" = "Discover {.emph stocnet} R packages at {.url https://github.com/stocnet/}.",
      # "i" = "Star me at {.url https://github.com/users/follow?target=jhollway}.",
      "i" = "You can list all the tutorials available in {.pkg manynet} using {.fn run_tute}, and run them too!"
    )
    snet_info(sample(tips, 1))
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

# nocov end