#' @importFrom cli cli_div cli_inform cli_end
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()

  # pkgs <- as.data.frame(utils::available.packages(utils::contrib.url(getOption("repos"))))
  # 
  # cran_version <- pkgs[pkgs$Package == "manynet","Version"]

  local_version <- utils::packageVersion("manynet")
  cli::cli_div(theme = list(span.emph = list(color = "red")))
  cli::cli_inform("This is {.pkg manynet} version {.emph {local_version}}", class = "packageStartupMessage")
  cli::cli_end()
  old.list <- as.data.frame(utils::old.packages())
  behind_cran <- "manynet" %in% old.list$Package
  
  greet_startup_cli <- function() {
    tips <- c(
      "Contribute to manynet at {.url https://github.com/stocnet/manynet/}.",
      "Let us know any issues or features requests at {.url https://github.com/stocnet/manynet/issues}.",
      "Use {.fn suppressPackageStartupMessages} to eliminate package startup messages.",
      "Visit the website to learn more: {.url https://stocnet.github.io/manynet/}.",
      "Discover all the {.emph stocnet} R packages at {.url https://github.com/stocnet/}.",
      # "Star me at {.url https://github.com/users/follow?target=jhollway}.",
      "Check out the tutorials included in the package using {.fn run_tute}."
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
