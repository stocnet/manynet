# nocov start
#' @importFrom cli cli_div cli_inform cli_end
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()
  
  options(snet_verbosity = getOption("snet_verbosity", "verbose"))
  options(manynet_theme = getOption("manynet_theme", "default"))
  options(cli.theme = manynet_console_theme())
  options(cli.progress_clear = TRUE)
  
  local_version <- utils::packageVersion("manynet")
  snet_info("You are using {.mnet manynet} version {.version {local_version}}.")

  greet_startup_cli <- function() {
    tips <- c(
      "i" = "Share bugs, issues, or feature requests at {.url https://github.com/stocnet/manynet/issues}.",
      "i" = "If too many messages appear in the console, run {.run base::options(snet_verbosity = 'quiet')}",
      "i" = "Explore changes since the last version with {.run [news(package = 'manynet')](utils::news(package = 'manynet'))}.",
      "i" = "Visualisation functions are in {.auto autograph}. Install everything with {.run [install.packages('migraph')](utils::install.packages('migraph'))}.",
      "i" = "Analytic functions are in {.tric netrics}. Install everything with {.run [install.packages('migraph')](utils::install.packages('migraph'))}.",
      "i" = "Visit {.url https://stocnet.github.io/manynet/} to learn more.",
      "i" = "Discover new functions at: {.url https://stocnet.github.io/manynet/reference/index.html}.",
      "i" = "Discover {.emph stocnet} R packages at {.url https://github.com/stocnet/}."
    )
    snet_info(sample(tips, 1))
  }

  greet_startup_cli()

}

# nocov end