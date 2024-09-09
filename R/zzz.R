.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()

  # pkgs <- as.data.frame(utils::available.packages(utils::contrib.url(getOption("repos"))))
  # 
  # cran_version <- pkgs[pkgs$Package == "manynet","Version"]

  local_version <- packageVersion("manynet")
  packageStartupMessage("This is manynet version ", local_version)
  old.list <- as.data.frame(old.packages())
  behind_cran <- "manynet" %in% old.list$Package

  tips <- c(
    "Learn more about manynet at https://github.com/stocnet/manynet/.",
    "Use `suppressPackageStartupMessages()` to eliminate package startup messages.",
    "You might like our website Visit: https://stocnet.github.io/manynet/",
    "Check out all the stocnet R packages. Visit: https://github.com/stocnet/.",
    "Check out the tutorials included in the package using `run_tute()`",
    "Star me at https://github.com/users/follow?target=jhollway"
  )

  tip <- sample(tips, 1)

  if (interactive()) {
    if (behind_cran) {
      msg <- "A new version of manynet is available with bug fixes and new features."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        update.packages("manynet")
      }
    } else {
      packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }
  }

}
