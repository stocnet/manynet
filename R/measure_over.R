#' Helper functions for measuring over splits of networks  
#' @inheritParams is
#' @param times Integer indicating number of simulations used for quantile estimation. 
#'   (Relevant to the null hypothesis test only - 
#'   the analysis itself is unaffected by this parameter.) 
#'   Note that, as for all Monte Carlo procedures, convergence is slower for more
#'   extreme quantiles.
#'   By default, `times=1000`.
#'   1,000 - 10,000 repetitions recommended for publication-ready results.
#' @param strategy If `{furrr}` is installed, 
#'   then multiple cores can be used to accelerate the function.
#'   By default `"sequential"`, 
#'   but if multiple cores available,
#'   then `"multisession"` or `"multicore"` may be useful.
#'   Generally this is useful only when `times` > 1000.
#'   See [`{furrr}`](https://furrr.futureverse.org) for more.
#' @param verbose Whether the function should report on its progress.
#'   By default FALSE.
#'   See [`{progressr}`](https://progressr.futureverse.org) for more.
#' @importFrom future plan
#' @importFrom furrr future_map_dfr furrr_options
#' @param FUN A function to run over all splits.
#' @param ... Further arguments to be passed on to FUN.
#' @param attribute A string naming the attribute to be split upon.
#' @param slice Optionally, a vector of specific slices.
#'   Otherwise all observed slices will be returned.
#' @name over
NULL

#' @describeIn over Runs a function, e.g. a measure,
#'   over waves of a panel network
#' @export
over_waves <- function(.data, FUN, ..., attribute = "wave",
                       strategy = "sequential",
                       verbose = FALSE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
    furrr::future_map_dbl(manynet::to_waves(.data, attribute), function(j) FUN(j, ...), 
                        .progress = verbose, .options = furrr::furrr_options(seed = T))
}

#' @describeIn over Runs a function, e.g. a measure,
#'   over time slices of a dynamic network
#' @export
over_time <- function(.data, FUN, ..., attribute = "time",
                      slice = NULL,
                      strategy = "sequential",
                      verbose = FALSE){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  out <- furrr::future_map_dbl(manynet::to_slices(.data, attribute, slice), 
                               function(j) FUN(j, ...), 
                               .progress = verbose, 
                               .options = furrr::furrr_options(seed = T))
  make_network_measures(out, .data)
}
