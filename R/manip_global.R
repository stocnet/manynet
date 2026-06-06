#' Manipulating global attributes
#' @name manip_global
#' @description
#'   These functions offer ways to manipulate network-level data constants
#'   or variables that are not tied to a particular node or tie.
#'   They include:
#'   
#'   - `mutate_globals()` adds a table of global variables to the network.
#'   
#'   It expects three columns for  
#'   the variable to which the change applies, which should be called 'var', 
#'   the time of the change, which should be called 'time',
#'   and the new value to be applied, which should be called 'value'.
#' @template param_data
#' @template param_dots
#' @family global
#' @eval detail_avail(".*_global")
#' @template fam_manip
#' @seealso [to_time()]
NULL

#' @rdname manip_global
#' @examples
#' as_stocnet(ison_algebra) |> 
#'    mutate_globals(time = 2, var = "active", value = FALSE)
#' @export
mutate_globals <- function(.data, ...) UseMethod("mutate_globals")

#' @export
mutate_globals.default <- function(.data, ...){
  as_input(.data, mutate_globals, ...)
}

#' @export
mutate_globals.stocnet <- function(.data, ...){
  out <- .data
  if(is.null(out$globals)){
    out$globals <- tibble::tibble(...)
  } else {
    globals <- out$globals
    globals <- dplyr::mutate(globals, ...)
    out$globals <- globals
  }
  out
}
