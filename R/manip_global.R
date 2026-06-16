#' Manipulating global attributes
#' @name manip_global
#' @description
#'   These functions offer ways to manipulate network-level data constants
#'   or variables that are not tied to a particular node or tie.
#'   They include:
#'   
#'   - `mutate_globals()` adds a table of global variables to the network.
#'   - `rename_globals()` renames columns in the global variables table.
#'   - `select_globals()` selects columns in the global variables table.
#'   
#'   It expects three columns for  
#'   the variable to which the change applies, which should be called 'var', 
#'   the time of the change, which should be called 'time',
#'   and the new value to be applied, which should be called 'value'.
#' @template param_data
#' @template param_dots
#' @family global
#' @eval detail_avail(".*_globals")
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
  if(is.null(out$global)){
    out$global <- tibble::tibble(...)
  } else {
    global <- out$global
    global <- dplyr::mutate(global, ...)
    out$global <- global
  }
  out
}

#' @rdname manip_global
#' @export
rename_globals <- function(.data, ...) UseMethod("rename_globals")

#' @export
rename_globals.default <- function(.data, ...){
  as_input(.data, rename_globals, ...)
}

#' @export
rename_globals.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    aka <- list(
      time   = c("wave", "period", "date", "begin"),
      var   = c("variable", "attribute"),
      value = c("weight", "strength", "increment", "replace", "sign")
    )
    
    current_names <- names(out)
    rename_map <- c()
    
    for(expected in names(aka)){
      if(!expected %in% current_names){
        match <- intersect(aka[[expected]], current_names)
        if(length(match) > 0){
          rename_map[expected] <- match[1]  # take the first match if multiple
        }
      }
    }
    
    if(length(rename_map) > 0){
      snet_minor_info("Renaming global columns to stocnet conventions:",
                      "{paste(rename_map, '->', names(rename_map), collapse = ', ')}")
      out <- out |> dplyr::rename(dplyr::any_of(rename_map))
    }
  } else out <- out |> dplyr::rename(...)
  out
}

#' @export
rename_globals.stocnet <- function(.data, ...){
  out <- .data
  out$global <- rename_globals.data.frame(out$global, ...)
  out
}

#' @rdname manip_global
#' @export
select_globals <- function(.data, ...) UseMethod("select_globals")

#' @export
select_globals.default <- function(.data, ...){
  as_input(.data, select_globals, ...)
}

#' @export
select_globals.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    out <- dplyr::select(out, dplyr::any_of(c("var", "time", "value")))
  } else out <- dplyr::select(out, ...)
  out
}

#' @export
select_globals.stocnet <- function(.data, ...){
  out <- .data
  out$global <- select_globals.data.frame(out$global, ...)
  out
}

