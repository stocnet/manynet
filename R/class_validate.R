#' @rdname make_stocnet
#' @template param_data
#' @export
validate_stocnet <- function(.data) {
  if(!inherits(.data, "stocnet")) 
    snet_abort("This function only works for stocnet objects.")
  validate_info(.data)
  validate_nodes(.data)
  validate_ties(.data)
  validate_changes(.data)
  validate_globals(.data)
  validate_missings(.data)
  invisible(.data)
}

validate_nodes <- function(.data){
  if(is.null(.data$nodes)) return(invisible(.data))
  expect_class(.data, "nodes", "tbl_df")
  # Note that an 'id' is not among the names for a label here, for the reason
  # `rename_nodes()` gives: a file format requires an id of its own, so taking
  # it for a label would name nodes the file never named.
  reserved_cols(.data, "nodes", "label", "character", 
           aka = "name")
  reserved_cols(.data, "nodes", "mode", "character")
  reserved_cols(.data, "nodes", "active", "logical")
  reserved_cols(.data, "nodes", "na", "logical")
  invisible(.data)
}

validate_ties <- function(.data){
  if(is.null(.data$ties)) return(invisible(.data))
  expect_class(.data, "ties", "tbl_df")
  required_cols(.data, "ties", c("from", "to"))
  reserved_cols(.data, "ties", "from", "integer", 
           aka = c("source", "sender", "ego"),
           pool = seq_nodes(.data))
  reserved_cols(.data, "ties", "to", "integer", 
           aka = c("target", "receiver", "alter"),
           pool = seq_nodes(.data))
  reserved_cols(.data, "ties", "by", "integer", 
                aka = c("tertius", "third", "about", "referent", "regarding"),
                pool = seq_nodes(.data))
  reserved_cols(.data, "ties", "weight", 
                class = c("numeric","integer"), 
           aka = c("value", "strength", "val", "sign"))
  # Note that 'begin' and 'end' are not among the names for a time here.
  # These mark the span over which a tie is present, which `is_dynamic()`
  # reads as such, rather than a time that is named some other way.
  # Note that a 'date' is not among the names for a time here, for the reason
  # `is_longitudinal()` gives: it reads a moment under 'time', 'wave', or
  # 'panel' and not under 'date', so a network of dated events, such as
  # `ison_southern_women`, records those dates as the attribute they are.
  reserved_cols(.data, "ties", "time",
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"),
                aka = c("wave", "period", "panel"))
  reserved_cols(.data, "ties", "layer", "character",
           aka = c("type", "plex", "tie"))
  invisible(.data)
}

validate_changes <- function(.data){
  if(is.null(.data$changes)) return(invisible(.data))
  expect_class(.data, "changes", "tbl_df")
  required_cols(.data, "changes", c("node", "time", "var", "value"))
  reserved_cols(.data, "changes", "node", "integer", aka = "id",
                pool = seq_nodes(.data))
  reserved_cols(.data, "changes", "time", 
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"), 
                aka = c("wave", "period", "date", "begin", "end"))
  reserved_cols(.data, "changes", "var", "character")
  # No pool is checked here, since a change to a variable that is not a tie
  # layer, such as a nodal attribute, names no layer and so holds NA.
  reserved_cols(.data, "changes", "layer", "character")
  invisible(.data)
}

validate_info <- function(.data){
  if(is.null(.data$info)) return(invisible(.data))
  expect_class(.data, "info", "list")
  reserved_cols(.data, "info", "name", "character")
  reserved_cols(.data, "info", "modes", "character", len = net_modes(.data))
  reserved_cols(.data, "info", "layers", "character", len = net_layers(.data))
  reserved_cols(.data, "info", "observation", "character",
                pool = c("panel", "event", "cross-sectional", "egocentric", 
                          "cognitive"))
  reserved_cols(.data, "info", "sender", "character",
                pool = mode_names(.data))
  reserved_cols(.data, "info", "receiver", "character",
                pool = mode_names(.data))
  reserved_cols(.data, "info", "update", "character",
                pool = c("increment", "replace"))
  focal_pool <- unique(c(layer_names(.data), net_node_attributes(.data),
                         as.character(.data$changes$var)))
  reserved_cols(.data, "info", "focal", "character",
                pool = focal_pool, aka = c("dependent","dv"))
  reserved_cols(.data, "info", "centered", "logical")
  invisible(.data)
}

validate_globals <- function(.data){
  if(is.null(.data$globals)) return(invisible(.data))
  expect_class(.data, "globals", "tbl_df")
  required_cols(.data, "globals", c("var", "value"))
  reserved_cols(.data, "globals", "time",
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"),
                aka = c("wave", "period", "date", "begin", "end"))
  reserved_cols(.data, "globals", "var", "character")
  reserved_cols(.data, "globals", "value")
  invisible(.data)
}

# The missings component lists dyads, so it takes the same columns as the ties.
validate_missings <- function(.data){
  if(is.null(.data$missings)) return(invisible(.data))
  expect_class(.data, "missings", "tbl_df")
  required_cols(.data, "missings", c("from", "to"))
  reserved_cols(.data, "missings", "from", "integer",
                aka = c("source", "sender", "ego"),
                pool = seq_nodes(.data))
  reserved_cols(.data, "missings", "to", "integer",
                aka = c("target", "receiver", "alter"),
                pool = seq_nodes(.data))
  reserved_cols(.data, "missings", "layer", "character",
                aka = c("type", "plex", "tie"))
  reserved_cols(.data, "missings", "time",
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"),
                aka = c("wave", "period", "date"))
  invisible(.data)
}

# Helpers ####

reserved_cols <- function(.data, component, column, class, 
                          len = NULL, pool = NULL, aka = NULL) {
  if(column %in% names(.data[[component]])){
    if(!is.null(len)){
      if(length(.data[[component]][[column]]) != len) 
        snet_abort("'{component}${column}' must be of length {len}.")
    }
    if(!missing(class) && 
       length(intersect(class(.data[[component]][[column]]), class))==0) 
      snet_abort("'{component}${column}' must be of class '{class}'.")
    if(!is.null(pool)){
      if(!all(.data[[component]][[column]] %in% pool)){
        values <- unique(as.character(.data[[component]][[column]]))
        unmatched <- values[which(!values %in% pool)]
        # More than one value can be unmatched, so the NA is named elementwise
        # rather than in a condition that only a single value would satisfy.
        unmatched[is.na(unmatched)] <- "NA (probably unmatched ids)"
        snet_abort("'{component}${column}' includes {phrase(unmatched)},",
                   "which must be one of {phrase(pool)}.")
      } 
    }
  } else if(!is.null(aka)){
    if(any(aka %in% names(.data[[component]]))){
      mislabelled <- names(.data[[component]])[names(.data[[component]]) %in% aka]
      snet_warn("'{component}${mislabelled}' might be better called {component}${column}.")
    }
  }
}

required_cols <- function(.data, component, required_cols) {
  if(!all(required_cols %in% names(.data[[component]]))) 
    snet_abort("The '{component}' component of a stocnet object must have the following columns: {phrase(required_cols)}.")
}

expect_class <- function(.data, component, expected_class) {
  if(!inherits(.data[[component]], expected_class)) 
    snet_abort("The '{component}' component of a stocnet object must be of class '{expected_class}'.")
}

