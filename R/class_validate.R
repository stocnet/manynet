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
  validate_global(.data)
  invisible(.data)
}

validate_nodes <- function(.data){
  if(is.null(.data$nodes)) return(invisible(.data))
  expect_class(.data, "nodes", "tbl_df")
  reserved_cols(.data, "nodes", "label", "character", 
           aka = c("name", "id"))
  reserved_cols(.data, "nodes", "mode", "character")
  reserved_cols(.data, "nodes", "active", "logical")
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
  reserved_cols(.data, "ties", "time", 
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"), 
                aka = c("wave", "period", "date", "begin", "end"))
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
  reserved_cols(.data, "info", "focal", "character", len = 1, 
                pool = layer_names(.data), aka = c("dependent","dv"))
  invisible(.data)
}

validate_global <- function(.data){
  if(is.null(.data$global)) return(invisible(.data))
  expect_class(.data, "global", "tbl_df")
  required_cols(.data, "global", c("var", "value"))
  reserved_cols(.data, "global", "time", 
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"), 
                aka = c("wave", "period", "date", "begin", "end"))
  reserved_cols(.data, "global", "var", "character")
  reserved_cols(.data, "global", "value")
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
        if(is.na(unmatched)) unmatched <- "NA (probably unmatched ids)"
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

