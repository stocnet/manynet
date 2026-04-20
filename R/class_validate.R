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
  invisible(.data)
}

reserved_cols <- function(.data, component, reserved_cols, class, 
                     length = NULL, match = NULL, aka = NULL) {
  if(reserved_cols %in% names(.data[[component]])){
    if(!is.null(length)){
      if(length(.data[[component]][[reserved_cols]]) != length) 
        snet_abort("'{reserved_cols}' must be of length {length}.")
    }
    if(!inherits(.data[[component]][[reserved_cols]], class)) 
      snet_abort("'{reserved_cols}' must be of class '{class}'.")
    if(!is.null(match)){
      if(!all(.data[[component]][[reserved_cols]] %in% match)) 
        snet_abort("'{reserved_cols}' must be one of {to_phrase(match)}.")
    }
  } else if(!is.null(aka)){
    if(any(aka %in% names(.data[[component]]))){
      mislabelled <- names(.data[[component]])[names(.data[[component]]) %in% aka]
      snet_warn("Columns '{mislabelled}' might be better called {reserved_cols}.")
    }
  }
}

required_cols <- function(.data, component, required_cols) {
  if(!all(required_cols %in% names(.data[[component]]))) 
    snet_abort("The '{component}' component of a stocnet object must have the following columns: {to_phrase(required_cols)}.")
}

expect_class <- function(.data, component, expected_class) {
  if(!inherits(.data[[component]], expected_class)) 
    snet_abort("The '{component}' component of a stocnet object must be of class '{expected_class}'.")
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
  reserved_cols(.data, "ties", "from", "numeric", 
           aka = c("source", "sender", "ego"))
  reserved_cols(.data, "ties", "to", "numeric", 
           aka = c("receiver", "target", "alter"))
  reserved_cols(.data, "ties", "weight", "numeric", 
           aka = c("value", "strength", "val", "sign"))
  reserved_cols(.data, "ties", "time", "character", 
           aka = c("wave", "period", "date", "begin", "end"))
  reserved_cols(.data, "ties", "layer", "character", 
           aka = c("type", "plex", "tie"))
  invisible(.data)
}

validate_changes <- function(.data){
  if(is.null(.data$changes)) return(invisible(.data))
  expect_class(.data, "changes", "tbl_df")
  required_cols(.data, "changes", c("node", "var", "value"))
  reserved_cols(.data, "changes", "node", "numeric", aka = c("id"))
  reserved_cols(.data, "changes", "var", "character")
  invisible(.data)
}

validate_info <- function(.data){
  if(is.null(.data$info)) return(invisible(.data))
  expect_class(.data, "info", "list")
  reserved_cols(.data, "info", "name", "character")
  reserved_cols(.data, "info", "modes", "character", length = net_modes(.data))
  reserved_cols(.data, "info", "layers", "character", length = net_layers(.data))
  reserved_cols(.data, "info", "dependent", "character", length = 1, 
           match = layer_names(.data))
  invisible(.data)
}

