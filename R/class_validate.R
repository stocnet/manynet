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
  # A hyperedge lists its members in 'with' and so names no node in 'from' or
  # 'to', which hold NA on its row and on no other.
  hyper <- validate_hyperedges(.data)
  ends <- if(any(hyper)) c(seq_nodes(.data), NA) else seq_nodes(.data)
  reserved_cols(.data, "ties", "from", "integer", 
           aka = c("source", "sender", "ego"),
           pool = ends)
  reserved_cols(.data, "ties", "to", "integer", 
           aka = c("target", "receiver", "alter"),
           pool = ends)
  # 'by' names the node that reported a tie, as in a cognitive social
  # structure, and 'about' the node a tie is about, as in gossip. Either may be
  # NA, for a layer that no node reported or that is about no node.
  # A broker, or 'tertius', sits between the ends of a tie and is neither.
  reserved_cols(.data, "ties", "by", "integer",
                aka = c("observer", "perceiver", "reporter"),
                pool = c(seq_nodes(.data), NA))
  reserved_cols(.data, "ties", "about", "integer",
                aka = c("referent", "regarding", "subject"),
                pool = c(seq_nodes(.data), NA))
  reserved_cols(.data, "ties", "weight", 
                class = c("numeric","integer"), 
           aka = c("value", "strength", "val", "sign"))
  # A length is a distance and not a strength, so it is held apart from the
  # weight, and a tie can have both. See `tie_lengths()`.
  reserved_cols(.data, "ties", "length",
                class = c("numeric","integer"),
                aka = "distance")
  # Note that 'begin' and 'end' are not among the names for a time here.
  # These mark the span over which a tie is present, which `is_dynamic()`
  # reads as such, rather than a time that is named some other way.
  # A 'date' is among the names for a time because a moment is a moment however
  # it is written. How a moment relates to the one before it is a separate
  # question, which `info$update` answers, so naming a column 'time' does not
  # by itself make a network a panel. See `.time_rule()`.
  reserved_cols(.data, "ties", "time",
                class = c("character","numeric","integer","mdate","Date","POSIXct","POSIXlt"),
                aka = c("wave", "period", "panel", "date"))
  reserved_cols(.data, "ties", "layer", "character",
           aka = c("type", "plex", "tie"))
  # A fixed tie is present by design, and so not free to vary, as SIENA's
  # structural one is. A dyad fixed at no tie would need a row that is not a
  # tie, which the ties do not hold, so a fixed tie cannot weigh nothing.
  reserved_cols(.data, "ties", "fixed", "logical")
  ties <- .data$ties
  if("fixed" %in% names(ties) && "weight" %in% names(ties) &&
     any(ties$fixed & ties$weight == 0, na.rm = TRUE))
    snet_abort(c(x = "A fixed tie marks a tie that is present by design, so it cannot have a weight of 0.",
                 i = "Structural zeros are not held yet (see #195)."))
  invisible(.data)
}

# The rows of the ties that are hyperedges, having checked that they are well
# formed. A hyperedge lists its members in 'with', and any value it holds for
# each member, such as that member's weight or the side of a directed
# hyperedge it is on, in a 'with_' column of the same lengths.
validate_hyperedges <- function(.data){
  ties <- .data$ties
  if(!"with" %in% names(ties)){
    held <- .with_cols(ties)
    if(length(held))
      snet_abort("'ties${held}' hold{?s/} a value for each member",
                 "of a hyperedge, so the ties must list those members in a",
                 "'with' column.")
    return(rep(FALSE, nrow(ties)))
  }
  if(!is.list(ties$with))
    snet_abort("'ties$with' must be a list of the nodes in each hyperedge.")
  hyper <- .hyper_rows(ties)
  members <- unlist(ties$with)
  if(length(members) && (!is.numeric(members) || anyNA(members) ||
                         !all(members %in% seq_nodes(.data))))
    snet_abort("'ties$with' must list the members of each hyperedge as",
               "indices of the nodes, from 1 to {net_nodes(.data)}.")
  # a node is a member of a hyperedge once, or once on each side of one that
  # has two sides
  sides <- if("with_direction" %in% names(ties) &&
              is.list(ties$with_direction) &&
              identical(lengths(ties$with_direction), lengths(ties$with)))
    ties$with_direction else lapply(ties$with, function(x) rep("", length(x)))
  if(any(mapply(function(x, d) anyDuplicated(paste(x, d)),
                ties$with[hyper], sides[hyper]) > 0))
    snet_abort("'ties$with' lists a node more than once in a hyperedge.")
  dyad <- !is.na(ties$from) | !is.na(ties$to)
  if(any(hyper & dyad))
    snet_abort("A tie is either a dyad, with nodes in 'from' and 'to', or a",
               "hyperedge, with nodes in 'with', and row{?s}",
               "{which(hyper & dyad)} hold{?s/} both.")
  if(any(!hyper & (is.na(ties$from) | is.na(ties$to))))
    snet_abort("'ties$from' and 'ties$to' may be NA only for a hyperedge,",
               "which lists its nodes in 'with'.")
  sizes <- lengths(ties$with)
  for(col in .with_cols(ties)){
    if(!is.list(ties[[col]]) || !identical(lengths(ties[[col]]), sizes))
      snet_abort("'ties${col}' must be a list holding one value for each",
                 "member of each hyperedge, in the order 'with' lists them.")
  }
  # the side of a member may be NA, where it is not known
  if("with_direction" %in% names(ties) &&
     !all(unlist(ties$with_direction) %in% c("from", "to", NA)))
    snet_abort("'ties$with_direction' must say whether each member of a",
               "hyperedge is on its 'from' or its 'to' side.")
  if("layer" %in% names(ties)){
    mixed <- intersect(ties$layer[hyper], ties$layer[!hyper])
    if(length(mixed))
      snet_abort("Layer{?s} {.val {mixed}} hold{?s/} both dyads and",
                 "hyperedges. A layer must hold one or the other.")
  } else if(any(hyper) && !all(hyper))
    snet_abort("These ties hold both dyads and hyperedges, so a 'layer'",
               "column must say which layer each belongs to.")
  reserved_cols(.data, "ties", "label", "character")
  hyper
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
  reserved_cols(.data, "missings", "by", "integer",
                pool = c(seq_nodes(.data), NA))
  reserved_cols(.data, "missings", "about", "integer",
                pool = c(seq_nodes(.data), NA))
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

