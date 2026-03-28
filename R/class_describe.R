#' Describe a network
#' @name class_describe
#' @description
#'   These functions are used to describe components of a given network
#'   in terms of a particular phrase.
#'   
#'   - `describe_network()` describes the features or properties of a network,
#'   such as whether it is two-mode, directed, or complex.
#'   - `describe_nodes()` describes how many of each type of nodes there are
#'   and, if available, names the different nodesets or modes.
#'   - `describe_ties()` describes how many of each type of ties there are
#'   and, if available, names the different types of ties.
#'   - `describe_changes()` describes the changing features of a network,
#'   if any, such as how many waves there are.
#'   
#'   These descriptions are constructed to be GRAND-consistent.
#' @template param_data
NULL

#' @rdname class_describe
#' @export
describe_network <- function(.data) {
  paste0("A ",
         ifelse(is_dynamic(.data), "dynamic, ", ""),
         ifelse(is_longitudinal(.data), "longitudinal, ", ""),
         ifelse(is_labelled(.data), "labelled, ", ""),
         ifelse(is_complex(.data), "complex, ", ""),
         ifelse(is_multiplex(.data), "multiplex, ", ""),
         ifelse(is_signed(.data), "signed, ", ""),
         ifelse(is_weighted(.data), "weighted, ", ""),
         ifelse(is_twomode(.data), "two-mode", 
                ifelse(is_directed(.data), "directed", "undirected")),
         " network"
  )
}

#' @rdname class_describe
#' @export
describe_nodes <- function(.data){
  nd <- net_dims(.data)
  nn <- net_node_names(.data)
  if(is.null(nn)) nn <- "nodes"
  nn <- ifelse(nd==1, singularize(nn), pluralize(nn))
  node_name <- paste(nd, nn)
  to_phrase(node_name)
}

#' @rdname class_describe
#' @export
describe_ties <- function(.data){
  nt <- net_ties(.data)
  tie_name <- ifelse(is_directed(.data), "arcs", "ties") 
  if(!is.null(net_tie_names(.data))){
    tie_name <- paste(net_tie_names(.data), tie_name)
  } else if(!is.null(tie_attribute(.data, "type"))){
    tab <- table(tie_attribute(.data, "type"))
    parts <- paste0(tab, " ", singularize(names(tab)))
    # if (length(parts) > 1) {
    #   result <- paste(
    #     paste(parts[-length(parts)], collapse = ", "),
    #     parts[length(parts)],
    #     sep = ", and "
    #   )
    # } else {
    #   result <- parts
    # }
    return(paste0(to_phrase(parts), " ties"))
  } 
  paste(nt, tie_name)
}

#' @rdname class_describe
#' @export
describe_changes <- function(.data){
  if(is_longitudinal(.data)){
    waves <- tie_attribute(.data, "wave")
    if(is.null(waves)) waves <- as_changelist(.data)$time
    paste(" over", max(waves), "waves")
  } else if (is_dynamic(.data)){
    if("time" %in% net_tie_attributes(.data)){
      paste(" from", min(tie_attribute(.data, "time"), na.rm = TRUE), 
            "to", max(tie_attribute(.data, "time"), na.rm = TRUE))
    } else if("begin" %in% net_tie_attributes(.data)){
      paste(" from", min(tie_attribute(.data, "begin"), na.rm = TRUE), 
            "to", max(tie_attribute(.data, "end"), na.rm = TRUE))
    }
    
  }
}

pluralize <- function(word) {
  if(length(word) > 1) return(sapply(word, pluralize))
  if (grepl("(us|x|z|ch|sh)$", word)) {
    paste0(word, "es")
  } else if (grepl("[^aeiou]y$", word)) {
    sub("y$", "ies", word)
  } else if (!grepl("s$", word)) {
    paste0(word, "s")
  } else {
    word
  }
}

singularize <- function(word) {
  if(length(word) > 1) return(sapply(word, singularize))
  if (grepl("friends$", word)) {
    sub("s$", "ship", word)
  } else if (grepl("ies$", word)) {
    sub("ies$", "y", word)
  } else if (grepl("(xes|ses|zes|ches|shes)$", word)) {
    sub("es$", "", word)
  } else if (grepl("s$", word)) {
    sub("s$", "", word)
  } else {
    word
  }
}

to_phrase <- function(items) {
  n <- length(items)
  
  if (n == 0) {
    ""
  } else if (n == 1) {
    items
  } else if (n == 2) {
    paste(items, collapse = " and ")
  } else {
    paste(
      paste(items[1:(n-1)], collapse = ", "),
      items[n],
      sep = ", and "
    )
  }
}
