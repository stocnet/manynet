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
#'   - `describe_transformations()` describes how the network has been
#'   transformed since it was collected or generated, if at all.
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
  nd <- mode_nodes(.data)
  nn <- mode_names(.data)
  if(is.null(nn)) nn <- "nodes"
  nn <- ifelse(nd==1, singularize(nn), pluralize(nn))
  node_name <- paste(nd, nn)
  phrase(node_name)
}

#' @rdname class_describe
#' @export
describe_ties <- function(.data){
  nt <- net_ties(.data)
  tie_name <- ifelse(is_directed(.data), "arcs", "ties")
  if(!is.null(layer_names(.data))){
    # Where a network records the directedness of each layer, an undirected
    # layer of an otherwise directed network holds ties rather than arcs.
    directed <- as_infolist(.data)$directed
    layer_name <- if(!is.null(directed) && !is.null(names(directed)))
      ifelse(directed[layer_names(.data)], "arcs", "ties") else tie_name
    parts <- paste0(layer_ties(.data), " ", singularize(layer_names(.data)),
                    " ", layer_name)
    return(phrase(parts))
  } else if(!is.null(tie_attribute(.data, "type"))){
    tab <- table(tie_attribute(.data, "type"))
    parts <- paste0(tab, " ", singularize(names(tab)))
    return(paste0(phrase(parts), " ", tie_name))
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

#' @rdname class_describe
#' @param details Logical. Where FALSE, the default, the description is given
#'   at whichever level of detail fits the width of the console. Where TRUE,
#'   every method and its consequence is named whatever the width.
#' @param width Integer. The number of characters the description may fill,
#'   by default the width of the console. Where a caller adds words of its own,
#'   such as the header `print()` puts this in, it passes what is left.
#' @examples
#' describe_transformations(to_undirected(ison_southern_women))
#' @export
describe_transformations <- function(.data, details = FALSE,
                                     width = cli::console_width()){
  trans <- as_infolist(.data)$transformations
  if(!length(trans) || is.null(names(trans))) return("")
  full <- phrase(.transformation_parts(trans, "full"))
  if(details) return(full)
  # GRAND asks for the method and the amount, but a console too narrow to hold
  # them is better given less than given a wrapped and unreadable line, so the
  # consequences go first and then the methods, leaving the names as the least
  # that still reports that the network was transformed at all.
  if(nchar(full) <= width) return(full)
  methods <- phrase(.transformation_parts(trans, "methods"))
  if(nchar(methods) <= width) return(methods)
  phrase(.transformation_parts(trans, "names"))
}

# One item for each transformation recorded, at one of three levels of detail.
# An element accumulates, so a network transformed twice in the same way names
# both, joined in the order they were applied.
.transformation_parts <- function(trans, level){
  vapply(names(trans), function(nm){
    if(level == "names") return(nm)
    entries <- trans[[nm]]
    # each entry names the method first and its consequence in parentheses
    if(level == "methods") entries <- trimws(sub("\\s*\\([^()]*\\)$", "",
                                                 entries))
    paste0(nm, ": ", paste(entries, collapse = " then "))
  }, character(1), USE.NAMES = FALSE)
}

pluralize <- function(word) {
  if(length(word) > 1) return(sapply(word, pluralize))
  if (grepl("(us|ss|x|z|ch|sh)$", word)) {
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
  } else if (grepl("(ss|us|is)$", word)) {
    word
  } else if (grepl("s$", word)) {
    sub("s$", "", word)
  } else {
    word
  }
}

phrase <- function(items) {
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
