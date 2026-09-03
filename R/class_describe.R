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
         # A multilevel network whose layers are its levels is multiplex only
         # because it is multilevel, so reporting both says the same thing
         # twice. A layer that is not a level, such as one of the four kinds
         # of tie within `fict_actually`'s characters, still earns the word.
         ifelse(is_multiplex(.data) && !.layers_are_levels(.data),
                "multiplex, ", ""),
         ifelse(is_signed(.data), "signed, ", ""),
         ifelse(is_weighted(.data), "weighted, ", ""),
         # A multilevel network has more than one mode, so naming it
         # multilevel already says that it is not one-mode, and says more
         # besides: that its levels are tied within as well as between.
         ifelse(is_multilevel(.data), "multilevel",
                ifelse(is_twomode(.data), "two-mode", 
                       ifelse(is_directed(.data), "directed", "undirected"))),
         " network"
  )
}

# The level each tie occupies: "between", where its ends are in different
# levels, and otherwise the level both of its ends are in.
.tie_positions <- function(.data){
  if(inherits(.data, "stocnet")){
    if(is.null(.data$nodes) || !"mode" %in% names(.data$nodes)) return(NULL)
    modes <- as.character(.data$nodes$mode)
    from <- .data$ties$from
    to <- .data$ties$to
  } else {
    graph <- as_igraph(.data)
    modes <- if("lvl" %in% igraph::vertex_attr_names(graph))
      as.character(igraph::vertex_attr(graph, "lvl")) else
        if(is_twomode(graph)) as.character(node_is_mode(graph)) else NULL
    if(is.null(modes)) return(NULL)
    el <- igraph::as_edgelist(graph, names = FALSE)
    from <- el[,1]
    to <- el[,2]
  }
  ifelse(modes[from] != modes[to], "between", modes[from])
}

# Whether a network's layers are its levels: every layer sits at one level
# position, and no two layers share a position. Where that holds, the network
# is multiplex only in that its levels are tied within as well as between,
# which is what marking it multilevel already reports.
.layers_are_levels <- function(.data){
  if(!isTRUE(tryCatch(is_multilevel(.data), error = function(e) FALSE)))
    return(FALSE)
  atts <- net_tie_attributes(.data)
  layer <- intersect(c("layer", "type"), atts)[1]
  if(is.na(layer)) return(FALSE)
  layers <- as.character(tie_attribute(.data, layer))
  positions <- .tie_positions(.data)
  if(is.null(positions) || length(positions) != length(layers)) return(FALSE)
  # A layer spanning more than one position is not a level.
  if(any(tapply(positions, layers, function(p) length(unique(p))) > 1))
    return(FALSE)
  # Two layers at one position are two kinds of tie there, not two levels.
  !anyDuplicated(tapply(positions, layers, function(p) p[[1]]))
}

#' @rdname class_describe
#' @export
describe_nodes <- function(.data){
  nd <- mode_nodes(.data)
  nn <- mode_names(.data)
  # A network that names its modes gives one name for each count. Where it
  # names fewer or more than it counts, no name can be matched to a count
  # with confidence, so every mode is described by the general word instead.
  if(is.null(nn) || length(nn) != length(nd)) nn <- rep("nodes", length(nd))
  # `ifelse()` returns as many values as its first argument holds, so it
  # would report only the first name where there are three or more modes.
  nn <- vapply(seq_along(nd),
               function(i) if(nd[i] == 1) singularize(nn[i]) else
                 pluralize(nn[i]),
               character(1))
  node_name <- paste(nd, nn)
  phrase(node_name)
}

#' @rdname class_describe
#' @export
describe_ties <- function(.data){
  nt <- net_ties(.data)
  tie_name <- ifelse(is_directed(.data), "arcs", "ties")
  # Parallel ties are reported as a count and not as a property of the network,
  # since a network holds them without every tie in it running parallel.
  npar <- sum(tie_is_parallel(.data))
  parallel <- if(npar) paste0(" (", npar, " parallel)") else ""
  if(!is.null(layer_names(.data))){
    # Where a network records the directedness of each layer, an undirected
    # layer of an otherwise directed network holds ties rather than arcs.
    directed <- as_infolist(.data)$directed
    layer_name <- if(!is.null(directed) && !is.null(names(directed)))
      ifelse(directed[layer_names(.data)], "arcs", "ties") else tie_name
    parts <- paste0(layer_ties(.data), " ", singularize(layer_names(.data)),
                    " ", layer_name)
    return(paste0(phrase(parts), parallel))
  } else if(!is.na(.layer_attribute(.data))){
    tab <- table(tie_attribute(.data, .layer_attribute(.data)))
    parts <- paste0(tab, " ", singularize(names(tab)))
    return(paste0(phrase(parts), " ", tie_name, parallel))
  }
  paste0(nt, " ", tie_name, parallel)
}

#' @rdname class_describe
#' @export
describe_changes <- function(.data){
  if(is_longitudinal(.data)){
    # The count of the waves, and not the last of them, since a panel may
    # number its waves by the year or the week it observed them in.
    paste(" over", net_waves(.data), "waves")
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
