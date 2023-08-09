#' Tools for splitting networks, graphs, and matrices into lists
#' 
#' @description
#'   These functions offer tools for splitting manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects) into lists of networks.
#' 
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'    ```{r, echo = FALSE, cache = TRUE} 
#'    knitr::kable(available_methods(c("to_egos", "to_subgraphs", "to_components", "to_waves", "to_slices")))
#'    ```
#' @name split
#' @family manipulations
#' @inheritParams reformat
#' @return The returned object will be a list of network objects.
NULL

#' @describeIn split Returns a list of ego (or focal)
#'   networks.
#' @param max_dist The maximum breadth of the neighbourhood.
#'   By default 1.
#' @param min_dist The minimum breadth of the neighbourhood.
#'   By default 0. 
#'   Increasing this to 1 excludes the ego,
#'   and 2 excludes ego's direct alters.
#' @importFrom igraph make_ego_graph
#' @examples
#'   to_egos(ison_adolescents)
#'   #autographs(to_egos(ison_adolescents,2))
#' @export
to_egos <- function(.data, 
                    max_dist = 1, 
                    min_dist = 0) UseMethod("to_egos")

#' @export
to_egos.igraph <- function(.data, 
                           max_dist = 1, 
                           min_dist = 0){
  if(is_twomode(.data)) max_dist <- max_dist*2
  out <- igraph::make_ego_graph(.data,
                                order = max_dist,
                                mindist = min_dist)
  if(is_labelled(.data)) 
    names(out) <- node_names(.data)
  out
}

#' @export
to_egos.tbl_graph <- function(.data, 
                           max_dist = 1, 
                           min_dist = 0){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_egos.network <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_network(x))
}

#' @export
to_egos.matrix <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_egos.data.frame <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_edgelist(x))
}

#' @describeIn split Returns a list of subgraphs
#'   on some given node attribute.
#' @param attribute A character string indicating the categorical
#'   attribute in a network used to split into subgraphs.
#' @importFrom igraph induced_subgraph
#' @examples
#' ison_adolescents %>%
#'   mutate(unicorn = sample(c("yes", "no"), 8,
#'                           replace = TRUE)) %>%
#'   to_subgraphs(attribute = "unicorn")
#' @export
to_subgraphs <- function(.data, attribute) UseMethod("to_subgraphs")

#' @export
to_subgraphs.igraph <- function(.data, attribute){
  types <- unique(node_attribute(.data, attribute))
  lapply(types, function(x) igraph::induced_subgraph(.data, 
                              node_attribute(.data, attribute) == x))
}

#' @export
to_subgraphs.tbl_graph <- function(.data, attribute){
  lapply(to_subgraphs(as_igraph(.data), attribute), as_tidygraph)
}

#' @export
to_subgraphs.network <- function(.data, attribute){
  lapply(to_subgraphs(as_igraph(.data), attribute), as_network)
}

#' @describeIn split Returns a list of the components
#'   in a network.
#' @examples 
#'   to_components(ison_marvel_relationships)
#' @export
to_components <- function(.data) UseMethod("to_components")

#' @importFrom igraph decompose
#' @export
to_components.igraph <- function(.data){
  igraph::decompose(.data)
}

#' @export
to_components.tbl_graph <- function(.data){
  out <- to_components.igraph(as_igraph(.data))
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_components.network <- function(.data){
  out <- to_components.igraph(as_igraph(.data))
  lapply(out, function(x) as_network(x))
}

#' @export
to_components.matrix <- function(.data){
  out <- to_components.igraph(as_igraph(.data))
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_components.data.frame <- function(.data){
  out <- to_components.igraph(as_igraph(.data))
  lapply(out, function(x) as_edgelist(x))
}

#' @describeIn split Returns a network
#'   with some discrete observations over time
#'   into a list of those observations.
#' @param attribute Character string indicating the date
#'   attribute in a network used to split into subgraphs.
#' @param panels Would you like to select certain waves?
#'   NULL by default.
#'   That is, a list of networks for every available wave is returned.
#'   Users can also list specific waves they want to select.
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave")
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave", panels = c(1995, 1996))
#' @export
to_waves <- function(.data, attribute = "wave", panels = NULL) UseMethod("to_waves")

#' @importFrom tidygraph to_subgraph as_tbl_graph
#' @export
to_waves.tbl_graph <- function(.data, attribute = "wave", panels = NULL) {
  
  wp <- unique(tie_attribute(.data, attribute))
  if(!is.null(panels))
    wp <- intersect(panels, wp)
  if(length(wp)>1){
    out <- lapply(wp, function(l){
      filter_ties(.data, !!as.name(attribute) == l)
    })
    names(out) <- wp
  } else {
    out <- filter_ties(.data, !!as.name(attribute) == wp)
  }
  out
}

#' @export
to_waves.igraph <- function(.data, attribute = "wave", panels = NULL) {
  out <- to_waves(as_tidygraph(.data))
  if(length(out)>1){
    lapply(out, function(o) as_igraph(0))  
  } else as_igraph(out)
}

#' @export
to_waves.data.frame <- function(.data, attribute = "wave", panels = NULL) {
  wp <- unique(tie_attribute(.data, attribute))
  if(!is.null(panels))
    wp <- intersect(panels, wp)
  if(length(wp)>1){
    out <- lapply(wp, function(l) .data[,attribute == l])
    names(out) <- wp
  } else if(length(wp)>1){
    out <- .data[,attribute == wp]
  }
  out
}

#' @describeIn split Returns a list of a network
#'   with some continuous time variable at some time slice(s).
#' @param attribute One or two attributes used to slice data.
#' @param slice Character string or character list indicating the date(s)
#'   or integer(s) range used to slice data (e.g slice = c(1:2, 3:4)).
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(time = 1:10, increment = 1) %>% 
#'   add_ties(c(1,2), list(time = 3, increment = -1)) %>% 
#'   to_slices(slice = 7)
#' @export
to_slices <- function(.data, attribute = "time", slice = NULL) UseMethod("to_slices")

#' @export
to_slices.tbl_graph <- function(.data, attribute = "time", slice = NULL) {
  increment <- weight <- NULL
  incremented <- "increment" %in% network_tie_attributes(.data)
  updated <- "replace" %in% network_tie_attributes(.data)
  if(!is.null(slice))
    moments <- slice else 
      moments <- unique(tie_attribute(.data, attribute = attribute))
  if(length(moments)>1){
    out <- lapply(moments, function(tm){
      snap <- filter_ties(.data, !!as.name(attribute) <= tm)
      if(incremented) snap <- summarise_ties(snap, sum(increment))
      if(updated) snap <- summarise_ties(snap, dplyr::last(replace))
      snap <- filter_ties(snap, weight != 0)
      snap
    })
    names(out) <- moments
  } else {
    out <- filter_ties(.data, !!as.name(attribute) <= moments)
    if(incremented) out <- summarise_ties(out, sum(increment))
    if(updated) out <- summarise_ties(out, dplyr::last(replace))
    out <- filter_ties(out, weight != 0)
  }
  out
}

#' @export
to_slices.igraph <- function(.data, attribute = "time", slice = NULL) {
  out <- to_slices(as_tidygraph(.data), attribute, slice)
  if(is.list(out) & !is_graph(out)) 
    lapply(out, function(ea) as_igraph(ea)) else
      as_igraph(out)
}
