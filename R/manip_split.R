#' Splitting networks into lists
#' 
#' @description
#'   These functions offer tools for splitting manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects) into lists of networks.
#' 
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   |              | data.frame| diff_model| igraph| matrix| network| tbl_graph|
#'   |:-------------|----------:|----------:|------:|------:|-------:|---------:|
#'   |to_components |          1|          0|      1|      1|       1|         1|
#'   |to_egos       |          1|          0|      1|      1|       1|         1|
#'   |to_slices     |          0|          0|      1|      0|       0|         1|
#'   |to_subgraphs  |          0|          0|      1|      0|       1|         1|
#'   |to_waves      |          1|          1|      1|      0|       0|         1|
#' @name manip_split
#' @family modifications
#' @inheritParams manip_scope
#' @return The returned object will be a list of network objects.
NULL

#' @describeIn manip_split Returns a list of ego (or focal)
#'   networks.
#' @importFrom igraph make_ego_graph
#' @examples
#'   to_egos(ison_adolescents)
#'   # graphs(to_egos(ison_adolescents,2))
#' @export
to_egos <- function(.data, 
                    max_dist = 1, 
                    min_dist = 0,
                    direction = c("out","in")) UseMethod("to_egos")

#' @export
to_egos.igraph <- function(.data, 
                           max_dist = 1, 
                           min_dist = 0,
                           direction = c("out","in")){
  if(is_twomode(.data)) max_dist <- max_dist*2
  snet_progress_step("Obtaining neighbourhoods")
  out <- igraph::make_ego_graph(.data,
                                order = max_dist,
                                mindist = min_dist,
                                mode = match.arg(direction))
  if(is_labelled(.data)) 
    names(out) <- node_names(.data)
  out
}

#' @export
to_egos.tbl_graph <- function(.data, 
                           max_dist = 1, 
                           min_dist = 0,
                           direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_egos.network <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0,
                            direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_network(x))
}

#' @export
to_egos.matrix <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0,
                           direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_egos.data.frame <- function(.data, 
                              max_dist = 1, 
                              min_dist = 0,
                              direction = c("out","in")){
  out <- to_egos(as_igraph(.data), 
                       max_dist, 
                       min_dist, direction)
  lapply(out, function(x) as_edgelist(x))
}

#' @describeIn manip_split Returns a list of subgraphs
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

#' @describeIn manip_split Returns a list of the components
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

#' @describeIn manip_split Returns a network
#'   with some discrete observations over time
#'   into a list of those observations.
#' @param attribute Character string indicating the date
#'   attribute in a network used to split into subgraphs.
#' @param panels Would you like to select certain waves?
#'   NULL by default.
#'   That is, a list of networks for every available wave is returned.
#'   Users can also list specific waves they want to select.
#' @param cumulative Whether to make wave ties cumulative.
#'   FALSE by default. That is, each wave is treated isolated.
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave")
#' @export
to_waves <- function(.data, attribute = "wave", panels = NULL,
                     cumulative = FALSE) UseMethod("to_waves")

#' @importFrom tidygraph to_subgraph as_tbl_graph
#' @export
to_waves.tbl_graph <- function(.data, attribute = "wave", panels = NULL,
                               cumulative = FALSE) {
  wp <- unique(tie_attribute(.data, attribute))
  if(!is.null(panels))
    wp <- intersect(panels, wp)
  if(length(wp) > 1) {
    out <- lapply(wp, function(l){
      filter_ties(.data, !!as.name(attribute) == l)
    })
    names(out) <- wp
  } else {
    out <- filter_ties(.data, !!as.name(attribute) == wp)
  }
  if (isTRUE(cumulative)) {
    out <- cumulative_ties(out, attribute)
  }
  out[order(names(out))]
}

#' @export
to_waves.igraph <- function(.data, attribute = "wave", panels = NULL,
                            cumulative = FALSE) {
  out <- to_waves(as_tidygraph(.data), attribute, panels, cumulative)
  if(length(out) > 1) lapply(out, as_igraph) else as_igraph(out)
}

#' @export
to_waves.data.frame <- function(.data, attribute = "wave", panels = NULL,
                                cumulative = FALSE) {
  wp <- unique(tie_attribute(.data, attribute))
  if(!is.null(panels)) wp <- intersect(panels, wp)
  if(length(wp) > 1) {
    out <- lapply(wp, function(l) .data[,attribute == l])
    names(out) <- wp
  } else if(length(wp) > 1) {
    out <- .data[,attribute == wp]
  }
  if (isTRUE(cumulative)) {
    out <- cumulative_ties(out, attribute)
  }
  out
}

#' @export
to_waves.diff_model <- function(.data, attribute = "t", panels = NULL,
                                cumulative = FALSE) {
  if (!is.null(panels)) .data <- .data[.data[[attribute]] %in% panels,]
  if (length(unique(.data[["n"]])) > 1)
    snet_abort("Please make sure diffusion has the same number of nodes for all time points.")
  net <- as_tidygraph(.data)
  diff <- .data
  out <- list()
  for (k in .data[[attribute]]) {
    out[[paste("Time:", formatC(k, width = max(nchar(.data[[attribute]])),
                                flag = 0))]] <- net %>%
      tidygraph::mutate(Infected = node_is_infected(diff, time = k),
                        Exposed = node_is_latent(diff, time = k),
                        Recovered = node_is_recovered(diff, time = k))
  }
  if (isTRUE(cumulative)) {
    out <- cumulative_ties(out, attribute)
  }
  out
}

cumulative_ties <- function(x, attribute) {
  edges <- to <- from <- NULL
  thisRequires("zoo")
  thisRequires("purrr")
  ties <- data.frame()
  x <- lapply(x, as_tidygraph)
  for (k in seq_len(length(names(x)))) {
    a <- x[[k]] %>%
      tidygraph::activate(edges) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(order = k) %>%
      dplyr::select(to, from, dplyr::all_of(attribute), order)
    ties <- rbind(ties, a)
  }
  if (is.numeric(ties[[attribute]])) {
    ties <- ties[order(ties[[attribute]]),]
    a <- list()
    for (k in unique(ties[[attribute]])) {
      if (k != unique(ties[[attribute]][1])) {
        a[[as.character(k)]] <- subset(ties, ties[[attribute]] < k)[1:3]
        a[[as.character(k)]][attribute] <- k
      }
    }
  } else {
    snet_info("Cumulative ties were added based on order of appearance for attribute.")
    a <- list()
    for (k in unique(ties$order)) {
      if (k != 1) {
        a[[unique(ties[[attribute]][k])]] <- subset(ties, ties$order < k)[1:3]
        a[[unique(ties[[attribute]][k])]][attribute] <- k
      }
    }
  }
  for (k in names(a)) {
    x[[k]] <- igraph::add.edges(
      x[[k]], c(a[[k]]$to, a[[k]]$from)[order(c(ceiling(seq_along(a[[k]]$to)/1),
                                                seq_along(a[[k]]$from)))],
      attr = a[[k]][3])
  }
  lapply(x, as_tidygraph)
}

#' @describeIn manip_split Returns a list of a network
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
  incremented <- "increment" %in% net_tie_attributes(.data)
  updated <- "replace" %in% net_tie_attributes(.data)
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
