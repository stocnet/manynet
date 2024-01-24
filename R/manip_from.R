#' Joining lists of networks, graphs, and matrices
#' 
#' @description
#'   These functions offer tools for joining lists of manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects) into a single object.
#'   
#'   - `from_subgraphs()` modifies a list of subgraphs into a single tidygraph.
#'   - `from_egos()` modifies a list of ego networks into a whole tidygraph
#'   - `from_waves()` modifies a list of network waves into a longitudinal tidygraph.
#'   - `from_slices()` modifies a list of time slices of a network into 
#'   a dynamic tidygraph.
#'   - `from_ties()` modifies a list of different ties into a multiplex tidygraph
#' @name from
#' @family modifications
#' @param netlist A list of network, igraph, tidygraph, matrix, or edgelist objects.
#' @param netnames A character vector of names for the different network objects,
#'   if not already named within the list.
#' @return A tidygraph object combining the list of network data.
NULL

#' @rdname from
#' @importFrom igraph graph_from_data_frame as_data_frame set_vertex_attr
#' @examples
#' ison_adolescents %>%
#'   mutate(unicorn = sample(c("yes", "no"), 8, replace = TRUE)) %>%
#'   to_subgraphs(attribute = "unicorn") %>%
#'   from_subgraphs()
#' @export
from_subgraphs <- function(netlist) {
  if (!is.list(netlist[1])) {
    stop("Please declare a list of subgraphs. ")
  }
  ann <- lapply(netlist, as_igraph)
  out <- ann[[1]]
  for (i in seq_along(ann)[-1]) {
    out <- join_nodes(out, ann[[i]])
  }
  for (i in seq_along(ann)[-1]) {
    out <- join_ties(out, ann[[i]])
  }
  orig <- object2 <- NULL
  out <- select_ties(out, -c(orig, object2))
  as_tidygraph(out)
}

#' @rdname from
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents %>%
#'   to_egos() %>%
#'   from_egos()
#' @export
from_egos <- function(netlist) {
  if (!is.list(netlist[1])) {
    stop("Please declare a list of egos.")
  }
  ann <- lapply(netlist, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  as_tidygraph(igraph::graph_from_data_frame(dplyr::distinct(out)))
}

#' @rdname from 
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1:4, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   from_waves()
#' @export
from_waves <- function(netlist) {
  if (!is.list(netlist[1])) {
    stop("Please declare a list of waves.")
  }
  ann <- lapply(netlist, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  as_tidygraph(igraph::graph_from_data_frame(out))
}

#' @rdname from 
#' @param remove.duplicates Should duplicates be removed?
#' By default FALSE.
#' If TRUE, duplicated edges are removed.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(time = 1:10, increment = 1) %>% 
#'   add_ties(c(1,2), list(time = 3, increment = -1)) %>% 
#'   to_slices(slice = c(5,7)) %>%
#'   from_slices()
#' @export
from_slices <- function(netlist, remove.duplicates = FALSE) {
  if (is.list(netlist[1])) {
    ann <- lapply(netlist, as_igraph)
    out <- igraph::as_data_frame(ann[[1]])
    for (i in seq_along(ann)[-1]){
      out <- rbind(out, igraph::as_data_frame(ann[[i]]))
    }
    if (isTRUE(remove.duplicates)) {
      out <- dplyr::distinct(out)
    }
    as_tidygraph(igraph::graph_from_data_frame(out))
  } else {
    message("Only one slice is available, cannot be joined.")
  }
}

#' @rdname from
#' @export
from_ties <- function(netlist, netnames){
  stopifnot(is_list(netlist))
  if(is.null(names(netlist))){
    if(!missing(netnames)){
      names(netlist) <- netnames
    } else stop(paste("Please name the elements of the list of networks",
                      "or provide a vector of names for them."))
  }
  netlist <- lapply(seq_along(netlist), 
                    function(x) if(is_multiplex(netlist[[x]])){
                      netlist[[x]] } else { 
                        mutate_ties(netlist[[x]], type = names(netlist)[x])
                        })
  Reduce(tidygraph::graph_join, netlist)
}
