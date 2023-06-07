#' Tools for joining lists of networks, graphs, and matrices
#' 
#' @description
#'   These functions offer tools for joining lists of manynet-consistent objects
#'   (matrices, igraph, tidygraph, or network objects).
#'   Joining expects a list of objects and returns a single network object.
#' @name from
#' @family manipulations
#' @inheritParams reformat
NULL

#' @describeIn from Returns a single network object
#'  from a list of subgraphs.
#' @importFrom igraph graph_from_data_frame as_data_frame set_vertex_attr
#' @examples
#' ison_adolescents %>%
#'   mutate(unicorn = sample(c("yes", "no"), 8, replace = TRUE)) %>%
#'   to_subgraphs(attribute = "unicorn") %>%
#'   from_subgraphs()
#' @export
from_subgraphs <- function(.data) {
  if (!is.list(.data[1])) {
    stop("Please declare a list of subgraphs. ")
  }
  ann <- lapply(.data, as_igraph)
  out <- ann[[1]]
  for (i in seq_along(ann)[-1]) {
    out <- join_nodes(out, ann[[i]])
  }
  for (i in seq_along(ann)[-1]) {
    out <- join_ties(out, ann[[i]])
  }
  orig <- object2 <- NULL
  out <- select_ties(out, -c(orig, object2))
  out
}

#' @describeIn from Returns a single network object
#'  from a list of egos.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   to_egos() %>%
#'   from_egos()
#' @export
from_egos <- function(.data) {
  if (!is.list(.data[1])) {
    stop("Please declare a list of egos.")
  }
  ann <- lapply(.data, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  as_tidygraph(igraph::graph_from_data_frame(dplyr::distinct(out)))
}

#' @describeIn from Returns a single network object
#'  from a list of waves.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @examples
#' ison_adolescents %>%
#'   mutate_ties(wave = sample(1:4, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   from_waves()
#' @export
from_waves <- function(.data) {
  if (!is.list(.data[1])) {
    stop("Please declare a list of waves.")
  }
  ann <- lapply(.data, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  as_tidygraph(igraph::graph_from_data_frame(out))
}

#' @describeIn from Returns a single network object
#'  from a list of slices.
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
from_slices <- function(.data, remove.duplicates = FALSE) {
  if (is.list(.data[1])) {
    ann <- lapply(.data, as_igraph)
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
