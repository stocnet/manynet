# Classes ####

#' Marking networks classes
#'
#' @description
#'   These functions implement logical tests for networks' classes.
#'   
#'   - `is_manynet()` marks a network TRUE if it is compatible with `{manynet}` functions.
#'   - `is_edgelist()` marks a network TRUE if it is an edgelist.
#'   - `is_graph()` marks a network TRUE if it contains graph-level information.
#'   - `is_list()` marks a network TRUE if it is a (non-igraph) list of networks,
#'   for example a set of ego networks or a dynamic or longitudinal set of networks.
#'   - `is_longitudinal()` marks a network TRUE if it contains longitudinal, panel data.
#'   - `is_dynamic()` marks a network TRUE if it contains dynamic, time-stamped data.
#'   - `is_changing()` marks a network TRUE if it contains changes to nodal attributes.
#'   
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#' @template param_data
#' @return TRUE if the condition is met, or FALSE otherwise.
#' @family marking
#' @name mark_is
NULL

#' @rdname mark_is
#' @importFrom igraph is_igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @examples
#' is_manynet(create_filled(2))
#' @export
is_manynet <- function(.data) UseMethod("is_manynet")

#' @export
is_manynet.default <- function(.data){FALSE}

#' @export
is_manynet.matrix <- function(.data){is.numeric(.data)}

#' @export
is_manynet.data.frame <- function(.data){
  "from" %in% names(.data) & "to" %in% names(.data)
}

#' @export
is_manynet.igraph <- function(.data){igraph::is_igraph(.data)}

#' @export
is_manynet.tbl_graph <- function(.data){tidygraph::is.tbl_graph(.data)}

#' @export
is_manynet.network <- function(.data) {network::is.network(.data)}

#' @export
is_manynet.stocnet <- function(.data) {inherits(.data, "stocnet")}

manynet_classes <- c("stocnet" = "stocnet",
                     "tidygraph" = "tbl_graph", 
                     "igraph" = "igraph", 
                     "network" = "network", 
                     "matrix" = "matrix"
                     )

#' @rdname mark_is
#' @importFrom igraph is_igraph
#' @importFrom tidygraph is.tbl_graph
#' @importFrom network is.network
#' @examples
#' is_graph(create_star(2))
#' @export
is_graph <- function(.data) UseMethod("is_graph")

#' @export
is_graph.default <- function(.data){FALSE}

#' @export
is_graph.tbl_graph <- function(.data){TRUE}

#' @export
is_graph.igraph <- function(.data){TRUE}

#' @export
is_graph.network <- function(.data){TRUE}

#' @export
is_graph.stocnet <- function(.data){TRUE}

#' @rdname mark_is
#' @examples
#' is_edgelist(matrix(c(2,2), 1, 2))
#' is_edgelist(as_edgelist(matrix(c(2,2), 1, 2)))
#' @export
is_edgelist <- function(.data) UseMethod("is_edgelist")
  
#' @export
is_edgelist.default <- function(.data){FALSE}

#' @export
is_edgelist.data.frame <- function(.data) {
  ncol(.data) >= 2 & "from" %in% names(.data) & "to" %in% names(.data)
}

#' @rdname mark_is
#' @export
is_list <- function(.data) UseMethod("is_list")

#' @export
is_list.default <- function(.data){FALSE}

#' @export
is_list.list <- function(.data) {
  !is_manynet(.data)
}

