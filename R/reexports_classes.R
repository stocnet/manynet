#' @importFrom tidygraph with_graph
#' @export
tidygraph::with_graph

#' @importFrom tidygraph is.tbl_graph
#' @export
tidygraph::is.tbl_graph

#' @importFrom tidygraph .G
#' @export
tidygraph::.G

#' @importFrom tidygraph .N
#' @export
tidygraph::.N

#' @importFrom tidygraph .E
#' @export
tidygraph::.E

# nocov start

#' Expecting either nodes or ties to be active
#' @keywords internal
#' @name expect
NULL

#' @rdname expect
#' @export
expect_nodes <- function(.data) {
  
  obj <- expect_igraph(.data, "nodes")
  
  if (is.null(obj))
    snet_abort(
      "This call requires nodes to be active",
      call. = FALSE
    )
  
  obj
}

#' @rdname expect
#' @export
expect_ties <- function(.data) {
  
  obj <- expect_igraph(.data, "edges")
  
  if (is.null(obj))
    snet_abort(
      "This call requires ties to be active",
      call. = FALSE
    )
  
  obj
}

expect_igraph <- function(.data, context = NULL) {
  
  if (!missing(.data))
    return(as_igraph(.data))
  
  if (!is.null(context)) {
    
    obj <- active_network(context)
    
    if (!is.null(obj))
      return(as_igraph(obj))
  }
  
  tg <- tryCatch(
    tidygraph::.G(),
    error = function(e) NULL
  )
  
  if (!is.null(tg))
    return(as_igraph(tg))
  
  NULL
}

.active_context <- new.env(parent = emptyenv())
.active_context$data <- NULL
.active_context$active <- NULL

get_active_context <- function() {
  list(
    data = .active_context$data,
    active = .active_context$active
  )
}

set_active_context <- function(data, active) {
  .active_context$data <- data
  .active_context$active <- active
}

clear_active_context <- function() {
  .active_context$data <- NULL
  .active_context$active <- NULL
}

active_network <- function(required = NULL) {
  
  ctx <- get_active_context()
  
  if (!is.null(ctx$data)) {
    
    if (!is.null(required) &&
        !identical(ctx$active, required)) {
      snet_abort(
        paste0(
          "This call requires ",
          if (identical(required, "edges")) "ties" else required,
          " to be active"
        ),
        call. = FALSE
      )
    }
    
    return(ctx$data)
  }
  
  tg <- tryCatch({
    
    if (!tidygraph::.graph_context$free()) {
      
      active <- tidygraph::.graph_context$active()
      
      if (!is.null(required) &&
          active != required) {
        snet_abort(
          paste0(
            "This call requires ",
            if (identical(required, "edges")) "ties" else required,
            " to be active"
          ),
          call. = FALSE
        )
      }
      
      tidygraph::.G()
    } else {
      NULL
    }
    
  }, error = function(e) NULL)
  
  tg
}

with_active_context <- function(data, active, expr) {
  
  old <- get_active_context()
  
  on.exit(
    set_active_context(old$data, old$active),
    add = TRUE
  )
  
  set_active_context(data, active)
  
  eval.parent(substitute(expr))
}

# nocov end