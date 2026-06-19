# Manipulating ties number ####

#' Manipulating number of ties
#' @name manip_ties_num
#' @description 
#'   These functions allow users to add and delete ties:
#'   
#'   - `add_ties()` adds additional ties to network data
#'   - `delete_ties()` deletes ties from network data
#'   - `bind_ties()` appends the tie data from two networks
#'   - `filter_ties()` subsets ties based on some tie attribute-related logical statement.
#'   
#'   While `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `filter*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @template param_data
#' @template param_dots
#' @family ties
#' @template fam_manip
#' @eval detail_avail("(add|del|bind|filter).*ties")
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   mutate_ties(other, form = 1:6) |> filter_ties(form < 4)
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
NULL

#' @rdname manip_ties_num
#' @param ties The number of ties to be added or an even list of ties.
#' @param attr_list A list of attributes to be added to the new ties.
#' @importFrom igraph add_edges
#' @examples
#' ison_adolescents |> add_ties(c("Betty","Tina"))
#' @export
add_ties <- function(.data, ties, attr_list = NULL) UseMethod("add_ties")

#' @export
add_ties.default <- function(.data, ties, attr_list = NULL){
  as_input(.data, add_ties, ties = ties, attr_list = attr_list)
}

#' @export
add_ties.igraph <- function(.data, ties, attr_list = NULL){
  igraph::add_edges(.data, edges = ties, attr = attr_list)
}

#' @export
add_ties.tbl_graph <- function(.data, ties, attr_list = NULL){
  as_tidygraph(add_ties(as_igraph(.data), ties, attr_list))
}

#' @export
add_ties.network <- function(.data, ties, attr_list = NULL){
  as_network(add_ties(as_igraph(.data), ties, attr_list))
}

#' @rdname manip_ties_num
#' @importFrom igraph delete_edges
#' @examples
#' delete_ties(ison_adolescents, 3)
#' delete_ties(ison_adolescents, "Alice|Sue")
#' @export
delete_ties <- function(.data, ties) UseMethod("delete_ties")

#' @export
delete_ties.default <- function(.data, ties){
  as_input(.data, delete_ties, ties = ties)
}

#' @export
delete_ties.igraph <- function(.data, ties){
  igraph::delete_edges(.data, edges = ties)
}

#' @export
delete_ties.tbl_graph <- function(.data, ties){
  as_tidygraph(igraph::delete_edges(.data, edges = ties))
}

#' @export
delete_ties.network <- function(.data, ties){
  as_network(igraph::delete_edges(as_igraph(.data), edges = ties))
}

#' @rdname manip_ties_num
#' @importFrom tidygraph bind_edges
#' @export
bind_ties <- function(.data, ...) UseMethod("bind_ties")

#' @export
bind_ties.default <- function(.data, ...){
  as_input(.data, bind_ties, ...)
}

#' @export
bind_ties.tbl_graph <- function(.data, ...){
  toAdd <- as_edgelist(...)
  tidygraph::bind_edges(.data, toAdd) |> 
    arrange_ties(from, to)
}

#' @export
bind_ties.stocnet <- function(.data, ...){
  out <- .data
  toAdd <- as_edgelist(...)
  
  # Track whether these are incremental changes rather than a full replacement
  was_increment <- "increment" %in% names(toAdd)
  was_replacement <- "replace" %in% names(toAdd)
  
  # Rename to stocnet conventions, following the same aliases recognised
  # by validate_ties()
  rename_map <- c(sender = "from", source = "from", ego = "from",
                  receiver = "to", target = "to", alter = "to",
                  increment = "weight", value = "weight", replace = "weight",
                  strength = "weight", val = "weight", sign = "weight",
                  type = "layer", plex = "layer", tie = "layer",
                  wave = "time", period = "time", date = "time",
                  begin = "time", end = "time")
  for(old in names(rename_map)){
    new <- rename_map[[old]]
    if(old %in% names(toAdd) && !(new %in% names(toAdd)))
      names(toAdd)[names(toAdd) == old] <- new
  }
  
  if(!all(c("from", "to") %in% names(toAdd)))
    snet_abort(paste("The data to bind must identify the sender and",
                     "receiver of each tie (e.g. as 'from'/'to',",
                     "'sender'/'receiver', etc.)."))
  
  ## Map labelled endpoints onto .data's existing node indexing -----------
  if(is.character(toAdd$from) || is.character(toAdd$to)){
    if(is.null(out$nodes) || !"label" %in% names(out$nodes))
      snet_abort(paste("Cannot match labelled ties to a network",
                       "without node labels."))
    new_from <- match(toAdd$from, out$nodes$label)
    new_to   <- match(toAdd$to,   out$nodes$label)
    if(anyNA(new_from) || anyNA(new_to))
      snet_warn(paste("Some labels in the data to bind do not match node",
                      "labels in '.data'; these ties will have missing",
                      "endpoints."))
    toAdd$from <- new_from
    toAdd$to   <- new_to
  }
  toAdd$from <- as.integer(toAdd$from)
  toAdd$to   <- as.integer(toAdd$to)
  
  ## If .data is single-layer with a 'layer' column, tag the new ties -----
  if("layer" %in% names(out$ties) && !"layer" %in% names(toAdd) &&
     length(out$info$layers) == 1)
    toAdd$layer <- out$info$layers
  
  ## Align 'time' column type if .data$ties$time is currently all-NA logical
  if("time" %in% names(toAdd) && "time" %in% names(out$ties) &&
     is.logical(out$ties$time) && all(is.na(out$ties$time)) &&
     !is.logical(toAdd$time)){
    out$ties$time <- rep(toAdd$time[NA_integer_], nrow(out$ties))
  }
  
  ## Bind, ordering columns as in .data$ties (new columns appended after) -
  out$ties <- dplyr::bind_rows(out$ties, toAdd)
  col_order <- union(names(.data$ties), names(out$ties))
  out$ties <- out$ties[, col_order]
  
  out <- arrange_ties(out, from, to)
  
  ## Record update behaviour for the relevant layer -----------
  if(was_increment || was_replacement){
    updateyo <- if(was_increment) "increment" else "replace"
    layers <- out$info$layers
    if(is.null(layers) && "layer" %in% names(out$ties))
      layers <- unique(stats::na.omit(out$ties$layer))
    if(length(layers) == 1){
      ly <- layers
      if(is.null(out$info[[ly]])) out$info[[ly]] <- list()
      out$info[[ly]]$update <- updateyo
      upd <- out$info$update
      if(is.null(upd)) upd <- stats::setNames(updateyo, ly)
      else upd[ly] <- updateyo
      out$info$update <- upd
    } else {
      out$info$update <- updateyo
    }
  }
  
  out
}

#' @rdname manip_ties_num 
#' @importFrom dplyr filter
#' @export
filter_ties <- function(.data, ...) UseMethod("filter_ties")

#' @export
filter_ties.default <- function(.data, ...){
  as_input(.data, filter_ties, ...)
}

#' @export
filter_ties.igraph <- function(.data, ...){
  as_tidygraph(.data) |> tidygraph::activate(edges) |> 
    tidygraph::filter(...) |> 
    tidygraph::activate(nodes) |> 
    as_igraph()
}

#' @export
filter_ties.tbl_graph <- function(.data, ...){
  .data |> tidygraph::activate(edges) |> 
    dplyr::filter(...) |> 
    tidygraph::activate(nodes)
}

#' @export
filter_ties.stocnet <- function(.data, ...){
  with_active_context(.data, "edges", {
    out <- .data
    out$ties <- out$ties |> dplyr::filter(...)
    out
  })
}

# Manipulating ties attributes ####

#' Manipulating tie attributes
#' @name manip_ties_attr
#' @description 
#'   These functions allow users to add and delete tie attributes:
#'   
#'   - `add_tie_attribute()` and `mutate_ties()` offer ways to add 
#'   a vector of values to a network as a tie attribute.
#'   - `rename_ties()` renames tie attributes.
#'   - `join_ties()` merges ties from two networks,
#'   adding a tie attribute identifying the newly added ties.
#'   
#'   Note that while `add_*()`/`delete_*()` functions operate similarly as comparable `{igraph}` functions,
#'   `mutate*()`, `bind*()`, etc work like `{tidyverse}` or `{dplyr}`-style functions.
#' @template param_data
#' @template param_dots
#' @template param_attr
#' @template param_vect
#' @template param_obj2
#' @family ties
#' @template fam_manip
#' @eval detail_avail("add_tie_attrib|(mutate|rename|arrange|join|select|summarise).*ties")
#' @examples
#'   other <- create_filled(4) |> mutate(name = c("A", "B", "C", "D"))
#'   mutate_ties(other, form = 1:6) |> filter_ties(form < 4)
#'   add_tie_attribute(other, "weight", c(1, 2, 2, 2, 1, 2))
NULL

#' @rdname manip_ties_attr
#' @importFrom igraph edge_attr delete_edge_attr set_edge_attr
#' @export
add_tie_attribute <- function(.data, attr_name, vector) UseMethod("add_tie_attribute")

#' @export
add_tie_attribute.default <- function(.data, attr_name, vector){
  as_input(.data, add_tie_attribute, attr_name = attr_name, vector = vector)
}

#' @export
add_tie_attribute.igraph <- function(.data, attr_name, vector){
  out <- .data
  igraph::edge_attr(out, name = attr_name) <- vector
  out
}

#' @export
add_tie_attribute.data.frame <- function(.data, attr_name, vector){
  is_edgelist(.data) || snet_abort("Not an edgelist")
  dplyr::mutate(.data, !!!stats::setNames(list(vector), attr_name))
}

#' @rdname manip_ties_attr
#' @importFrom tidygraph activate
#' @export
mutate_ties <- function(.data, ...) UseMethod("mutate_ties")

#' @export
mutate_ties.default <- function(.data, ...){
  as_input(.data, mutate_ties, ...)
}

#' @export
mutate_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> mutate(...) |> activate(nodes)
}

#' @export
mutate_ties.stocnet <- function(.data, ...){
  with_active_context(.data, "edges", {
    out <- .data
    out$ties <- out$ties |> 
      dplyr::mutate(...)
    out
  })
}

#' @rdname manip_ties_attr
#' @importFrom dplyr rename
#' @export
rename_ties <- function(.data, ...) UseMethod("rename_ties")

#' @export
rename_ties.default <- function(.data, ...){
  as_input(.data, rename_ties, ...)
}

#' @export
rename_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> dplyr::rename(...) |> activate(nodes)
}

#' @export
rename_ties.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    aka <- list(
      from   = c("sender", "ego", "source", "caller"),
      to     = c("recipient", "receiver", "alter", "target", "callee"),
      weight = c("value", "strength", "increment", "replace", "sign")
    )
    
    current_names <- names(out)
    rename_map <- c()
    
    for(expected in names(aka)){
      if(!expected %in% current_names){
        match <- intersect(aka[[expected]], current_names)
        if(length(match) > 0){
          rename_map[expected] <- match[1]  # take the first match if multiple
        }
      }
    }
    
    if(length(rename_map) > 0){
      snet_minor_info("Renaming tie attributes to stocnet conventions:",
                      "{paste(rename_map, '->', names(rename_map), collapse = ', ')}")
      out <- out |> dplyr::rename(dplyr::any_of(rename_map))
    }
  } else out <- out |> dplyr::rename(...)
  out
}

#' @export
rename_ties.stocnet <- function(.data, ...){
  out <- .data
  out$ties <- rename_ties.data.frame(out$ties, ...)
  out
}

#' @rdname manip_ties_attr
#' @importFrom dplyr arrange
#' @export
arrange_ties <- function(.data, ...) UseMethod("arrange_ties")

#' @export
arrange_ties.default <- function(.data, ...){
  as_input(.data, arrange_ties, ...)
}

#' @export
arrange_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> dplyr::arrange(...) |> activate(nodes)
}

#' @export
arrange_ties.stocnet <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    default_cols <- intersect(c("from", "to", "by", "time"), names(out$ties))
    out$ties <- out$ties |> dplyr::arrange(dplyr::across(dplyr::all_of(default_cols)))
  } else {
    out$ties <- out$ties |> dplyr::arrange(...)
  }
  out
}

#' @rdname manip_ties_attr 
#' @importFrom igraph add_edges set_edge_attr E
#' @importFrom dplyr mutate summarise across group_by everything ungroup
#' @export
join_ties <- function(.data, object2, attr_name) {
  el <- c(t(as.matrix(as_edgelist(object2))))
  obj <- as_tidygraph(.data) |> 
    tidygraph::activate(edges)
  if(ncol(as.data.frame(obj)) < 3){
    obj <- obj |> igraph::set_edge_attr("orig", value = 1)
  } 
  out <- igraph::add_edges(as_igraph(obj),
                           el, object2 = 1) |> 
    as_tidygraph()
  if(!missing(attr_name)){
    out <- igraph::set_edge_attr(out, attr_name,
                                 value = igraph::E(out)$object2) |>
      select_ties(-object2)
  }
  edges <- out |>
    tidygraph::activate(edges) |>
    as.data.frame() |> 
    dplyr::group_by(from, to) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), 
                                   function(x){
                                     out <- suppressWarnings(max(x, na.rm = TRUE))
                                     if(is.infinite(out)){
                                       if(is.numeric(out)) out <- 0 else 
                                         out <- NA
                                     }
                                     out
                                   }), 
                     .groups = "keep") |> dplyr::ungroup()
  nodes <- out |> tidygraph::activate(nodes) |> as.data.frame()
  tidygraph::tbl_graph(nodes, edges, 
                       directed = is_directed(.data))
}

#' @rdname manip_ties_attr
#' @importFrom dplyr select
#' @export
select_ties <- function(.data, ...) UseMethod("select_ties")

#' @export
select_ties.default <- function(.data, ...){
  as_input(.data, select_ties, ...)
}

#' @export
select_ties.tbl_graph <- function(.data, ...){
  out <- .data
  out |> tidygraph::activate(edges) |> dplyr::select(...) |> activate(nodes)
}

#' @export
select_ties.data.frame <- function(.data, ...){
  out <- .data
  if(...length() == 0){
    out <- dplyr::select(out, dplyr::any_of(c("from","to","by", "layer", "time","weight")),
                         dplyr::everything())
  } else out <- dplyr::select(out, ...)
  out
}

#' @export
select_ties.stocnet <- function(.data, ...){
  out <- .data
  out$ties <- select_ties.data.frame(out$ties, ...)
  out
}

#' @rdname manip_ties_attr
#' @importFrom dplyr summarise
#' @export
summarise_ties <- function(.data, ...) UseMethod("summarise_ties")

#' @export
summarise_ties.default <- function(.data, ...){
  as_input(.data, summarise_ties, ...)
}

#' @export
summarise_ties.tbl_graph <- function(.data, ...){
  out <- as_edgelist(.data) |> 
    dplyr::summarise(..., .by = c("from","to")) |> 
    as_tidygraph(twomode = is_twomode(.data))
  out <- as_tidygraph(bind_node_attributes(out, .data))
  if(!is_directed(.data)) out <- to_undirected(out)
  out
}

