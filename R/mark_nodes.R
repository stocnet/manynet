# Diffusion properties ####

#' Marking nodes based on diffusion properties
#' 
#' @description 
#'   These functions return logical vectors the length of the 
#'   nodes in a network identifying which hold certain properties or positions in the network.
#'   
#'   - `node_is_infected()` marks nodes that are infected by a particular time point. 
#'   - `node_is_exposed()` marks nodes that are exposed to a given (other) mark.
#'   - `node_is_latent()` marks nodes that are latent at a particular time point.
#'   - `node_is_recovered()` marks nodes that are recovered at a particular time point.
#' @inheritParams mark_is
#' @family marks
#' @name mark_diff
NULL

#' @rdname mark_diff 
#' @examples
#'   # To mark nodes that are latent by a particular time point
#'   node_is_latent(play_diffusion(create_tree(6), latency = 1), time = 1)
#' @export
node_is_latent <- function(.data, time = 0){
  if(is_changing(.data)){
    t <- time
    latent <- as_changelist(.data) %>% 
      dplyr::filter(time <= t & value %in% c("E", "I")) %>%
      dplyr::group_by(node) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::filter(n == 1 & value == "E")
    if (is_labelled(.data)) {
      out <- seq_len(net_nodes(.data)) %in% latent$node
      names(out) <- node_names(.data)
    } else {
      out <- seq_len(net_nodes(.data)) %in% latent$node
    }
    make_node_mark(out, .data)
  } else if(inherits(.data, "diff_model")){
    latent <- summary(.data) %>%
      dplyr::filter(t <= time & event %in% c("E", "I")) %>%
      group_by(nodes) %>%
      mutate(n = dplyr::n()) %>%
      filter(n == 1 & event == "E")
    net <- attr(.data, "network")
    if (is_labelled(net)) {
      out <- seq_len(net_nodes(net)) %in% latent$nodes
      names(out) <- node_names(net)
    } else {
      out <- seq_len(net_nodes(net)) %in% latent$nodes
    }
    make_node_mark(out, net)
  } else {
    out <- to_time(.data, time)
    out <- node_attribute(out, "diffusion") == "E"
    make_node_mark(out, .data)
  }
}

#' @rdname mark_diff 
#' @param time A time step at which nodes are identified.
#' @examples
#'   # To mark nodes that are infected by a particular time point
#'   node_is_infected(play_diffusion(create_tree(6)), time = 1)
#' @export
node_is_infected <- function(.data, time = 0) {
  if(is_changing(.data)){
    t <- time
    infected <- as_changelist(.data) %>% 
      dplyr::filter(time <= t & value %in% c("I", "R")) %>%
      dplyr::group_by(node) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::filter(n == 1 & value == "I")
    if (is_labelled(.data)) {
      out <- seq_len(net_nodes(.data)) %in% infected$node
      names(out) <- node_names(.data)
    } else {
      out <- seq_len(net_nodes(.data)) %in% infected$node
    }
    make_node_mark(out, .data)
  } else if(inherits(.data, "diff_model")){
    infected <- summary(.data) %>% 
      dplyr::filter(t <= time & event %in% c("I", "R")) %>%
      group_by(nodes) %>%
      mutate(n = dplyr::n()) %>%
      filter(n == 1 & event == "I")
    net <- attr(.data, "network")
    if (is_labelled(net)) {
      out <- seq_len(net_nodes(net)) %in% infected$nodes
      names(out) <- node_names(net)
    } else {
      out <- seq_len(net_nodes(net)) %in% infected$nodes
    }
    make_node_mark(out, net)
  } else {
    out <- to_time(.data, time)
    out <- node_attribute(out, "diffusion") == "I"
    make_node_mark(out, .data)
  }
}

#' @rdname mark_diff 
#' @examples
#'   # To mark nodes that are recovered by a particular time point
#'   node_is_recovered(play_diffusion(create_tree(6), recovery = 0.5), time = 3)
#' @export
node_is_recovered <- function(.data, time = 0){
  if(is_changing(.data)){
    t <- time
    recovered <- as_changelist(.data) %>% 
      dplyr::filter(time <= t & value %in% c("R")) %>%
      dplyr::group_by(node) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::filter(n == 1 & value == "R")
    if (is_labelled(.data)) {
      out <- seq_len(net_nodes(.data)) %in% recovered$node
      names(out) <- node_names(.data)
    } else {
      out <- seq_len(net_nodes(.data)) %in% recovered$node
    }
    make_node_mark(out, .data)
  } else if(inherits(.data, "diff_model")){
    recovered <- summary(.data) %>% 
      dplyr::filter(t <= time & event == "R") %>%
      group_by(nodes) %>%
      mutate(n = dplyr::n()) %>%
      filter(n == 1)
    net <- attr(.data, "network")
    if (is_labelled(net)) {
      out <- seq_len(net_nodes(net)) %in% recovered$nodes
      names(out) <- node_names(net)
    } else {
      out <- seq_len(net_nodes(net)) %in% recovered$nodes
    }
    make_node_mark(out, net)
  } else {
    out <- to_time(.data, time)
    out <- node_attribute(out, "diffusion") == "R"
    make_node_mark(out, .data)
  }
}

#' @rdname mark_diff 
#' @param mark A valid 'node_mark' object or
#'   logical vector (TRUE/FALSE) of length equal to 
#'   the number of nodes in the network.
#' @section Exposed:
#'   `node_is_exposed()` is similar to `node_exposure()`,
#'   but returns a mark (TRUE/FALSE) vector indicating which nodes
#'   are currently exposed to the diffusion content.
#'   This diffusion content can be expressed in the 'mark' argument.
#'   If no 'mark' argument is provided,
#'   and '.data' is a diff_model object,
#'   then the function will return nodes exposure to the seed nodes
#'   in that diffusion.
#' @param mark vector denoting which nodes are infected
#' @examples
#'   # To mark which nodes are currently exposed
#'   (expos <- node_is_exposed(manynet::create_tree(14), mark = c(1,3)))
#'   which(expos)
#' @export
node_is_exposed <- function(.data, mark, time = 0){
  if (missing(mark)){
    if(is_changing(.data)){
      t <- time
      return(make_node_mark(node_exposure(.data, time = t)>0, .data))
    } else if(inherits(.data, "diff_model")){
      mark <- summary(.data) %>% 
        dplyr::filter(t == 0 & event == "I") %>% 
        dplyr::select(nodes) %>% unlist()
      .data <- attr(.data, "network")
    }    
  }
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(F, manynet::net_nodes(.data))
  out[unique(setdiff(unlist(igraph::neighborhood(.data, nodes = mark)),
                     mark))] <- TRUE
  make_node_mark(out, .data)
}

