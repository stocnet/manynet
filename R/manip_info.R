# Network information ####

#' Manipulating network information
#' @name manip_info
#' @description
#'   These functions allow users to add and edit information about the network
#'   itself.
#'   Where available, this information is printed for tidygraph-class objects,
#'   and can be used for printing a grand table in the `{grand}` package.
#'   
#'   - `add_info()` adds information attributes to the network.
#'   - `mutate_info()` updates information attributes of the network.
#'   - `net_attributes()` lists the information attributes of the network.
#' @template param_data
#' @family info
#' @eval detail_avail(".*_info")
#' @template fam_manip
#' @param ... Named attributes. The following are currently recognised:
#'   - "name" is the name of the network
#'   - "modes" is the name(s) of the nodeset(s)
#'   - "layers" is the name(s) of the tie type(s)
#'   - "directed" is a logical vector indicating whether each layer is directed
#'   - "source" is the source of the network ("empirical" or "synthetic")
#'   - "method" is the method of data collection or model used
#'   (e.g. "survey", "interview","sensor","observation","archival", or "simulation")
#'   - "location" is the geographic, institutional, or digital location of the network
#'   - "date" is the date of data collection or model run
#'   - "boundary" is the boundary specification of the network ("ego", "roster", or "snowball")
#'   - "observation" is the observation type of the network ("cross-sectional",
#'   "panel", or "event")
#'   - "update" is the update type of the network ("increment" or "replacement")
#'   - "max_degree" is the maximum degree of the network
#'   - "min_degree" is the minimum degree of the network
#'   - "doi" is the DOI or URL of the network
#'   
#'   If no arguments are used, 
#'   the function will check for missing information and prompt the user to add it.
#'   If `optional = TRUE` is specified, the function will also prompt for optional information.
#' @seealso \href{https://grand-statement.org}{GRAND statement} for more 
#'   information on the Guidelines for Reporting About Network Data (GRAND).
#' @examples
#' add_info(ison_algebra, name = "Algebra")
#' @export
add_info <- function(.data, ...) UseMethod("add_info")

#' @export
add_info.default <- function(.data, ...){
  as_input(.data, add_info, ...)
}

#' @export
add_info.igraph <- function(.data, ...){
  
  if(!is.null(igraph::graph_attr(.data)$grand)){ # Updating
    snet_success("Deleting information from previous version(s).")
    igraph::graph_attr(.data)$grand <- NULL
  }
  
  info <- list(...)
  optional <- info$optional %||% FALSE
  info$optional <- NULL
  if(length(info)==0) return(.check_info(.data, optional = optional))
  
  unrecog <- setdiff(names(info), c("name", "nodes", "ties", "doi", 
                                    "source", "method", "location", "date", "system",
                                    "degree",
                                    "dependent",
                                    "collection", "year", "mode", "vertex1", 
                                    "vertex1.total", "vertex2", 
                                    "vertex2.total", 
                                    "edge.pos", "edge.neg", "positive", "negative"))
  if(length(unrecog)>0) 
    snet_warn("{unrecog} are not recognised fields.")
  
  out <- .data
  if("name" %in% names(info)){
    igraph::graph_attr(out)$name <- info$name
  }
  if("nodes" %in% names(info)){
    if(is_twomode(.data) && length(info$nodes)!=2) 
      snet_abort("Please name both nodesets in a two-mode network.")
    igraph::graph_attr(out)$nodes <- info$nodes
  }
  if("ties" %in% names(info)){
    if(is_multiplex(.data) && length(info$ties) != length(unique(tie_attribute(.data, "type")))) 
      snet_abort("Please name all types of tie in a multiplex network.")
    igraph::graph_attr(out)$ties <- info$ties
  }
  if("collection" %in% names(info)){
    igraph::graph_attr(out)$collection <- info$collection
  }
  if("doi" %in% names(info)){
    igraph::graph_attr(out)$doi <- info$doi
  }
  if("year" %in% names(info)){
    igraph::graph_attr(out)$year <- info$year
  }
  # return(str(info)) # for debugging
  as_tidygraph(out)
}

#' @export
add_info.stocnet <- function(.data, ...){
  dots <- list(...)
  # if dots contains optional = FALSE/TRUE
  optional <- dots$optional %||% FALSE
  dots$optional <- NULL
  if(length(dots) == 0){
    return(.check_info(.data, optional = optional))
  }
  for(item in names(dots)){
    .data$info[[item]] <- dots[[item]]
  }
  .data
}

#' @rdname manip_info
#' @export
mutate_info <- function(.data, ...) UseMethod("mutate_info")

#' @export
mutate_info.default <- function(.data, ...){
  as_input(.data, mutate_info, ...)
}

#' @export
mutate_info.igraph <- function(.data, ...){
  info <- list(...)
  out <- as_tidygraph(.data)
  for(item in names(info)){
    igraph::graph_attr(out, item) <- info[[item]]
  }
  out
}

#' @export
mutate_info.stocnet <- function(.data, ...){
  dots <- list(...)
  out <- .data
  for(item in names(dots)){
    out$info[[item]] <- dots[[item]]
  }
  out
}

#' @rdname manip_info
#' @export
net_attributes <- function(.data){
  names(igraph::graph_attr(as_igraph(.data)))
}


.check_info <- function(.data, optional = FALSE){
  
  out <- .data
  read_optional <- function(prompt) {
    x <- readline(prompt)
    if (x == "") NULL else x
  }
  
  # Names
  if(is.null(net_name(out)) || net_name(out) == ""){
    snet_prompt("This network does not have a name. Please add one.")
    out <- add_info(out, name = read_optional(prompt = "Network name: "))
  }
  if(!is.null(net_name(out))) snet_success("Network name: {net_name(out)}")
  
  # Nodes ####
  if(is.null(mode_names(out))){
    if(is_twomode(out)){
    snet_prompt("This two-mode network does not have names for the nodesets. Please add one.")
    out$modes <- c(read_optional(prompt = "Nodeset 1 name: "),
                   read_optional(prompt = "Nodeset 2 name: "))
  } else {
    snet_prompt("This network does not have a name for the nodeset. Please add one.")
    out <- add_info(out, modes = read_optional(prompt = "Nodeset name: "))
  }}
  if(!is.null(mode_names(out))) snet_success("Modes: {mode_names(out)}")
  
  # Ties ####
  if(is.null(layer_names(out))){
    snet_prompt("This network does not have a name for the layer/type of tie. Please add one.")
    out <- add_info(out, layers = read_optional(prompt = "Layer name: "))
  }
  if(!is.null(layer_names(out))) snet_success("Layers: {layer_names(out)}")
  if(!"directed" %in% net_attributes(out)){
    if(net_layers(out) > 1){
      snet_prompt("This network has multiple layers. Please specify whether they are directed or undirected.")
      for (layer in layer_names(out)) {
        directed <- utils::menu(choices = c("Directed", "Undirected"), 
                                title = paste0("Is the layer '", layer, "' directed or undirected?"))
        out$info$directed[match(layer, layer_names(out))] <- stats::setNames(directed == 1, layer)
      }
    } else out <- add_info(out, directed = is_directed(.data))
  }
  if("directed" %in% net_attributes(out)) snet_success("Directed: {as_infolist(out)$directed}")
  
  # Optionals ####
  if(optional){
    if(!"source" %in% net_attributes(out)){
      snet_prompt("This network does not have a source. You may add one.")
      source_options <- c("Empirical", "Synthetic")
      source <- utils::menu(choices = source_options, title = "Is this network empirical or synthetic?")
      if(source == 1){
        method_options <- c("Survey", "Interview", "Sensor", "Archival", "Trace", "Ethnography")
        out <- add_info(out, source = source_options[source],
                        method = utils::menu(choices = method_options, title = "Method: "))
        out <- add_info(out, location = read_optional(prompt = "Location: "))
        out <- add_info(out, date = read_optional(prompt = "Date: "))
        bound_options <- c("Ego", "Roster", "Snowball")
        out <- add_info(out, boundary = utils::menu(choices = bound_options, title = "Boundary: "))
      } else if(source == 2){
        out <- add_info(out, source = source_options[source],
                        method = read_optional(prompt = "Model: "))
      }
      if(!"doi" %in% net_attributes(out)){
        out <- add_info(out, doi = read_optional(prompt = "DOI/URL: "))
      }
      if(!"max_degree" %in% net_attributes(out)){
        out <- add_info(out, max_degree = read_optional(prompt = "Maximum degree: "))
      }
      if(!"min_degree" %in% net_attributes(out)){
        out <- add_info(out, min_degree = read_optional(prompt = "Minimum degree: "))
      }
      if(!"observation" %in% net_attributes(out)){
        obs_options <- c("Cross-sectional", "Panel", "Event")
        for (layer in layer_names(out)) {
          observation <- utils::menu(choices = obs_options, 
                                  title = paste0("The layer '", layer, "' is observed as: "))
          out$info$observation[match(layer, layer_names(out))] <- stats::setNames(obs_options[observation], layer)
        }
      }
      if(is_weighted(out) && !"update" %in% net_attributes(out)){
        upd_options <- c("Increment", "Replacement")
        for (layer in layer_names(out)) {
          update <- utils::menu(choices = upd_options, 
                                     title = paste0("The layer '", layer, "' is updated by: "))
          out$info$update[match(layer, layer_names(out))] <- stats::setNames(upd_options[update], layer)
        }
      }
      if(is_multiplex(out) && !"focal" %in% net_attributes(out)){
        out$info$focal <- utils::menu(choices = layer_names(out), 
                                      title = "The focal ties are: ")
      }
    }
    if("source" %in% net_attributes(out)) snet_success("Source: {as_infolist(out)$source}")
    if("method" %in% net_attributes(out)) snet_success("Method/Model: {as_infolist(out)$method}")
    if("boundary" %in% net_attributes(out)) snet_success("Boundary: {as_infolist(out)$boundary}")
    if("location" %in% net_attributes(out)) snet_success("Location: {as_infolist(out)$location}")
    if("observation" %in% net_attributes(out)) snet_success("Observation: {as_infolist(out)$observation}")
    if("update" %in% net_attributes(out)) snet_success("Update: {as_infolist(out)$update}")
    if("max_degree" %in% net_attributes(out)) snet_success("Max degree: {as_infolist(out)$max_degree}")
    if("min_degree" %in% net_attributes(out)) snet_success("Min degree: {as_infolist(out)$min_degree}")
    if("date" %in% net_attributes(out)) snet_success("Date: {as_infolist(out)$date}")
    if("doi" %in% net_attributes(out)) snet_success("DOI/URL: {as_infolist(out)$doi}")
  }
  
  out
}

