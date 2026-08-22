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
#'   - "transformations" records how the network has been transformed since it
#'   was collected or generated. See the Transformations section.
#'   Note that this records what has been done to the network,
#'   where "method" records how the network was collected or modelled.
#'
#'   If no arguments are used,
#'   the function will check for missing information and prompt the user to add it.
#'   If `optional = TRUE` is specified, the function will also prompt for optional information.
#' @section Transformations:
#'   The "transformations" field implements section 4 of the GRAND guidelines,
#'   which names six ways raw data is turned into analytic data.
#'   It holds a named list, one element for each of them:
#'
#'   - "symmetrisation" (GRAND 4.1), which `to_undirected()` sets.
#'   - "dichotomisation" (4.2), which `to_unweighted()` sets.
#'   - "projection" (4.3), which `to_mode1()` and `to_mode2()` set.
#'   - "exclusion" (4.4), which the functions that drop nodes or ties set,
#'   such as `delete_isolates()`, `to_component()`, and `to_uniplex()`.
#'   - "aggregation" (4.5), which `to_flat()`, `join_ties()`,
#'   and `to_blockmodel()` set.
#'   - "imputation" (4.6), which `impute_ties()` and `impute_nodes()` set.
#'
#'   One further name, "normalisation", records what `to_normalised()` does.
#'   The guidelines do not name it, since rescaling tie values neither
#'   dichotomises them (4.2 ends in an unweighted network)
#'   nor aggregates them (4.5 combines what was separate),
#'   but it changes the analytic network and so is recorded too.
#'
#'   A name that is absent means that transformation was not applied,
#'   so `"symmetrisation" %in% names(as_infolist(.data)$transformations)`
#'   answers whether a network was symmetrised
#'   without reading past everything else done to it.
#'   `describe_transformations()` reports the same thing as a phrase.
#'
#'   Each element is a character vector naming the method first,
#'   and, where the guidelines ask for an amount too,
#'   its consequence in parentheses:
#'   `list(symmetrisation = "collapse", imputation = "reciprocity (73 missing ties)")`.
#'   An element accumulates rather than replaces,
#'   so a network imputed in more than one step reports each of them,
#'   and the order of the names is the order the transformations were applied.
#'
#'   The `to_*()` and `impute_*()` functions set this themselves,
#'   so it rarely needs to be set by hand.
#'   Where it does, `add_info()` takes a named list and merges it in,
#'   and refuses a name that is not one of those above.
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
                                    "edge.pos", "edge.neg", "positive", "negative",
                                    "transformations"))
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
    lattr <- .layer_attribute(.data)
    nlayers <- if(!is.na(lattr)) length(unique(tie_attribute(.data, lattr))) else 0L
    if(is_multiplex(.data) && length(info$ties) != nlayers) 
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
  if("transformations" %in% names(info)){
    # merges rather than replaces, so that a network transformed more than once
    # reports each step
    igraph::graph_attr(out)$transformations <-
      .merge_transformations(igraph::graph_attr(.data)$transformations,
                             info$transformations)
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
    # "transformations" merges rather than replaces, as in the igraph method,
    # so that successive transformations leave a trail
    if(item == "transformations"){
      .data$info[[item]] <- .merge_transformations(.data$info[[item]],
                                                   dots[[item]])
    } else .data$info[[item]] <- dots[[item]]
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

# GRAND section 4 names six ways raw data is turned into analytic data, and
# each is recorded under its own name so that a reader can ask whether a
# network was symmetrised without reading past everything else done to it.
# A name that is absent means that transformation was not applied.
grand_transformations <- c("symmetrisation", "dichotomisation", "projection",
                           "exclusion", "aggregation", "imputation")

# Rescaling tie values changes the analytic network too, but the guidelines do
# not name it: it is neither a dichotomisation, which ends in an unweighted
# network, nor an aggregation, which combines what was separate. So it is
# recorded under a name of its own, kept apart from the six so that the GRAND
# vector stays a faithful list of the guidelines' items.
manynet_transformations <- c(grand_transformations, "normalisation")

# Merges new entries into the transformations already recorded. An element
# accumulates rather than replaces, so a network transformed twice in the same
# way reports both, and a named list keeps its names in the order they were
# added, which is the order the transformations were applied.
.merge_transformations <- function(old, new){
  if(!is.list(new) || is.null(names(new)))
    snet_abort("{.arg transformations} must be a named list, one name for each transformation applied.")
  unrecognised <- setdiff(names(new), manynet_transformations)
  if(length(unrecognised))
    snet_abort(c(x = "{.val {unrecognised}} {?is/are} not {?a recognised transformation/recognised transformations}.",
                 i = "The six GRAND names are {.val {grand_transformations}}.",
                 i = "{.val {setdiff(manynet_transformations, grand_transformations)}} {?is/are} also recorded."))
  out <- old %||% list()
  for(item in names(new)) out[[item]] <- c(out[[item]], new[[item]])
  out
}

# Recording one transformation, for the `to_*()` and `impute_*()` functions to
# call. A matrix or an edgelist has nowhere to hold information about itself,
# so there is nothing to record on and it is returned as it is.
.record_transformation <- function(.data, item, entry){
  if(is.matrix(.data) || is.data.frame(.data) && !inherits(.data, "stocnet"))
    return(.data)
  add_info(.data, transformations = stats::setNames(list(entry), item))
}

# The counts these helpers take differences of come from `net_nodes()` and
# `net_ties()`, whose igraph methods return a network measure rather than a
# bare number, so each is unwrapped before the arithmetic.
.count_of <- function(.data, unit){
  count <- tryCatch(if(unit == "nodes") net_nodes(.data) else net_ties(.data),
                    error = function(e) NA_real_)
  as.numeric(count %||% NA_real_)
}

# GRAND item 4.4 asks for the exclusion criteria and the number of nodes or
# ties that were excluded, so the criterion leads and the count of what it
# removed follows. Nothing is recorded where nothing was excluded, since a
# function that had nothing to do did not transform the network.
.record_exclusion <- function(out, .data, criterion,
                              unit = c("nodes", "ties")){
  unit <- match.arg(unit)
  count <- .count_of(.data, unit) - .count_of(out, unit)
  if(is.na(count) || count <= 0) return(out)
  if(count == 1) unit <- sub("s$", "", unit)
  .record_transformation(out, "exclusion",
                         paste0(criterion, " (", count, " ", unit,
                                " excluded)"))
}

# Names a summarising function for reporting. The `default` methods pass the
# function itself through `as_input()`, which loses the symbol the user wrote,
# so it is recognised by what it is rather than by what it was called.
.fun_name <- function(FUN){
  known <- list(mean = mean, median = stats::median, sum = sum,
                min = min, max = max)
  hit <- vapply(known, function(f) identical(f, FUN), logical(1))
  if(any(hit)) names(known)[which(hit)[1]] else "custom"
}

# Copies a network's information onto another network, for the functions that
# build their result from a matrix and so start with none of it. The
# transformations merge rather than overwrite, as they do everywhere else.
.carry_info <- function(out, .data){
  info <- as_infolist(.data)
  transformations <- info$transformations
  info$transformations <- NULL
  if(length(info)) out <- do.call(mutate_info, c(list(out), info))
  if(length(transformations))
    out <- add_info(out, transformations = transformations)
  out
}

# The splitting functions return a list of networks, each of which leaves out
# what the others hold, so every element records its own exclusion against the
# network they were all split from.
.record_exclusions <- function(out, .data, criteria, unit = c("nodes", "ties")){
  unit <- match.arg(unit)
  out[] <- Map(function(x, criterion)
    .record_exclusion(x, .data, criterion, unit), out, criteria)
  out
}

# GRAND item 4.2 asks for the dichotomising method and the number of ties that
# were deleted by it. The guidelines say "deleted" here and "excluded" in item
# 4.4, and the two words are kept apart so that a reader can tell which item an
# entry answers. This records even where no tie fell below the threshold, since
# the dichotomisation happened either way.
.record_dichotomisation <- function(out, .data, threshold){
  count <- .count_of(.data, "ties") - .count_of(out, "ties")
  if(is.na(count)) return(out)
  ties <- if(count == 1) "tie" else "ties"
  .record_transformation(out, "dichotomisation",
                         paste0("threshold ", threshold, " (", count, " ",
                                ties, " deleted)"))
}

# GRAND item 4.1 asks for the percent of connected dyads that had
# non-reciprocal ties before the network was symmetrised, which is the
# asymmetric dyads over the dyads that were connected at all. Returns NULL
# where the question does not arise, so that the caller reports the rule alone.
.non_reciprocal_percent <- function(.data){
  if(!is_directed(.data)) return(NULL)
  census <- .net_by_dyad(as_igraph(.data))
  connected <- unname(census["Mutual"] + census["Asymmetric"])
  if(is.na(connected) || connected == 0) return(NULL)
  round(unname(census["Asymmetric"]) / connected * 100)
}

#' @rdname manip_info
#' @export
net_attributes <- function(.data){
  names(igraph::graph_attr(as_igraph(.data)))
}

# nocov start
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
# nocov end

# A network that loses nodes can lose every tie of a layer with them, and one
# that is reduced to a single layer leaves the other layers behind. The
# per-layer information then still describes layers that the network no longer
# holds, which `validate_stocnet()` rejects, so it is cut back to the layers
# that remain. `focal` names layers and variables together, so only the layers
# it names are taken out of it.
.prune_layer_info <- function(info, layers){
  if(is.null(info)) return(info)
  dropped <- setdiff(info$layers, layers)
  if(length(dropped) == 0) return(info)
  info$layers <- if(length(layers) > 0) layers else NULL
  for(field in c("directed", "observation", "update")){
    vals <- info[[field]]
    if(is.null(vals) || is.null(names(vals))) next
    keep <- intersect(names(vals), layers)
    # A vector that names none of the layers is not keyed by layer, so it is
    # left as it is rather than emptied.
    if(length(keep) == 0 && length(layers) > 0) next
    info[[field]] <- if(length(keep) > 0) vals[keep] else NULL
  }
  if(!is.null(info$focal)){
    focal <- setdiff(info$focal, dropped)
    info$focal <- if(length(focal) > 0) focal else NULL
  }
  info
}
