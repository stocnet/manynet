#' Joining lists of networks, graphs, and matrices
#' @name modif_from
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
#' @param netlist A list of network, igraph, tidygraph, matrix, or edgelist objects.
#' @template fam_modif
NULL

#' @rdname modif_from
#' @importFrom igraph graph_from_data_frame as_data_frame set_vertex_attr
#' @examples
#' ison_adolescents |>
#'   mutate(unicorn = sample(c("yes", "no"), 8, replace = TRUE)) |>
#'   to_subgraphs(attribute = "unicorn") |>
#'   from_subgraphs()
#' @export
from_subgraphs <- function(netlist) {
  if (!is.list(netlist[1])) {
    snet_abort("Please declare a list of subgraphs. ")
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

#' @rdname modif_from
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents |>
#'   to_egos() |>
#'   from_egos()
#' @export
from_egos <- function(netlist) {
  if (!is.list(netlist[1])) {
    snet_abort("Please declare a list of egos.")
  }
  ann <- lapply(netlist, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  as_tidygraph(igraph::graph_from_data_frame(dplyr::distinct(out)))
}

#' @rdname modif_from 
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @examples
#' ison_adolescents |>
#'   mutate_ties(wave = sample(1:4, 10, replace = TRUE)) |>
#'   to_waves(attribute = "wave") |>
#'   from_waves()
#' @export
from_waves <- function(netlist) {
  if (!is.list(netlist[1])) {
    snet_abort("Please declare a list of waves.")
  }
  ann <- lapply(netlist, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  as_tidygraph(igraph::graph_from_data_frame(out))
}

#' @rdname modif_from 
#' @param remove.duplicates Should duplicates be removed?
#' By default FALSE.
#' If TRUE, duplicated edges are removed.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents |>
#'   mutate_ties(time = 1:10, increment = 1) |> 
#'   add_ties(c(1,2), list(time = 3, increment = -1)) |> 
#'   to_slices(slice = c(5,7)) |>
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

#' @rdname modif_from
#' @param ... Two or more tidygraph or stocnet objects to be merged, 
#'   each representing a different set of ties to be combined into a single
#'   multiplex network.
#' @param layer_names A character vector of names for the different network objects,
#'   if not already named within the list.
#' @export
from_ties <- function(..., layer_names) UseMethod("from_ties")

#' @export
from_ties.tbl_graph <- function(..., layer_names){
  netlist <- list(...)
  stopifnot(length(netlist) >= 2)
  if(is.null(names(netlist))){
    if(!missing(layer_names)){
      names(netlist) <- layer_names
    } else snet_abort("Please name the layers of the networks to be merged,",
                            "either by naming the list elements or",
                            "by providing a vector of names to 'layer_names'.")
    
  }
  netlist <- lapply(seq_along(netlist), 
                    function(x) if(is_multiplex(netlist[[x]])){
                      netlist[[x]] } else { 
                        mutate_ties(netlist[[x]], type = names(netlist)[x])
                        })
  suppressMessages(Reduce(tidygraph::graph_join, netlist))
}

#' @export
from_ties.stocnet <- function(..., layer_names){
  
  netlist <- list(...)
  stopifnot(length(netlist) >= 2)
  
  ## 1. Name the list elements (used as fallback layer names) -------------
  if(is.null(names(netlist)) || any(names(netlist) == "")){
    if(!missing(layer_names)){
      names(netlist) <- layer_names
    } else {
      snet_abort("Please name the layers of the networks to be merged,",
                 "either by naming the list elements or",
                 "by providing a vector of names to 'layer_names'.")
    }
  }
  layer_names <- names(netlist)
  
  ## 2. Resolve layer names per network, only renaming on clashes ----------
  seen_layers <- character(0)
  for(i in seq_along(netlist)){
    net <- netlist[[i]]
    layers_i <- .get_layers(net, layer_names[i])
    clash <- intersect(layers_i, seen_layers)
    if(length(clash)){
      rn <- stats::setNames(paste0(layer_names[i], ".", clash), clash)
      net <- .rename_layers(net, rn)
      layers_i[layers_i %in% clash] <- unname(rn[layers_i[layers_i %in% clash]])
      snet_warn(paste0("Layer name(s) ", phrase(clash),
                       " already used by another network; renamed to ",
                       phrase(unname(rn)), " for '", layer_names[i], "'."))
    }
    netlist[[i]] <- .ensure_layers(net, layers_i, layer_names[i])
    seen_layers <- c(seen_layers, layers_i)
  }
  
  ## 3. Merge labelled node tables ------------------------------------------
  labelled <- vapply(netlist, function(x) is_labelled(x), logical(1))
  
  merged_nodes <- .merge_node_tables(lapply(netlist[labelled], function(x) x$nodes))
  
  ## 4. Reindex labelled networks onto merged_nodes -------------------------
  netlist[labelled] <- lapply(netlist[labelled], function(net){
    new_idx <- match(net$nodes$label, merged_nodes$label)
    .apply_reindex(net, new_idx)
  })
  
  ## 5. Match/merge unlabelled networks, by block size (and mode) ----------
  for(i in which(!labelled)){
    net <- netlist[[i]]
    blocks <- .get_blocks(net)
    res <- .match_anon_blocks(blocks, merged_nodes)
    merged_nodes <- res$merged_nodes
    netlist[[i]] <- .apply_reindex(net, res$new_idx)
  }
  
  ## 6. Combine ties, changes, global ---------------------------------------
  bind_or_null <- function(lst){
    lst <- lst[!vapply(lst, is.null, logical(1))]
    if(length(lst) == 0) return(NULL)
    out <- dplyr::bind_rows(lst)
    if(nrow(out) == 0) NULL else out
  }
  merged_ties    <- bind_or_null(lapply(netlist, function(x) x$ties))
  merged_changes <- bind_or_null(lapply(netlist, function(x) x$changes))
  merged_global  <- bind_or_null(lapply(netlist, function(x) x$global))
  
  ## 7. Combine info ----------------------------------------------------------
  merged_info <- .merge_info(netlist, merged_nodes)
  if (!is.null(merged_nodes) &&
      "label" %in% names(merged_nodes) &&
      all(is.na(merged_nodes$label))) {
    merged_nodes$label <- NULL
  }
  
  ## 8. Assemble ----------------------------------------------------------------
  out <- make_stocnet(info = merged_info, nodes = merged_nodes,
               ties = merged_ties, changes = merged_changes,
               global = merged_global)
  out
}

## --- Helpers ----------------------------------------------------------------

# Work out a network's layer name(s), preferring info$layers and any
# 'layer' column on ties; falling back to the list-element name.
.get_layers <- function(net, fallback_name){
  layers <- net$info$layers
  if(!is.null(net$ties) && "layer" %in% names(net$ties)){
    tl <- unique(net$ties$layer)
    layers <- union(layers, tl[!is.na(tl)])
  }
  if(is.null(layers) || length(layers) == 0) layers <- fallback_name
  layers
}

# Make sure info$layers and ties$layer are consistent with `layers`,
# and that 'directed'/'observation'/'update' are named by layer.
.ensure_layers <- function(net, layers, fallback_name){
  info <- net$info
  if(is.null(info)) info <- list()
  info$layers <- layers
  
  if(!is.null(net$ties)){
    if(!"layer" %in% names(net$ties)){
      net$ties$layer <- if(length(layers) == 1) layers else NA_character_
      if(length(layers) > 1)
        snet_warn(paste0("Network '", fallback_name, "' declares multiple ",
                         "layers but its ties have no 'layer' column."))
    } else if(any(is.na(net$ties$layer)) && length(layers) == 1){
      net$ties$layer[is.na(net$ties$layer)] <- layers
    }
  }
  
  if(length(layers) == 1){
    for(field in c("directed", "observation", "update")){
      v <- info[[field]]
      if(!is.null(v) && is.null(names(v))) info[[field]] <- stats::setNames(v, layers)
    }
  }
  
  net$info <- info
  net
}

# Rename layers (in ties$layer, info$layers, named info fields, info$focal,
# and nested per-layer info blocks) per a named map old -> new.
.rename_layers <- function(net, rename_map){
  if(!is.null(net$ties) && "layer" %in% names(net$ties)){
    idx <- net$ties$layer %in% names(rename_map)
    net$ties$layer[idx] <- unname(rename_map[net$ties$layer[idx]])
  }
  info <- net$info
  if(!is.null(info)){
    if(!is.null(info$layers)){
      idx <- info$layers %in% names(rename_map)
      info$layers[idx] <- unname(rename_map[info$layers[idx]])
    }
    for(field in c("directed", "observation", "update")){
      v <- info[[field]]
      if(!is.null(v) && !is.null(names(v))){
        nm <- names(v)
        idx <- nm %in% names(rename_map)
        nm[idx] <- unname(rename_map[nm[idx]])
        names(info[[field]]) <- nm
      }
    }
    if(!is.null(info$focal) && info$focal %in% names(rename_map))
      info$focal <- unname(rename_map[[info$focal]])
    for(old in names(rename_map)){
      new <- rename_map[[old]]
      if(old %in% names(info) && new != old){
        info[[new]] <- info[[old]]
        info[[old]] <- NULL
      }
    }
  }
  net$info <- info
  net
}

# Full-join node tables on 'label', coalescing overlapping attribute columns
# (including 'mode') and warning on genuine conflicts.
.merge_node_tables <- function(node_tbls){
  node_tbls <- node_tbls[!vapply(node_tbls, is.null, logical(1))]
  if(length(node_tbls) == 0) return(NULL)
  
  merged <- node_tbls[[1]]
  for(tb in node_tbls[-1]){
    common <- intersect(setdiff(names(merged), "label"),
                        setdiff(names(tb), "label"))
    merged <- dplyr::full_join(merged, tb, by = "label", suffix = c("", ".y"))
    for(col in common){
      ycol <- paste0(col, ".y")
      if(ycol %in% names(merged)){
        conflict <- !is.na(merged[[col]]) & !is.na(merged[[ycol]]) &
          merged[[col]] != merged[[ycol]]
        if(any(conflict))
          snet_warn(paste0("Conflicting values for node attribute '", col,
                           "' across networks; keeping the first network's ",
                           "values where they differ."))
        merged[[col]] <- dplyr::coalesce(merged[[col]], merged[[ycol]])
        merged[[ycol]] <- NULL
      }
    }
  }
  merged
}

# Apply a from-old-to-new node index map to ties$from/to and changes$node
.apply_reindex <- function(net, new_idx){
  if(!is.null(net$ties)){
    net$ties$from <- new_idx[net$ties$from]
    net$ties$to   <- new_idx[net$ties$to]
  }
  if(!is.null(net$changes)){
    net$changes$node <- new_idx[net$changes$node]
  }
  net
}

# Describe an unlabelled network's nodeset as one or more "blocks"
# (by mode, if a mode column exists), each with a size and a mode tag.
.get_blocks <- function(net){
  nodes <- net$nodes
  if(is.null(nodes)){
    n <- suppressWarnings(max(c(net$ties$from, net$ties$to,
                                net$changes$node), 0, na.rm = TRUE))
    return(list(list(mode = NA_character_, size = n)))
  }
  if("mode" %in% names(nodes)){
    keys <- ifelse(is.na(nodes$mode), "\u0001NA\u0001", nodes$mode)
    lapply(unique(keys), function(k){
      list(mode = if(k == "\u0001NA\u0001") NA_character_ else k,
           size = sum(keys == k))
    })
  } else {
    list(list(mode = NA_character_, size = nrow(nodes)))
  }
}

# Match each unlabelled block to an existing label==NA pool in merged_nodes,
# primarily by size; 'mode' is used only to disambiguate when there are
# multiple same-sized candidates, and to fill in unknown mode info.
.match_anon_blocks <- function(blocks, merged_nodes){
  sizes <- vapply(blocks, function(b) b$size, numeric(1))
  offsets <- cumsum(c(0, sizes))
  new_idx <- integer(sum(sizes))
  
  for(i in seq_along(blocks)){
    b <- blocks[[i]]
    rng <- (offsets[i] + 1):offsets[i + 1]
    if(b$size == 0) next
    
    candidates <- list()
    if(!is.null(merged_nodes)){
      na_label <- is.na(merged_nodes$label)
      mode_vals <- if("mode" %in% names(merged_nodes)) merged_nodes$mode
      else rep(NA_character_, nrow(merged_nodes))
      pool_idx <- which(na_label)
      if(length(pool_idx)){
        keys <- ifelse(is.na(mode_vals[pool_idx]), "\u0001NA\u0001", mode_vals[pool_idx])
        for(k in unique(keys)){
          idx <- pool_idx[keys == k]
          pm <- if(k == "\u0001NA\u0001") NA_character_ else k
          # match on size alone; mode is not a hard requirement
          if(length(idx) == b$size)
            candidates[[length(candidates) + 1]] <- list(idx = idx, mode = pm)
        }
      }
    }
    
    if(length(candidates) > 1 && !is.na(b$mode)){
      # if several same-sized pools exist, prefer one whose mode matches
      # (or is unknown) over one with a known, differing mode
      compatible <- vapply(candidates, function(cand)
        is.na(cand$mode) || cand$mode == b$mode, logical(1))
      if(any(compatible)) candidates <- candidates[compatible]
    }
    
    if(length(candidates) == 1){
      claim <- candidates[[1]]
      new_idx[rng] <- claim$idx
      if(is.na(claim$mode) && !is.na(b$mode)){
        if(!("mode" %in% names(merged_nodes))) merged_nodes$mode <- NA_character_
        merged_nodes$mode[claim$idx] <- b$mode
      }
    } else {
      if(length(candidates) > 1)
        snet_warn(paste0("Multiple possible matches found for an unlabelled ",
                         "block of ", b$size, " nodes; adding as new nodes ",
                         "rather than guessing."))
      add <- dplyr::tibble(label = rep(NA_character_, b$size))
      if(!is.na(b$mode)) add$mode <- b$mode
      merged_nodes <- if(is.null(merged_nodes)) add else 
        dplyr::bind_rows(merged_nodes, add)
      new_idx[rng] <- (nrow(merged_nodes) - b$size + 1):nrow(merged_nodes)
    }
  }
  list(new_idx = new_idx, merged_nodes = merged_nodes)
}

# Build the merged info list
.merge_info <- function(netlist, merged_nodes){
  all_layers <- unique(unlist(lapply(netlist, function(x) x$info$layers),
                              use.names = FALSE))
  
  all_modes <- character(0)
  if(!is.null(merged_nodes) && "mode" %in% names(merged_nodes))
    all_modes <- unique(stats::na.omit(merged_nodes$mode))
  for(net in netlist) all_modes <- union(all_modes, net$info$modes)
  
  directed_vec    <- stats::setNames(rep(NA, length(all_layers)), all_layers)
  observation_vec <- stats::setNames(rep(NA_character_, length(all_layers)), all_layers)
  update_vec      <- stats::setNames(rep(NA_character_, length(all_layers)), all_layers)
  
  for(net in netlist){
    for(ly in net$info$layers){
      d <- net$info$directed
      if(!is.null(d)){
        val <- if(!is.null(names(d)) && ly %in% names(d)) d[[ly]]
        else if(length(d) == 1) d[[1]] else NA
        if(!is.na(val)) directed_vec[ly] <- val
      }
      obs <- if(!is.null(net$info[[ly]]$observation)) net$info[[ly]]$observation
      else net$info$observation
      if(!is.null(obs)){
        if(!is.null(names(obs)) && ly %in% names(obs)) observation_vec[ly] <- obs[[ly]]
        else if(length(obs) == 1) observation_vec[ly] <- obs
      }
      upd <- if(!is.null(net$info[[ly]]$update)) net$info[[ly]]$update
      else net$info$update
      if(!is.null(upd)){
        if(!is.null(names(upd)) && ly %in% names(upd)) update_vec[ly] <- upd[[ly]]
        else if(length(upd) == 1) update_vec[ly] <- upd
      }
    }
  }
  if(all(is.na(directed_vec)))    directed_vec    <- NULL
  if(all(is.na(observation_vec))) observation_vec <- NULL
  if(all(is.na(update_vec)))      update_vec      <- NULL
  
  focals <- unlist(lapply(netlist, function(x) x$info$focal))
  focal <- if(length(focals) >= 1) focals[1] else NULL
  if(length(focals) > 1)
    snet_warn(paste0("Multiple networks declared a 'focal' layer; keeping ",
                     "the first ('", focal, "')."))
  
  merged_info <- list(
    name     = paste(all_layers, collapse = " + "),
    modes    = if(length(all_modes)) all_modes else NULL,
    layers   = all_layers,
    directed = directed_vec,
    observation = observation_vec,
    update   = update_vec,
    focal    = focal
  )
  
  # date / doi: keep if consistent; if conflicting, warn and take the earlier
  for(key in c("date", "doi")){
    vals <- lapply(netlist, function(x) x$info[[key]])
    vals <- vals[!vapply(vals, is.null, logical(1))]
    if(length(vals) == 1){
      merged_info[[key]] <- vals[[1]]
    } else if(length(vals) > 1){
      uvals <- unlist(vals)
      if(length(unique(uvals)) > 1)
        snet_warn(paste0("Networks specify different '", key,
                         "' values; using the earliest."))
      merged_info[[key]] <- if(key == "date") min(uvals) else uvals[which.min(uvals)]
    }
  }
  
  # location / source: carry forward only if a single source specifies it
  for(key in c("location", "source")){
    vals <- lapply(netlist, function(x) x$info[[key]])
    vals <- vals[!vapply(vals, is.null, logical(1))]
    if(length(vals) == 1) merged_info[[key]] <- vals[[1]]
  }
  
  # per-layer metadata blocks (sender, receiver, method, etc.)
  layer_keys <- c("sender", "receiver", "update", "observation", "source",
                  "method", "date", "location", "doi")
  for(net in netlist){
    for(ly in net$info$layers){
      nested <- net$info[[ly]]
      if(is.null(nested)) nested <- list()
      toplevel <- net$info[intersect(names(net$info), layer_keys)]
      block <- c(nested, toplevel[setdiff(names(toplevel), names(nested))])
      block$observation <- NULL
      block$update <- NULL
      if(length(block) > 0) merged_info[[ly]] <- block
    }
  }
  
  merged_info[!vapply(merged_info, is.null, logical(1))]
}
