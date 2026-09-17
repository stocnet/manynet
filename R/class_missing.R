# Missingness ####
#
# A missing tie is one that could have been observed and was not.
# Almost always this is a node that did not report, so a stocnet records the
# nodes that did not report rather than one record per tie: `nodes$na` where a
# node reports at no point, and a change of the 'na' variable where it varies
# over time. `missings` holds whatever those records do not imply.
#
# `.expand_missing()` derives the ties from those records, which is what
# `as_missinglist()` returns, and `.compress_missing()` derives the records
# from the ties, which is what coercion into a stocnet does.

# The moments a network records, or NA where it records none.
.stocnet_times <- function(.data){
  times <- c(.data$ties[["time"]], .data$changes[["time"]],
             .data$missings[["time"]])
  if(is.null(times)) NA else sort(unique(times))
}

# The layer and moment pairs a network records, one row each. A network without
# layers or without moments holds NA in that column, so that the pairs can be
# matched against the ties either way.
.stocnet_occasions <- function(.data){
  ties <- .data$ties
  layers <- if(!is.null(ties[["layer"]])) unique(ties$layer) else NA_character_
  if(is.null(ties[["time"]]))
    return(dplyr::tibble(layer = layers, time = NA))
  out <- unique(dplyr::tibble(layer = if(is.null(ties[["layer"]]))
    NA_character_ else as.character(ties$layer), time = ties$time))
  out[order(out$time), , drop = FALSE]
}

# A nodal state over the moments a network records, as a logical matrix of one
# row per node and one column per moment. The changelog states what a node's
# value becomes from that moment on, so each change is carried forward.
.node_state <- function(.data, var, times, default){
  n <- net_nodes(.data)
  init <- if(var %in% names(.data$nodes)) as.logical(.data$nodes[[var]]) else
    rep(default, n)
  init[is.na(init)] <- default
  out <- matrix(rep(init, length(times)), nrow = n)
  chg <- .data$changes
  if(!is.null(chg) && nrow(chg) && var %in% as.character(chg$var)){
    sub <- chg[as.character(chg$var) == var, , drop = FALSE]
    sub <- sub[order(as.numeric(sub$time)), , drop = FALSE]
    for(r in seq_len(nrow(sub))){
      at <- match(sub$time[[r]], times)
      node <- sub$node[[r]]
      value <- unlist(sub$value[[r]])
      if(is.na(node)) next
      if(length(value) >= 2){
        # `as_stocnet.sienadata()` logs an activity change as the intervals a
        # node is present over, rather than as one value per moment.
        out[node, ] <- .within_intervals(times, value)
      } else if(!is.na(at)){
        out[node, at:ncol(out)] <- as.logical(value)[1]
      }
    }
  }
  out
}

# Whether each moment falls within one of the (enter, leave) pairs a node is
# present over, which is how a composition change reaches here from RSiena.
.within_intervals <- function(times, value){
  pairs <- matrix(as.numeric(value), nrow = 2)
  vapply(as.numeric(times), function(t)
    any(t >= pairs[1, ] & t <= pairs[2, ]), logical(1))
}

# Which nodes a node could hold a tie with at a given moment: the other nodes
# still in the network, of the other mode where the network is two-mode.
.stocnet_alters <- function(.data, node, active){
  alters <- which(active)
  if(is_twomode(.data)){
    modes <- .data$nodes$mode
    alters <- alters[modes[alters] != modes[node]]
  } else alters <- setdiff(alters, node)
  alters
}

# The ties a network records as missing, derived from its nonresponse records.
.expand_missing <- function(.data){
  empty <- dplyr::tibble(from = integer(0), to = integer(0),
                         layer = character(0), time = numeric(0))
  if(is.null(.data$nodes) && is.null(.data$ties)) return(empty)
  times <- .stocnet_times(.data)
  na_state <- .node_state(.data, "na", times, default = FALSE)
  act_state <- .node_state(.data, "active", times, default = TRUE)
  occasions <- .stocnet_occasions(.data)
  egocentric <- is_egocentric(.data)
  rows <- lapply(seq_len(nrow(occasions)), function(o){
    layer <- occasions$layer[[o]]; time <- occasions$time[[o]]
    at <- if(is.na(time)) 1L else match(time, times)
    # A node that is not in the network at all misses nothing, since there was
    # nothing there to miss. That is composition change and not missingness.
    absent <- which(na_state[, at] & act_state[, at])
    absent <- .layer_absent(.data, absent, layer, time)
    if(!length(absent)) return(NULL)
    directed <- layer_is_directed(.data, layer)
    reported <- !egocentric && .occasion_reported(.data, layer, time)
    pairs <- lapply(absent, function(node){
      # A node that did not report missed the whole network it was asked
      # about, and not only the ties it would itself have sent.
      if(reported)
        return(.unreported_report(.data, node, act_state[, at], layer, time,
                                  directed))
      alters <- .stocnet_alters(.data, node, act_state[, at])
      if(!length(alters)) return(NULL)
      dplyr::tibble(from = as.integer(node), to = as.integer(alters),
                    layer = layer, time = time)
    })
    out <- dplyr::bind_rows(pairs)
    # An undirected layer holds one row per dyad, so its missing ties do too.
    if(!directed && nrow(out)) out <- out[!duplicated(.undirected_key(out)), ,
                                          drop = FALSE]
    out
  })
  out <- dplyr::bind_rows(rows)
  out <- dplyr::bind_rows(out, .missing_registry(.data))
  if(!nrow(out)) return(empty)
  out <- out[!duplicated(out[intersect(c("from", "to", "layer", "time", "by", "about"),
                                       names(out))]), , drop = FALSE]
  # An observed tie is not a missing one, whatever the records imply.
  out[!.tie_key(out) %in% .tie_key(.data$ties), , drop = FALSE]
}

# A node may have reported on some layers and not others, which is recorded by
# a 'layer' column on the change. A change without one covers every layer.
.layer_absent <- function(.data, absent, layer, time){
  chg <- .data$changes
  if(!length(absent) || is.null(chg) || is.null(chg[["layer"]]) || is.na(layer))
    return(absent)
  sub <- chg[as.character(chg$var) == "na" & !is.na(chg$layer), , drop = FALSE]
  if(!nrow(sub)) return(absent)
  named <- sub$node[sub$layer != layer & sub$time == time]
  setdiff(absent, named)
}

# Whether the ties recorded at an occasion name who reported them. Where the
# occasion holds no ties at all, as where no reporter named any, the ties
# cannot say, and the network's 'observation' information says instead.
.occasion_reported <- function(.data, layer, time){
  ties <- .data$ties
  n <- if(is.null(ties)) 0L else nrow(ties)
  occasion <- dplyr::tibble(
    layer = if(is.null(ties[["layer"]])) NA_character_ else as.character(ties$layer),
    time = if(is.null(ties[["time"]])) NA else ties$time,
    .rows = n)
  held <- .same_occasion(occasion, layer, time)
  if(any(held)) return(.holds_node(ties[["by"]][held]))
  .observed_as(.data, layer) == "cognitive"
}

# How a layer was observed, as the network's 'observation' information says,
# whether it gives one design for the network or one for each layer.
.observed_as <- function(.data, layer){
  obs <- .data$info$observation
  if(is.null(obs)) return("")
  if(is.null(names(obs))) return(as.character(obs[1]))
  if(!is.na(layer) && layer %in% names(obs)) return(as.character(obs[[layer]]))
  ""
}

# The report a reporter was asked for: every dyad among the nodes in the
# network at that occasion, between the two modes where it is two-mode, and
# once for each dyad where the layer is undirected.
.unreported_report <- function(.data, reporter, active, layer, time, directed){
  nodes <- which(active)
  # A multilevel network ties nodes within a mode too, so every pair is asked
  # about, as in a one-mode network.
  if(is_twomode(.data) && !is_multilevel(.data)){
    modes <- .data$nodes$mode
    senders <- nodes[modes[nodes] == modes[1]]
    pairs <- expand.grid(from = senders, to = setdiff(nodes, senders))
  } else {
    pairs <- expand.grid(from = nodes, to = nodes)
    pairs <- pairs[if(directed) pairs$from != pairs$to else
      pairs$from < pairs$to, , drop = FALSE]
  }
  if(!nrow(pairs)) return(NULL)
  dplyr::tibble(from = as.integer(pairs$from), to = as.integer(pairs$to),
                layer = layer, time = time, by = as.integer(reporter))
}

# The registry of missing ties that no node's nonresponse implies.
.missing_registry <- function(.data){
  reg <- .data$missings
  if(is.null(reg) || !nrow(reg)) return(NULL)
  reg <- dplyr::as_tibble(reg)
  if(is.null(reg[["layer"]])) reg$layer <- NA_character_
  if(is.null(reg[["time"]])) reg$time <- NA
  reg[intersect(c("from", "to", "layer", "time", "by", "about"), names(reg))]
}

# Keys for matching ties, one respecting direction and one disregarding it.
# Two reports of a tie are two ties, as is gossip about two targets.
.tie_key <- function(ties){
  if(is.null(ties) || !nrow(ties)) return(character(0))
  layer <- if(is.null(ties[["layer"]])) NA_character_ else as.character(ties$layer)
  time <- if(is.null(ties[["time"]])) NA else ties$time
  by <- if(is.null(ties[["by"]])) NA else ties$by
  about <- if(is.null(ties[["about"]])) NA else ties$about
  paste(ties$from, ties$to, layer, time, by, about, sep = "\r")
}

.undirected_key <- function(ties){
  lo <- pmin(ties$from, ties$to); hi <- pmax(ties$from, ties$to)
  by <- if(is.null(ties[["by"]])) NA else ties$by
  about <- if(is.null(ties[["about"]])) NA else ties$about
  paste(lo, hi, ties$layer, ties$time, by, about, sep = "\r")
}

# The reverse: nonresponse records from a list of missing ties. Any node whose
# whole reported neighbourhood is missing becomes a record of its own, and
# whatever is left over is held in the registry. Without this, a round trip
# through another class would return one row per missing tie.
.compress_missing <- function(x, missing){
  # The list of ties is the authority here, so any records the object already
  # carries are cleared and rebuilt rather than added to.
  x <- .clear_missing(x)
  if(is.null(missing) || !nrow(missing)) return(x)
  if(is.null(missing[["layer"]])) missing$layer <- NA_character_
  if(is.null(missing[["time"]])) missing$time <- NA
  times <- .stocnet_times(x)
  act_state <- .node_state(x, "active", times, default = TRUE)
  occasions <- unique(missing[c("layer", "time")])
  egocentric <- is_egocentric(x)
  found <- list()
  for(o in seq_len(nrow(occasions))){
    layer <- occasions$layer[[o]]; time <- occasions$time[[o]]
    at <- if(is.na(time)) 1L else match(time, times)
    if(is.na(at)) at <- 1L
    sub <- missing[.same_occasion(missing, layer, time), , drop = FALSE]
    directed <- layer_is_directed(x, layer)
    # Where the missing ties name their reporters, a node that did not report
    # is one whose whole report is missing. The missing ties are read here and
    # not the ties, since where no reporter named a tie there are none to read.
    if(!egocentric && .holds_node(sub[["by"]])){
      # Once the report is held as the node's nonresponse, nothing in the ties
      # may say that the network names its reporters, so the design says so.
      if(!nzchar(.observed_as(x, layer)) && is.null(names(x$info$observation)))
        x$info$observation <- "cognitive"
      dyad <- function(tab) if(directed) paste(tab$from, tab$to) else
        paste(pmin(tab$from, tab$to), pmax(tab$from, tab$to))
      for(node in unique(stats::na.omit(sub$by))){
        asked <- .unreported_report(x, node, act_state[, at], layer, time,
                                    directed)
        held <- sub[!is.na(sub$by) & sub$by == node, , drop = FALSE]
        if(!is.null(asked) && setequal(dyad(asked), dyad(held)))
          found[[length(found)+1]] <- dplyr::tibble(node = as.integer(node),
                                                    layer = layer, time = time,
                                                    reported = TRUE)
      }
      next
    }
    for(node in unique(c(sub$from, if(!directed) sub$to))){
      alters <- .stocnet_alters(x, node, act_state[, at])
      held <- if(directed) sub$to[sub$from == node] else
        c(sub$to[sub$from == node], sub$from[sub$to == node])
      if(length(alters) && setequal(alters, held))
        found[[length(found)+1]] <- dplyr::tibble(node = as.integer(node),
                                                  layer = layer, time = time,
                                                  reported = FALSE)
    }
  }
  found <- dplyr::bind_rows(found)
  if(nrow(found)){
    keep <- !.node_covered(missing, found, x)
    missing <- missing[keep, , drop = FALSE]
    x <- .record_nonresponse(x, found, times)
  }
  x$missings <- if(nrow(missing)) .tidy_registry(missing) else NULL
  x
}

.same_occasion <- function(ties, layer, time){
  (is.na(layer) & is.na(ties$layer) | (!is.na(layer) & ties$layer %in% layer)) &
    (is.na(time) & is.na(ties$time) | (!is.na(time) & ties$time %in% time))
}

# Which rows of a missing list a nonresponse record already accounts for.
.node_covered <- function(missing, found, x){
  covered <- rep(FALSE, nrow(missing))
  for(r in seq_len(nrow(found))){
    same <- .same_occasion(missing, found$layer[[r]], found$time[[r]])
    if(found$reported[[r]]){
      covered <- covered | (same & !is.na(missing$by) &
                              missing$by == found$node[[r]])
      next
    }
    directed <- layer_is_directed(x, found$layer[[r]])
    covered <- covered | (same & (missing$from == found$node[[r]] |
                                    (!directed & missing$to == found$node[[r]])))
  }
  covered
}

# Nonresponse records onto the nodes or the changelog. A node that reported at
# no point is held in the nodes, since there is no change to log; one that
# reported at some points and not others is a change like any other, so a
# change back is logged too where the node reported again afterwards.
.record_nonresponse <- function(x, found, times){
  if(all(is.na(found$time))){
    na <- rep(FALSE, net_nodes(x))
    na[found$node] <- TRUE
    x$nodes$na <- na
    return(x)
  }
  occasions <- .stocnet_occasions(x)
  rows <- lapply(sort(unique(found$node)), function(node){
    at <- sort(unique(found$time[found$node == node]))
    # Where the node reported on some of the layers recorded at that moment and
    # not others, the change names the layer it applies to.
    lyr <- vapply(at, function(t){
      l <- found$layer[found$node == node & found$time == t]
      if(setequal(l, occasions$layer[occasions$time %in% t])) NA_character_ else l[1]
    }, character(1))
    ends <- times[match(at, times) + 1]
    keep <- !is.na(ends) & !ends %in% at
    dplyr::bind_rows(
      dplyr::tibble(time = at, node = as.integer(node), var = "na",
                    value = rep(list(TRUE), length(at)), layer = lyr),
      dplyr::tibble(time = ends[keep], node = as.integer(node), var = "na",
                    value = rep(list(FALSE), sum(keep)), layer = lyr[keep]))
  })
  rows <- dplyr::bind_rows(rows)
  if(all(is.na(rows$layer))) rows$layer <- NULL
  rows$time <- .match_class(rows$time, x$changes[["time"]])
  out <- dplyr::bind_rows(x$changes, rows)
  x$changes <- out[order(out$time, out$node, out$var), , drop = FALSE]
  x
}

.match_class <- function(x, like){
  if(is.null(like)) x else if(is.integer(like)) as.integer(x) else x
}

.tidy_registry <- function(missing){
  out <- dplyr::as_tibble(missing)
  if(all(is.na(out$layer))) out$layer <- NULL
  if(all(is.na(out$time))) out$time <- NULL
  for(col in c("by", "about"))
    if(!is.null(out[[col]]) && all(is.na(out[[col]]))) out[[col]] <- NULL
  out
}

# Clear the records of what a network did not observe, whether because those
# ties have been imputed and so are missing no longer, or because the records
# are about to be rebuilt from a list of the ties themselves.
.clear_missing <- function(.data){
  .data$nodes$na <- NULL
  .data$missings <- NULL
  chg <- .data$changes
  if(!is.null(chg) && nrow(chg)){
    chg <- chg[as.character(chg$var) != "na", , drop = FALSE]
    if("layer" %in% names(chg) && all(is.na(chg$layer))) chg$layer <- NULL
    .data$changes <- if(nrow(chg)) chg else NULL
  }
  .data
}

# A ties table may arrive with an 'na' column marking which of its rows record a
# missing tie rather than a tie. That is a reasonable way to hand the data over,
# and how the other classes hold it, so it is accepted and split apart here.
# A missing weight is how a matrix, and SIENA, hold a missing tie. Marking it
# in an 'na' column hands it to `make_stocnet()` in the form that gets
# compressed into nonresponse records.
.mark_missing_weights <- function(ties){
  if(is.null(ties) || !"weight" %in% names(ties)) return(ties)
  miss <- is.na(ties$weight)
  if(!any(miss)) return(ties)
  ties$na <- miss
  # Where the weights recorded nothing but which ties are present, they are
  # dropped again, since the 'na' column now records all that they held.
  if(.holds_only_binary(ties$weight)) ties$weight <- NULL
  ties
}

.join_missing_ties <- function(.data){
  missing <- as_missinglist(.data)
  ties <- .data$ties
  if(is.null(missing) || !nrow(missing)) return(ties)
  missing <- missing[intersect(names(ties), names(missing))]
  missing$na <- TRUE
  dplyr::bind_rows(ties, missing)
}

.split_missing_input <- function(ties){
  if(is.null(ties) || !"na" %in% names(ties))
    return(list(ties = ties, missing = NULL))
  miss <- !is.na(ties$na) & ties$na
  ties$na <- NULL
  list(ties = ties[!miss, , drop = FALSE],
       missing = if(any(miss)) ties[miss, , drop = FALSE] else NULL)
}
