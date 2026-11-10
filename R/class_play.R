# diffusionModel ####

# A diffusionModel keeps diff_model as its second class for one release,
# so that methods and checks written for a diff_model still find it.
make_diff_model <- function(events, report, .data) {
  class(report) <- c("diffusionModel", "diff_model", class(report))
  attr(report, "events") <- events
  if(is_list(.data)){
    attr(report, "mode") <- node_is_mode(.data[[1]])
    attr(report, "network") <- .data
  } else {
    attr(report, "mode") <- node_is_mode(.data)
    attr(report, "network") <- as_tidygraph(.data)
  }
  report
}

#' @export
print.diff_model <- function(x, ..., verbose = FALSE){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  if(!verbose){
    x$n <- NULL
    x$s <- NULL
    x <- x[, !endsWith(names(x), "_new"), drop = FALSE]
  }
  print(dplyr::tibble(x, ...))
  invisible(x)
}

#' @export
summary.diff_model <- function(object, ...) {
  dplyr::tibble(attr(object, "events"), ...)
}
#' @rdname make_play
#' @param twomode Not used. Kept for compatibility.
#' @param events A table (data frame or tibble) of diffusion events
#'   with columns `t` indicating the time (typically an integer) of the event, 
#'   `nodes` indicating the number or name of the node involved in the event,
#'   and `event`, which can take on the values "I" for an infection event,
#'   "E" for an exposure event, or "R" for a recovery event.
#' @returns 
#'   `as_diffusion()` returns a 'diffusionModel' object
#'   (with the class 'diff_model' second, for compatibility),
#'   a table of the number of nodes in each value of the attribute at each step,
#'   from the start of the diffusion.
#'   The table has a column for each value, and a column of the new nodes in
#'   each value (for example `I` and `I_new`).
#'   A record in S, E, I, and R always has these four columns,
#'   and `s` counts the susceptible nodes that are exposed.
#'   The table keeps the name of the attribute in `attr(, "attribute")`,
#'   and the roles of its values in `attr(, "compartments")`,
#'   where these are known.
#'   By default, a compact version of the component table is printed
#'   (to print all the changes at each time point, use `print(..., verbose = T)`).
#'   To retrieve the diffusion events table, use `summary(...)`.
#' @importFrom dplyr tibble
#' @examples
#'   # How to create a diffusionModel from (basic) observed data
#'   events <- data.frame(time = c(0,1,1,2,3),
#'                        node = c(1,2,3,2,4),
#'                        var = "diffusion",
#'                        value = c("I","I","I","R","I"))
#'   as_diffusion(bind_changes(create_filled(4), events))
#' @export
as_diffusion <- function(.data, attribute = "diffusion", twomode = FALSE, events) 
  UseMethod("as_diffusion")

#' @export
as_diffusion.diff_model <- function(.data, attribute = "diffusion", twomode = FALSE, events) {
  .data
}

#' @export
as_diffusion.tbl_graph <- function(.data, attribute = "diffusion", twomode = FALSE, events) {
  if (missing(events)) events <- as_changelist(.data)
  report <- .diffusion_report(.data, attribute, events)
  keys <- attributes(report)[c("attribute", "compartments")]
  class(report) <- c("diffusionModel", "diff_model", class(report))
  attributes(report)[names(keys)] <- keys
  report
}

#' @export
as_diffusion.igraph <- function(.data, attribute = "diffusion", twomode = FALSE, events) {
  net <- as_tidygraph(.data)
  if (missing(events)) events <- as_changelist(.data)
  events <- .diffusion_events(events, attribute)
  report <- .diffusion_report(net, attribute, events)
  keys <- attributes(report)[c("attribute", "compartments")]
  report <- dplyr::rename(report, t = time)
  report$S_new <- NULL
  events <- dplyr::transmute(events, t = time, nodes = node, event = value)
  report <- make_diff_model(events, report, .data)
  attributes(report)[names(keys)] <- keys
  report
}

# A diffusion is reported in the same way whatever class it was played on,
# with a `time` column and the nodes newly susceptible in `S_new`.
#' @export
as_diffusion.stocnet <- as_diffusion.tbl_graph

# Keeps the changes of the attribute, with the values as a vector.
.diffusion_events <- function(events, attribute){
  if(is.null(events)) 
    events <- data.frame(time = numeric(0), node = integer(0), 
                         var = character(0), value = character(0))
  events <- as.data.frame(events)
  # an events table in the old form, as `play_diffusion(old_version = TRUE)` gives
  if(all(c("t", "nodes", "event") %in% names(events)) && !"time" %in% names(events))
    events <- data.frame(time = events$t, node = events$nodes, 
                         var = attribute, value = events$event)
  # a network can record other changes too, such as when its nodes are active
  if ("var" %in% names(events)) 
    events <- events[events$var == attribute, , drop = FALSE]
  events$value <- .unlist_values(events$value)
  if(is.factor(events$value)) events$value <- as.character(events$value)
  events
}

# Counts the nodes in each value of `attribute` at each step, from the start
# of the diffusion to its last change.
.diffusion_report <- function(.data, attribute = "diffusion", events){
  events <- .diffusion_events(events, attribute)
  n <- c(net_nodes(.data))
  comps <- if(inherits(.data, "stocnet")) .data$info$compartments else NULL
  # the start state is on the nodes; without it, all nodes start in S
  susceptible <- if(!is.null(comps$S)) comps$S[1] else "S"
  state <- if(attribute %in% net_node_attributes(.data)) 
    node_attribute(.data, attribute) else rep(susceptible, n)
  if(is.factor(state)) state <- as.character(state)
  if(nrow(events) && is.character(events$value) && !is.character(state)) 
    state <- as.character(state)
  # the values, in the order of the compartments, then as they appear
  values <- unique(c(unlist(comps, use.names = FALSE), 
                     sort(unique(c(state[!is.na(state)], events$value)))))
  seir <- all(values %in% c("S", "E", "I", "R", "D"))
  if(seir) values <- union(c("S", "E", "I", "R"), values)
  # the roles of the values, so that a plot can name and colour them
  if(is.null(comps)) comps <- if(is.logical(state)) list(S = FALSE, I = TRUE) else
    if(seir) list(S = "S", E = "E", I = "I", R = "R")
  # a missing state starts as susceptible, as in `play_diffusion()`
  if(!is.null(comps$S)) state[is.na(state)] <- comps$S[1]
  # the diffusion starts at the start of the network, or its first change
  begin <- min(c(.network_start(.data), events$time))
  end <- max(c(begin, events$time))
  times <- seq(begin, end)
  
  infectious <- if(!is.null(comps$I)) comps$I else if("I" %in% values) "I" else NULL
  recovered <- if(!is.null(comps$R)) comps$R else if(seir) "R" else NULL
  mat <- if(!is.null(infectious) && !is_twomode(.data)) 
    .exposure_matrix(.data) else NULL
  counts <- matrix(0, length(times), length(values))
  news <- matrix(0, length(times), length(values))
  exposed <- rep(0, length(times))
  for(i in seq_along(times)){
    now <- events[events$time == times[i], , drop = FALSE]
    if(nrow(now)) state[now$node] <- now$value
    counts[i, ] <- vapply(values, function(v) sum(state %in% v), numeric(1))
    news[i, ] <- vapply(values, function(v) sum(now$value %in% v), numeric(1))
    if(!is.null(infectious)){
      infected <- which(state %in% infectious)
      expos <- if(is_twomode(.data)) c(.node_is_exposed(.data, infected)) else
        .exposure_kernel(mat, infected, weighted = FALSE) > 0
      expos[state %in% recovered] <- FALSE
      exposed[i] <- sum(expos)
    }
  }
  report <- dplyr::tibble(time = times, n = n)
  for(j in seq_along(values)){
    v <- as.character(values[j])
    report[[v]] <- as.integer(counts[, j])
    if(!is.null(infectious) && j == 1) report$s <- exposed
    report[[paste0(v, "_new")]] <- as.integer(news[, j])
  }
  if(seir) report <- dplyr::select(report, 
                                   dplyr::any_of(c("time", "n", "S", "s", "S_new", 
                                                   "E", "E_new", "I", "I_new", 
                                                   "R", "R_new", "D", "D_new")))
  attr(report, "attribute") <- attribute
  attr(report, "compartments") <- comps
  report
}

#' @export
as_diffusion.diffnet <- function(.data, attribute = "diffusion", twomode = FALSE, events) {
  diffnet <- .data
  net <- as.matrix(.data$graph[[1]])
  event <- NULL
  events <- data.frame(t = .data$toa, 
                       nodes = attr(.data$toa, "names"), 
                       event = "I")
  if(!all.equal(diffnet$graph[[1]], diffnet$graph[[length(diffnet$graph)]]))
    warning(paste("This function currently only takes the first network.",
                  "Network changes are not currently retained."))
  rownames(net) <- diffnet$meta$ids
  colnames(net) <- diffnet$meta$ids
  sumchanges <- events |> dplyr::group_by(t) |> 
    dplyr::reframe(I_new = sum(event == "I"),
                   E_new = sum(event == "E"),
                   R_new = sum(event == "R"))
  report <- dplyr::tibble(t = min(events$t):max(events$t),
                          n = diffnet$meta$n) |> 
    dplyr::left_join(sumchanges, by = dplyr::join_by(t))
  report[is.na(report)] <- 0
  report$R <- cumsum(report$R_new)
  report$I <- cumsum(report$I_new) - report$R
  report$E <- ifelse(report$E_new == 0 & 
                       cumsum(report$E_new) == max(cumsum(report$E_new)),
                     report$E_new, cumsum(report$E_new))
  report$E <- ifelse(report$R + report$I + report$E > report$n,
                     report$n - (report$R + report$I),
                     report$E)
  report$S <- report$n - report$R - report$I - report$E
  report$s <- vapply(report$t, function(time){
    twin <- dplyr::filter(events, events$t <= time)
    infected <- dplyr::filter(twin, twin$event == "I")$nodes
    recovered <- dplyr::filter(twin, twin$event == "R")$nodes
    infected <- setdiff(infected, recovered)
    expos <- .node_is_exposed(as_igraph(net), infected)
    expos[infected] <- F
    expos[recovered] <- F
    sum(expos)
  }, numeric(1) )
  if (any(report$R + report$I + report$E + report$S != report$n)) {
    snet_abort("Oops, something is wrong")
  }
  if(is_labelled(net)) events$nodes <- match(events$nodes, node_labels(net))
  events <- events |> dplyr::arrange(t)
  report <- dplyr::select(report, dplyr::any_of(c("t", "n", "S", "s", "E", "E_new", "I", "I_new", "R", "R_new")))
  make_diff_model(events, report, net)
}

# diffusionTransitions ####

# A table of transitions shows a joint source as "E+I", a spontaneous
# transition with no source as NA, and a function in `rate` as "<function>".
#' @export
print.diffusionTransitions <- function(x, ...){
  show <- function(v) vapply(v, function(e){
    if(is.function(e)) "<function>" else if(is.null(e) || all(is.na(e))) NA_character_ else
      if(length(e) > 1 && !is.character(e)) "<by node>" else
        if(is.numeric(e)) format(signif(e, 3)) else paste(e, collapse = "+")
  }, character(1))
  out <- dplyr::tibble(from = x$from, to = x$to, rate = show(x$rate),
                       source = show(x$source), exposure = x$exposure,
                       threshold = show(x$threshold),
                       prevalence = x$prevalence)
  print(out, ...)
  invisible(x)
}

# learn_model ####
make_learn_model <- function(out, .data) {
  out <- as.data.frame(out)
  if(is_labelled(.data))
    names(out) <- node_labels(.data)
  class(out) <- c("learn_model", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

#' @export
print.learn_model <- function(x, ...){
  print(dplyr::tibble(x))
  invisible(x)
}

#' @export
summary.learn_model <- function(object, ..., epsilon = 0.0005) {
  steps <- nrow(object)
  max_belief <- max(object[steps,])
  min_belief <- min(object[steps,])
  if(abs(max_belief - min_belief) < epsilon){
    cat(paste(steps-1, 
              "steps to convergence.\n"))
    cat("Final belief =", max_belief)
  } else 
    cat(paste("No convergence after",
              steps-1, "steps."))
}

# opinionModel ####

#' @rdname make_opinion
#' @param .data A network.
#'   For `as_opinion()`, a network with a record of `opinions`
#'   in its changes, or an 'opinionModel'.
#' @returns
#'   `as_opinion()` returns an 'opinionModel' (a data frame),
#'   with one row for each step from the start,
#'   and one column for each node.
#' @examples
#' as_opinion(play_opinion(create_ring(6), opinions = 1:6/6))
#' @export
as_opinion <- function(.data, opinions = "opinions") UseMethod("as_opinion")

#' @export
as_opinion.opinionModel <- function(.data, opinions = "opinions") .data

#' @export
as_opinion.learn_model <- function(.data, opinions = "opinions"){
  class(.data) <- unique(c("opinionModel", class(.data)))
  .data
}

#' @export
as_opinion.default <- function(.data, opinions = "opinions"){
  if(!is_manynet(.data))
    snet_abort("{.fn as_opinion} needs a network with a record of opinions,",
               "such as one that {.fn play_opinion} returns.")
  if(!is.character(opinions) || length(opinions) != 1)
    snet_abort("{.arg opinions} must be the name of one node attribute.")
  if(!opinions %in% net_node_attributes(.data))
    snet_abort("There is no node attribute {.val {opinions}} in the network.",
               "Please name the attribute that holds the opinions.")
  start <- node_attribute(.data, opinions)
  if(is.logical(start)) start <- start * 1
  if(!is.numeric(start))
    snet_abort("The node attribute {.val {opinions}} must be numeric.")
  changes <- as_changelist(.data)
  if(!is.null(changes) && nrow(changes) > 0)
    changes <- changes[changes$var == opinions, , drop = FALSE]
  if(is.null(changes) || nrow(changes) == 0)
    return(make_opinion_model(matrix(start, nrow = 1), .data))
  values <- changes$value
  if(is.list(values)) values <- unlist(values, use.names = FALSE)
  values <- as.numeric(values)
  times <- changes$time
  nodes <- changes$node
  if(is.character(nodes)) nodes <- match(nodes, node_labels(.data))
  changes$node <- nodes
  steps <- sort(unique(c(0, times)))
  if(all(steps == round(steps))) steps <- seq(min(steps), max(steps))
  out <- matrix(NA_real_, length(steps), length(start))
  current <- as.numeric(start)
  for(k in seq_along(steps)){
    now <- times == steps[k]
    current[changes$node[now]] <- values[now]
    out[k, ] <- current
  }
  make_opinion_model(out, .data)
}

make_opinion_model <- function(out, .data) {
  out <- as.data.frame(out)
  if(is_labelled(.data))
    names(out) <- node_labels(.data)
  class(out) <- c("opinionModel", "learn_model", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

#' @export
print.opinionModel <- function(x, ...){
  print(dplyr::tibble(x))
  invisible(x)
}

#' @export
summary.opinionModel <- function(object, ..., tolerance = 0.0005) {
  steps <- nrow(object)
  final <- unlist(object[steps, ], use.names = FALSE)
  if(abs(max(final) - min(final)) < tolerance){
    cat(paste(steps - 1, "steps to consensus.\n"))
    cat("Final opinion =", max(final))
  } else
    cat(paste("No consensus after", steps - 1, "steps."))
}

# mobilityModel ####

#' @rdname make_mobility
#' @param .data A network.
#'   For `as_mobility()`, a network of places with a record of `occupant`
#'   in its changes, such as [play_mobility()] gives, or a 'mobilityModel'.
#' @returns
#'   `as_mobility()` returns a 'mobilityModel' (a tibble),
#'   with one row for each move:
#'   the `time` of the move, the `person` who moves (the `occupant`),
#'   the node index of the `origin` and of the `destination`,
#'   the value of `attribute` for the person
#'   (in a column with the name of the attribute,
#'   or `value` if another column has that name),
#'   and the number of occupants that are `dissatisfied` after the move.
#'   `dissatisfied` is `NA` if the network has no `satisfied` record.
#' @export
as_mobility <- function(.data, attribute) UseMethod("as_mobility")

#' @export
as_mobility.mobilityModel <- function(.data, attribute) .data

#' @export
as_mobility.default <- function(.data, attribute){
  if(!is_manynet(.data))
    snet_abort("{.fn as_mobility} needs a network with a record of moves,",
               "such as one that {.fn play_mobility} returns.")
  net <- as_stocnet(.data)
  if(missing(attribute)) attribute <- net$info$mobility$attribute
  if(is.null(attribute))
    snet_abort("Please name the node attribute of the occupants in {.arg attribute}.")
  if(!is.character(attribute) || length(attribute) != 1 ||
     !attribute %in% names(net$nodes))
    snet_abort("There is no node attribute {.val {attribute}} in the network.")
  if(!"occupant" %in% names(net$nodes))
    snet_abort("{.fn as_mobility} needs a node attribute {.var occupant},",
               "such as one that {.fn play_mobility} adds.")
  changes <- net$changes
  if(is.null(changes)) changes <- data.frame(time = numeric(0), node = integer(0),
                                             var = character(0), value = character(0))
  if(is.character(changes$node))
    changes$node <- match(changes$node, net$nodes$label)
  value_of <- function(v){
    if(is.list(v)) unlist(lapply(v, function(e) if(length(e)) e[1] else NA))
    else v
  }
  moves <- changes[changes$var == "occupant", , drop = FALSE]
  moves$value <- as.character(value_of(moves$value))
  times <- sort(unique(moves$time))
  start_value <- net$nodes[[attribute]]
  att <- changes[changes$var == attribute, , drop = FALSE]
  att$value <- value_of(att$value)
  sat <- changes[changes$var == "satisfied", , drop = FALSE]
  sat$value <- as.logical(value_of(sat$value))
  satisfied <- if("satisfied" %in% names(net$nodes))
    as.logical(net$nodes$satisfied) else NULL
  rows <- lapply(times, function(t){
    now <- moves[moves$time == t, , drop = FALSE]
    to <- now$node[!is.na(now$value)]
    from <- now$node[is.na(now$value)]
    if(length(to) != 1 || length(from) != 1)
      snet_abort("At time {t}, the record of {.var occupant} is not one move.")
    a <- att[att$time == t & att$node == to, , drop = FALSE]
    dplyr::tibble(time = t, person = now$value[now$node == to],
                  origin = as.integer(from), destination = as.integer(to),
                  value = if(nrow(a)) a$value[nrow(a)] else start_value[from])
  })
  out <- dplyr::bind_rows(rows)
  if(!nrow(out))
    out <- dplyr::tibble(time = numeric(0), person = character(0),
                         origin = integer(0), destination = integer(0),
                         value = start_value[0])
  # the value keeps the name of the attribute, unless another column has it
  if(!attribute %in% c("time", "person", "origin", "destination", "dissatisfied"))
    names(out)[names(out) == "value"] <- attribute
  out$dissatisfied <- if(is.null(satisfied)) rep(NA_integer_, nrow(out)) else
    vapply(out$time, function(t){
      now <- satisfied
      upto <- sat[sat$time <= t, , drop = FALSE]
      now[upto$node] <- upto$value
      sum(!now, na.rm = TRUE)
    }, integer(1))
  class(out) <- c("mobilityModel", class(out))
  attr(out, "attribute") <- attribute
  out
}

#' @export
print.mobilityModel <- function(x, ...){
  print(dplyr::tibble(x), ...)
  invisible(x)
}
