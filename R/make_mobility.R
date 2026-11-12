# Mobility ####

#' Making mobility models on networks
#'
#' @description
#' These functions allow mobility games to be played upon networks.
#'
#' - `play_mobility()` moves the occupants of a network of places
#'   to empty places, as in Schelling's model of segregation,
#'   in vacancy chains, or in migration.
#'
#' Each node is a place, which holds at most one occupant.
#' The nodal attributes describe the occupant of the place,
#'   and a missing value of `attribute` marks a place as empty.
#' When an occupant moves, all its attributes move with it.
#' Only the attributes of the place stay where they are:
#'   the node labels, which name the places, the nodes' mode (`type`),
#'   and whether the place is `active`.
#'
#' At each step, the occupants that are dissatisfied with their place are
#'   checked in the order given by `who_moves`.
#' The first of them that has an empty place to go to,
#'   within `max_dist` of its place,
#'   moves to the place that `choice_function` chooses.
#' The game stops after `steps` moves,
#'   or when no dissatisfied occupant has a place to go to.
#' @name make_mobility
#' @family makes
#' @family models
#' @param steps The maximum number of moves.
#'   By default the number of nodes in the network.
#' @param attribute A string naming some nodal attribute in the network.
#'   A missing value marks an empty place.
#'   Currently only tested for binary attributes.
#' @param preference What satisfies an occupant with its place.
#'   Either a number, or a function.
#'
#'   A number is a threshold from -1 to 1 on the E-I index of the occupant's
#'   neighbours on `attribute`,
#'   above which the occupant is dissatisfied.
#'   One number gives the same threshold to every occupant,
#'   and a vector as long as the nodes of the network gives each occupant
#'   its own threshold, which moves with it.
#'   By default this is 0, meaning occupants will be dissatisfied if more than
#'   half of their neighbours differ on the given attribute.
#'
#'   A function takes the network (as an 'igraph') and the index of a node,
#'   in this order, and returns the satisfaction of the occupant of that node
#'   as one number.
#'   An occupant with a satisfaction below 0 is dissatisfied.
#'   This allows other preferences, such as a continuous utility.
#' @param who_moves The order in which dissatisfied occupants are checked,
#'   one of the following options:
#'   "ordered" (the default) checks them in the order of the nodes,
#'   "random" checks them in a random order,
#'   and "most_dissatisfied" checks the most dissatisfied first.
#'   Alternatively, a function that takes the network and returns
#'   one score for each node, such as `netrics::node_by_degree`.
#'   The occupants are then checked from the highest score to the lowest.
#' @param choice_function The empty place that a moving occupant chooses,
#'   one of the following options:
#'   "satisficing" (the default) chooses at random among the places that
#'   would satisfy it,
#'   "optimising" chooses the place that would satisfy it most,
#'   and "minimising" chooses the nearest of the places that would satisfy it.
#'   With "satisficing" or "minimising", an occupant that no empty place
#'   would satisfy does not move, and the next occupant is checked.
#'   Alternatively, a function that takes the network and returns
#'   one score for each node.
#'   The occupant then chooses the empty place with the highest score.
#' @param max_dist The maximum distance along ties from an occupant's place to
#'   an empty place that it can move to.
#'   By default `Inf`, so that an occupant can move to any empty place,
#'   whether or not there is a path to it.
#'   In a directed network, distances follow the direction of the ties.
#'   With `max_dist = 1` on a directed network of positions,
#'   an occupant can move only to a next position,
#'   so that vacancy chains follow from the empty places.
#' @param ... Former names of arguments, which still work but are deprecated:
#'   `heterophily` for `preference`.
#' @return The network of the class it was given,
#'   with the occupants in their start places,
#'   and a record of the moves in its changes.
#'   Each move is one step of time.
#'   At each move, the record gives the new value of each attribute
#'   of the occupant at the two places, and the `satisfied` value
#'   of each occupant whose satisfaction changes.
#'   Use [to_time()] to get the network at a step,
#'   [as_mobility()] to get a table of the moves,
#'   and [to_mobility()] to get the moves as a network of places.
#'
#'   Two node attributes are added at the start.
#'   `occupant` names the occupant of each place, with `NA` for an empty place.
#'   A network that already has an `occupant` attribute keeps it.
#'   Otherwise, each occupant has the index of its start place as its name.
#'   `satisfied` is `TRUE` for an occupant that is satisfied with its place,
#'   `FALSE` for an occupant that is dissatisfied,
#'   and `NA` for an empty place.
#'   A 'stocnet' also keeps `attribute`, and `preference` if it is a number,
#'   in `info$mobility`.
#' @references
#' ## On segregation
#' Schelling, Thomas C. 1971.
#' "Dynamic Models of Segregation."
#' _Journal of Mathematical Sociology_ 1(2): 143-186.
#' \doi{10.1080/0022250X.1971.9989794}
#'
#' ## On vacancy chains
#' White, Harrison C. 1970.
#' _Chains of Opportunity: System Models of Mobility in Organizations_.
#' Cambridge, MA: Harvard University Press.
#' @examples
#'   startValues <- rbinom(100,1,prob = 0.5)
#'   startValues[sample(seq_len(100), round(100*0.2))] <- NA
#'   latticeEg <- create_lattice(100)
#'   latticeEg <- add_node_attribute(latticeEg, "startValues", startValues)
#'   latticeEg
#'   schelling <- play_mobility(latticeEg, "startValues", 0.5)
#'   # The network after the last move
#'   to_time(schelling, max(as_changelist(schelling)$time))
#'   # A vacancy chain: a person moves up one position when it is empty
#'   ladder <- delete_ties(create_ring(10, directed = TRUE), 10)
#'   ladder <- add_node_attribute(ladder, "person", c(LETTERS[1:9], NA))
#'   chain <- play_mobility(ladder, "person",
#'                          preference = function(.data, node) -1,
#'                          choice_function = "optimising", max_dist = 1)
#'   as_mobility(chain, "person")
#' @export
play_mobility <- function(.data,
                          attribute,
                          preference = 0,
                          who_moves = c("ordered","random","most_dissatisfied"),
                          choice_function = c("satisficing","optimising", "minimising"),
                          max_dist = Inf,
                          steps,
                          ...) {
  former <- .former_args(list(...), c(heterophily = "preference"))
  if (!is.null(former$preference)) {
    snet_warn(c("!" = "The argument `heterophily` is deprecated.",
                "i" = "Please use `preference` instead."))
    preference <- former$preference
  }
  g <- as_igraph(.data)
  n <- net_nodes(g)
  if(missing(steps)) steps <- n
  if(!attribute %in% igraph::vertex_attr_names(g))
    snet_abort("There is no nodal attribute called {.val {attribute}}.")
  if(!is.function(who_moves)) who_moves <- match.arg(who_moves)
  if(!is.function(choice_function)) choice_function <- match.arg(choice_function)
  if(!is.numeric(max_dist) || length(max_dist) != 1 || is.na(max_dist) || max_dist < 1)
    snet_abort("`max_dist` must be one number of at least 1.")
  preference_given <- preference
  if(!is.function(preference)){
    if(!is.numeric(preference))
      snet_abort("`preference` must be a number or a function.")
    if(length(preference)==1) preference <- rep(preference, n)
    if(length(preference)!=n)
      snet_abort(paste("`preference` must be one number,",
                       "or as many numbers as there are nodes in the network."))
  }

  # Who holds each place. This identifies an occupant across its moves.
  holder <- seq_len(n)
  holder[is.na(igraph::vertex_attr(g, attribute))] <- NA
  # The `occupant` column names the occupants in the record of moves.
  # A network that has no such column gets one, with the start node index
  # of each occupant.
  if(!"occupant" %in% igraph::vertex_attr_names(g)){
    snet_info("Named each occupant by its start node in the node attribute {.var occupant}.")
    g <- igraph::set_vertex_attr(g, "occupant", value = as.character(holder))
    .data <- add_node_attribute(.data, "occupant", as.character(holder))
  }
  # Everything that describes the occupant moves with it;
  # only what describes the place stays:
  # its label, its mode, and whether it is active.
  # Whether the occupant is satisfied is found again after each move.
  carried <- setdiff(igraph::vertex_attr_names(g),
                     c("name", "type", "active", "satisfied"))

  # The satisfaction of the occupants of the places given,
  # were the network `net`
  satisfaction <- function(net, places, occupants){
    if(is.function(preference)){
      vapply(places, function(p) as.numeric(preference(net, p))[1],
             FUN.VALUE = numeric(1))
    } else {
      preference[occupants] - .node_heterophily(net, attribute)[places]
    }
  }
  swap <- function(net, i, j){
    for(a in carried){
      x <- igraph::vertex_attr(net, a)
      x[c(i,j)] <- x[c(j,i)]
      net <- igraph::set_vertex_attr(net, a, value = x)
    }
    net
  }
  node_scores <- function(fn, net){
    out <- as.numeric(fn(net))
    if(length(out) != n)
      snet_abort("A function in `who_moves` or `choice_function` must return one score for each node.")
    out
  }

  # Whether each occupant is satisfied, with NA for an empty place
  satisfied_now <- function(net){
    sat <- rep(NA_real_, n)
    occupied <- which(!is.na(holder))
    if(length(occupied))
      sat[occupied] <- satisfaction(net, occupied, holder[occupied])
    sat
  }
  sat <- satisfied_now(g)
  satisfied <- ifelse(is.na(sat), NA, sat >= 0)
  .data <- add_node_attribute(.data, "satisfied", satisfied)
  # The record of the moves: the nodes keep the start,
  # and each move adds its rows at its step
  record <- list()
  t <- 0
  while(steps > t){
    occupied <- which(!is.na(holder))
    unoccupied <- which(is.na(holder))
    if(length(unoccupied)==0 || length(occupied)==0) break
    dissatisfied <- occupied[!is.na(sat[occupied]) & sat[occupied] < 0]
    if(length(dissatisfied)==0) break
    if(is.function(who_moves)){
      scores <- node_scores(who_moves, g)
      dissatisfied <- dissatisfied[order(-scores[dissatisfied])]
    } else dissatisfied <- switch(who_moves,
           ordered = dissatisfied,
           random = dissatisfied[sample.int(length(dissatisfied))],
           most_dissatisfied = dissatisfied[order(sat[dissatisfied])])
    if(is.function(choice_function)) choice_scores <- node_scores(choice_function, g)

    move_from <- move_to <- NA
    for(i in dissatisfied){
      options <- unoccupied
      if(is.finite(max_dist) || identical(choice_function, "minimising")){
        dists <- igraph::distances(g, v = i, to = options, mode = "out")[1,]
        if(is.finite(max_dist)){
          options <- options[dists <= max_dist]
          dists <- dists[dists <= max_dist]
        }
      }
      if(length(options)==0) next
      if(is.function(choice_function)){
        move_to <- options[which.max(choice_scores[options])]
      } else {
        option_sat <- vapply(options, function(u){
          satisfaction(swap(g, i, u), u, holder[i])
        }, FUN.VALUE = numeric(1))
        if(choice_function == "optimising"){
          move_to <- options[which.max(option_sat)]
        } else {
          # Satisficing and minimising both take only a place that satisfies
          satisfies <- !is.na(option_sat) & option_sat >= 0
          if(!any(satisfies)) next
          move_to <- if(choice_function == "minimising"){
            options[satisfies][which.min(dists[satisfies])]
          } else {
            options[satisfies][sample.int(sum(satisfies), 1)]
          }
        }
      }
      move_from <- i
      break
    }
    if(is.na(move_from)) break
    t <- t+1
    snet_minor_info("Moving the occupant of node {move_from} to node {move_to}")
    g <- swap(g, move_from, move_to)
    holder[c(move_from, move_to)] <- holder[c(move_to, move_from)]
    # The carried attributes at the two places, then the occupants
    # whose satisfaction changes
    places <- c(move_from, move_to)
    for(a in carried){
      record[[length(record) + 1]] <- list(time = t, node = places, var = a,
                                           value = igraph::vertex_attr(g, a)[places])
    }
    sat <- satisfied_now(g)
    now <- ifelse(is.na(sat), NA, sat >= 0)
    changed <- which(!(now %in% NA & satisfied %in% NA) &
                       (is.na(now) | is.na(satisfied) | now != satisfied))
    if(length(changed))
      record[[length(record) + 1]] <- list(time = t, node = changed,
                                           var = "satisfied",
                                           value = now[changed])
    satisfied <- now
  }

  # The nodes keep the start, and the moves are bound as changes
  if(inherits(.data, "stocnet")){
    .data$info$mobility <- list(attribute = attribute)
    if(is.numeric(preference_given))
      .data$info$mobility$preference <- preference_given
  }
  if(length(record)){
    changes <- dplyr::tibble(
      time = unlist(lapply(record, function(r) rep(r$time, length(r$node)))),
      node = as.integer(unlist(lapply(record, `[[`, "node"))),
      var = unlist(lapply(record, function(r) rep(r$var, length(r$node)))),
      value = unlist(lapply(record, function(r) as.list(r$value)),
                     recursive = FALSE))
    .data <- bind_changes(.data, changes)
  }
  .data
}

.node_heterophily <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  if(anyNA(attribute)){
    m[is.na(attribute),] <- NA
    m[,is.na(attribute)] <- NA
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- rowSums(m * same, na.rm = TRUE)
  nInternal[is.na(attribute)] <- NA
  nExternal <- rowSums(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / rowSums(m, na.rm = TRUE)
  ei
}
