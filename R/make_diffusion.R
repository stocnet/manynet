# Diffusions ####

#' Making diffusion models on networks
#' @description
#' These functions simulate diffusion or compartment models upon a network.
#' 
#' - `play_diffusion()` runs a single simulation of a compartment model,
#' allowing the results to be visualised and examined.
# #' - `play_diffusions()` runs multiple simulations of a compartment model
# #' for more robust inference.
#' 
#' These functions allow both a full range of compartment models,
#' as well as simplex and complex diffusion to be simulated upon a network.
#' 
#' @section Simple and complex diffusion: 
#' 
#' By default, the function will simulate a simple diffusion process in
#' which some infectious disease or idea diffuses from seeds through
#' contacts at some constant rate (`transmissibility`).
#' 
#' These `seeds` can be specified by a vector index 
#' (the number of the position of each node in the network that should serve as a seed) 
#' or as a logical vector where TRUE is interpreted as already infected.
#' 
#' `thresholds` can be set such that adoption/infection requires more than one
#' (the default) contact already being infected.
#' This parameter also accepts a vector so that thresholds can vary.
#' 
#' Complex diffusion is where the `thresholds` are defined less than one.
#' In this case, the thresholds are interpreted as proportional.
#' That is, the threshold to adoption/infection is defined by the
#' proportion of the node's contacts infected.
#' 
#' Nodes that cannot be infected can be indicated as `immune`
#' with a logical vector or index, similar to how `seeds` are identified.
#' Note that `immune` nodes are interpreted internally as Recovered (R)
#' and are thus subject to `waning` (see below).
#' @section Compartment models: 
#' 
#' Compartment models are flexible models of diffusion or contagion,
#' where nodes are compartmentalised into one of two or more categories.
#' 
#' The most basic model is the SI model.
#' The SI model is the default in `play_diffusion()`/`play_diffusions()`,
#' where nodes can only move from the Susceptible (S) category to the
#' Infected (I) category.
#' Whether nodes move from S to I depends on whether they are exposed
#' to the infection, for instance through a contact,
#' the `transmissibility` of the disease,
#' and their `thresholds` to the disease.
#' 
#' Another common model is the SIR model.
#' Here nodes move from S to I, as above, but additionally they can
#' move from I to a Recovered (R) status.
#' The probability that an infected node recovers at a timepoint
#' is controlled by the `recovery` parameter.
#' 
#' The next most common models are the SIS and SIRS models.
#' Here nodes move from S to I or additionally to R, as above, 
#' but additionally they can move from I or R back to a Susceptible (S) state.
#' This probability is governed by the `waning` parameter.
#' Where `recover > 0` and `waning = 1`, the Recovery (R) state will be skipped
#' and the node will return immediately to the Susceptible (S) compartment.
#' 
#' Lastly, these functions also offer the possibility of specifying
#' a latency period in which nodes have been infected but are not yet infectious.
#' Where `latency > 0`, an additional Exposed (E) compartment is introduced
#' that governs the probability that a node moves from this E compartment
#' to infectiousness (I).
#' This can be used in in SEI, SEIS, SEIR, and SEIRS models.
#' @section Start state:
#'
#' The state of each node is held in a categorical node attribute,
#' named by `attribute` (by default `"diffusion"`).
#' The diffusion starts from this attribute in one of three ways,
#' in this order:
#'
#' 1. The attribute has changes recorded on the network:
#'   the diffusion continues this history from its last time,
#'   and the history stays in the record.
#'   Observed times of adoption can be added first with [bind_changes()].
#' 2. The attribute is on the nodes, but has no changes:
#'   its values are the start state, at the first time of the network.
#'   A logical attribute reads `TRUE` as infected (I) and `FALSE` as
#'   susceptible (S).
#' 3. The attribute is not on the nodes:
#'   all nodes start as susceptible, `seeds` start as infected,
#'   and `immune` nodes start as recovered,
#'   at the first time of the network.
#'
#' The first time of the network is the earliest `time` of its ties,
#' or 0 for a network whose ties have no time.
#' `seeds` and `immune` are used only in the third case.
#' A node whose state is missing (`NA`) starts as susceptible.
#' In the first two cases, the record holds only the changes after the start,
#' because the attribute already holds the start state.
#'
#' `compartments` maps the roles of the model (S, E, I, R, and D) onto
#' the values of the attribute, for example
#' `compartments = c(S = "unaware", I = "adopter")`.
#' Only the roles that the model uses must be given:
#' S and I always, R for `recovery` or `immune`, E for `latency`,
#' and D for `fatality`.
#' Several values can share a role, as in
#' `compartments = list(S = c("Conservative", "Moderate"), I = "Liberal")`.
#' The first value of a role is the one that the record writes.
#' By default, each role is its own value.
#' @section Arguments that set the model:
#'
#' The arguments after `...` are shortcuts that set the default model.
#' Give them by their full name.
#' A name that is not one of these arguments gives an error,
#' with the nearest argument name.
#' @inheritParams mark_is
#' @param attribute The name of the categorical node attribute that holds
#'   the state of each node.
#'   By default `"diffusion"`.
#'   See the section "Start state".
#'   For `as_diffusion()`, the attribute whose changes record the diffusion:
#'   the report has one column for each of its values.
#' @param transitions A table of the transitions between the values of
#'   `attribute`.
#'   This is not yet available.
#'   By default `NULL`, which uses the model that the other arguments set.
#' @param ... Not used. Any argument given here gives an error,
#'   with the nearest argument name.
#' @param seeds The nodes that start as infected, when `attribute` is not
#'   on the nodes.
#'   An index, a logical vector the length of the number of nodes,
#'   one or more node labels,
#'   or the name of a logical node attribute.
#'   By default the first node.
#' @param compartments A named vector or list that maps the roles
#'   S, E, I, R, and D onto the values of `attribute`.
#'   By default `NULL`, which maps each role onto itself.
#'   See the section "Start state".
#' @param contact A matrix or network that replaces ".data" with some
#'   other explicit contact network, e.g.
#'   `create_components(.data, membership = node_in_structural(.data))`.
#'   Can be of arbitrary complexity, but must of the same dimensions
#'   as .data.
#' @param direction Which ties expose a node, from the view of the node
#'   at risk.
#'   `"in"` (the default) counts the ties that infected nodes send to it,
#'   `"out"` counts the ties that it sends to infected nodes,
#'   and `"all"` counts both.
#'   On an undirected network, the three are the same.
#' @param thresholds A numeric vector indicating the thresholds
#'   each node has. By default 1.
#'   A single number means a generic threshold;
#'   for thresholds that vary among the population please use a vector
#'   the length of the number of nodes in the network.
#'   If 1 or larger, the threshold is interpreted as a simple count
#'   of the number of contacts/exposures sufficient for infection.
#'   If less than 1, the threshold is interpreted as complex,
#'   where the threshold concerns the proportion of contacts.
#' @param prevalence The proportion that global prevalence contributes
#'   to diffusion.
#'   That is, if prevalence is 0.5, then the current number of infections
#'   is multiplied by 0.5 and added
#'   "prevalence" is 0 by default, i.e. there is no global mechanism.
#'   Note that this is endogenously defined and is updated
#'   at the outset of each step.
#' @param background The probability that a susceptible node becomes
#'   infected at a step without any exposure, its background rate of
#'   infection or innovation.
#'   This is similar to the coefficient of innovation in the Bass model.
#'   By default 0, which means that a node is infected only through
#'   its contacts.
#' @param transmissibility The transmission rate probability,
#'   \eqn{\beta}.
#'   By default 1, which means any node for which the threshold is met
#'   or exceeded will become infected.
#'   Anything lower means a correspondingly lower probability of adoption,
#'   even when the threshold is met or exceeded.
#' @param recovery The probability those who are infected
#'   recover, \eqn{\gamma}.
#'   For example, if infected individuals take, on average,
#'   four days to recover, then \eqn{\gamma = 0.25}.
#'   By default 0, which means there is no recovery (i.e. an SI model).
#'   Anything higher results in an SIR model.
#' @param latency The inverse probability those who have been exposed
#'   become infectious (infected), \eqn{\sigma} or \eqn{\kappa}.
#'   For example, if exposed individuals take, on average,
#'   four days to become infectious, then \eqn{\sigma = 0.75} (1/1-0.75 = 1/0.25 = 4).
#'   By default 0, which means those exposed become immediately infectious (i.e. an SI model).
#'   Anything higher results in e.g. a SEI model.
#' @param waning The probability those who are recovered
#'   become susceptible again, \eqn{\xi}.
#'   For example, if recovered individuals take, on average,
#'   four days to lose their immunity, then \eqn{\xi = 0.25}.
#'   By default 0, which means any recovered individuals retain lifelong immunity (i.e. an SIR model).
#'   Anything higher results in e.g. a SIRS model.
#'   \eqn{\xi = 1} would mean there is no period of immunity, e.g. an SIS model.
#' @param fatality The probability those who are infected
#'   are removed from the network, \eqn{\alpha}.
#'   Note that fatality is distinct from a natural mortality rate.
#'   By default \eqn{\alpha = 0}, which means that there is no fatality.
#'   Where \eqn{\alpha > 0}, the nodal attribute 'active' will be
#'   added if it is not already present.
#'   A node that dies no longer infects others, and cannot be infected again.
#' @param immune A logical or numeric vector identifying nodes
#'   that begin the diffusion process as already recovered.
#'   This could be interpreted as those who are vaccinated or equivalent.
#'   Note however that a waning parameter will affect these nodes too.
#'   By default NULL, indicating that no nodes begin immune.
#' @param steps The number of steps forward in the diffusion to play,
#'   after the start.
#'   By default the number of nodes in the network.
#'   If `steps = Inf` then the diffusion process will continue until
#'   there are no new infections or all nodes are infected.
#' @family makes
#' @family models
#' @family diffusion
#' @name make_play
NULL

#' @rdname make_play
#' @param old_version This is included to maintain backward compatibility
#'   with the old version of this function, that would return a special
#'   object.
#'   The new version adds the diffusion event record as changes to the
#'   original network.
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   play_diffusion(smeg, seeds = 1, recovery = 0.2)
#'   # A start state from an attribute, in labels of its own
#'   smeg <- mutate_nodes(smeg, aware = rep(c("yes", "no", "no"), 5))
#'   play_diffusion(smeg, "aware", steps = 5,
#'                  compartments = c(S = "no", I = "yes"))
#' @export
play_diffusion <- function(.data,
                           attribute = "diffusion",
                           transitions = NULL,
                           steps,
                           ...,
                           seeds = 1,
                           compartments = NULL,
                           contact = NULL,
                           direction = c("in", "out", "all"),
                           prevalence = 0,
                           background = 0,
                           thresholds = 1,
                           transmissibility = 1,
                           latency = 0,
                           recovery = 0,
                           waning = 0,
                           fatality = 0,
                           immune = NULL,
                           old_version = FALSE) {
  choices <- setdiff(names(formals(play_diffusion)), c(".data", "..."))
  .check_dots_empty(list(...), choices, "play_diffusion")
  # R matches part of a name to the arguments before `...`,
  # as `trans` to `transitions`, so these names must be given in full too.
  called <- names(sys.call())
  partial <- setdiff(called[nzchar(called)], c(".data", choices))
  if(length(partial))
    .check_dots_empty(stats::setNames(as.list(partial), partial), choices,
                      "play_diffusion")
  seeds_given <- !missing(seeds)
  # A number or a logical vector in the second position is the old
  # positional `seeds`, before `attribute` took that position.
  if(is.numeric(attribute) || is.logical(attribute)){
    if(seeds_given)
      snet_abort("{.arg seeds} is given twice: by name and in the second position.")
    snet_warn(paste("Giving {.arg seeds} in the second position is deprecated.",
                    "Please use {.code seeds = } instead."))
    seeds <- attribute
    seeds_given <- TRUE
    attribute <- "diffusion"
  }
  if(!is.character(attribute) || length(attribute) != 1 || is.na(attribute))
    snet_abort("{.arg attribute} must be the name of one node attribute.")
  if(!is.null(transitions))
    snet_unavailable("A table of {.arg transitions} is not yet available.",
                     "Please use the other arguments to set the model.")
  direction <- match.arg(direction)
  if(old_version)
    snet_warn(paste("Please use the new version of this function (since v1.4.0), with new features.",
                    "The old version will no longer be maintained and is retained here only for backwards compatibility."))

  # A list of networks is a list of waves. Its first wave gives the nodes.
  waves <- is_list(.data)
  first <- if(waves) .data[[1]] else .data
  n <- net_nodes(first)
  if(missing(steps)) steps <- n
  if(is.character(thresholds)) thresholds <- node_attribute(first, thresholds)
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  if(all(thresholds <= 1) & !all(thresholds == 1))
    thresholds <- thresholds *
      .node_deg(first)

  # Start state ####
  begin <- .diffusion_start(first, attribute = attribute,
                            seeds = if(seeds_given) seeds else NULL,
                            compartments = compartments, immune = immune)
  comps <- begin$compartments
  .diffusion_roles_needed(comps, recovery = recovery, latency = latency,
                          fatality = fatality, waning = waning,
                          immune = begin$recovered)
  infected <- begin$infected
  latent <- begin$latent
  recovered <- begin$recovered
  dead <- begin$dead
  start <- begin$time
  time <- start

  # initialise events table
  events <- list(data.frame(t = rep(time, length(begin$seeds)),
                            nodes = begin$seeds, event = rep("I", length(begin$seeds)),
                            exposure = rep(NA, length(begin$seeds))))
  # The contact matrices are made once, before the loop.
  twomode <- is_twomode(first)
  complex <- is_complex(first)
  # Until the diffusion follows the time of the ties, a network that changes
  # is played on all its ties at once.
  if(!waves && is.null(contact) && (is_longitudinal(first) || is_dynamic(first)))
    snet_warn(paste("{.fn play_diffusion} does not yet follow the time of the ties,",
                    "so it uses all the ties of all the waves at once.",
                    "To play each wave in turn, use {.code to_waves()} first."))
  contacts <- if(waves) .data else list(if(is.null(contact)) .data else contact)
  contacts <- lapply(contacts, .exposure_matrix, inform = TRUE)
  sinit <- sum(.exposure_exposed(contacts[[1]], infected, direction, twomode))
  # initialise report table
  report <- list(data.frame(t = time,
                            n = n,
                            S = n - (length(latent) + length(infected) +
                                       length(recovered) + length(dead)),
                            s = sinit,
                            E = length(latent),
                            I_new = length(begin$seeds),
                            I = length(infected),
                            R = length(recovered)))
  last_new <- length(begin$seeds)

  repeat{ # At each time step:
    # update network, if necessary:
    # a wave stays in force until the next one, and the last after that
    mat <- contacts[[min(time - start + 1, length(contacts))]]
    # some who have already recovered may lose their immunity:
    waned <- recovered[stats::rbinom(length(recovered), 1, waning)==1]
    # some may recover:
    recovers <- infected[stats::rbinom(length(infected), 1, recovery)==1]
    # the recovered are no longer infected
    recovered <- c(recovered, recovers)
    infected <- setdiff(infected, recovered)
    # those for whom immunity has waned are no longer immune
    recovered <- setdiff(recovered, waned)
    # some infected, sadly, shake off this mortal coil
    deceased <- infected[stats::rbinom(length(infected), 1, fatality)==1]
    # and the dead no longer infect anyone
    if(length(deceased) > 0){
      infected <- setdiff(infected, deceased)
      dead <- c(dead, deceased)
    }

    # add any global/prevalence feedback
    new_prev <- mat + ((last_new - length(recovers)) * prevalence)
    if(!twomode & !complex) diag(new_prev) <- 0

    # at main infection stage, get currently exposed to infection:
    exposed <- .exposure_exposed(new_prev, infected, direction, twomode)
    # count exposures for each node:
    exposure <- .exposure_count(new_prev, infected, direction, twomode)
    # identify those nodes who are exposed at or above their threshold

    open_to_it <- which(exposure >= thresholds)
    newinf <- open_to_it[stats::rbinom(length(open_to_it), 1, transmissibility)==1]
    if(!is.null(recovery) & length(recovered)>0)
      newinf <- setdiff(newinf, recovered) # recovered can't be reinfected
    if(!is.null(latent) & length(latent)>0)
      newinf <- setdiff(newinf, latent) # latent already infected
    if(length(dead)>0) newinf <- setdiff(newinf, dead) # nor can the dead
    # some may adopt without any exposure, at the background rate
    if(background > 0){
      unaware <- setdiff(seq_len(n), c(infected, latent, recovered, dead, newinf))
      newinf <- c(newinf,
                  unaware[stats::rbinom(length(unaware), 1, background)==1])
    }
    if(is.infinite(steps) & length(newinf)==0 & length(latent)==0) break # if no new infections we can stop

    # new list of infected
    latent <- c(latent, newinf)
    infectious <- latent[stats::rbinom(length(latent), 1, latency)==0]
    latent <- setdiff(latent, infectious)
    newinf <- setdiff(newinf, infectious)
    infected <- c(infected, infectious)
    # tick time
    time <- time+1

    # Update events table ####
    # record new infections
    if(!is.null(infectious) & length(infectious)>0)
      events[[length(events)+1]] <- data.frame(t = time, nodes = infectious, event = "I",
                                               exposure = exposure[infectious])
    # record exposures
    if(!is.null(newinf) & length(newinf)>0)
      events[[length(events)+1]] <- data.frame(t = time, nodes = newinf, event = "E",
                                               exposure = exposure[newinf])
    # record recoveries
    if(!is.null(recovers) & length(recovers)>0)
      events[[length(events)+1]] <- data.frame(t = time, nodes = recovers, event = "R",
                                               exposure = NA)
    # record wanings
    if(!is.null(waned) & length(waned)>0)
      events[[length(events)+1]] <- data.frame(t = time, nodes = waned, event = "S",
                                               exposure = NA)
    # record fatalities
    if(!is.null(fatality) & length(deceased)>0)
      events[[length(events)+1]] <- data.frame(t = time, nodes = deceased, event = "D",
                                               exposure = NA)
    # Update report table ####
    last_new <- length(infectious)
    report[[length(report)+1]] <- data.frame(t = time,
                                             n = n,
                                             S = n - (length(latent) + length(infected) +
                                                        length(recovered) + length(dead)),
                                             s = sum(exposed),
                                             E = length(latent),
                                             I_new = length(infectious),
                                             I = length(infected),
                                             R = length(recovered))

    if(is.infinite(steps) & length(infected)==n) break
    if(time - start == steps) break
  }
  events <- do.call(rbind, events)
  report <- do.call(rbind, report)
  if(old_version){
    make_diff_model(events, report, .data)
  } else {
    if(waves) lapply(.data, .diffusion_record, events = events, begin = begin,
                     attribute = attribute) else
      .diffusion_record(.data, events, begin, attribute)
  }
}

# contagion_function = attrib ~ 1 + prevalence + threshold + contact + equivalence

# Start state ####

# Finds the start of a diffusion on `.data`, in one of three cases:
# a history of `attribute` in the changes (1), the values of `attribute` on the
# nodes (2), or no `attribute`, where `seeds` and `immune` give the start (3).
# Returns the nodes in each role at the start, the start time, the seeds that
# the record writes at the start (case 3 only), and the compartments as a list.
.diffusion_start <- function(.data, attribute = "diffusion", seeds = NULL,
                             compartments = NULL, immune = NULL){
  n <- net_nodes(.data)
  # a stocnet can keep its compartments, as bind_changes() does
  if(is.null(compartments) && inherits(.data, "stocnet"))
    compartments <- .data$info$compartments
  has_attr <- attribute %in% net_node_attributes(.data)
  history <- NULL
  if(is_changing(.data)){
    history <- as_changelist(.data)
    history <- history[history$var == attribute, , drop = FALSE]
    if(nrow(history) == 0) history <- NULL
  }
  if(!is.null(history) || has_attr){
    case <- if(!is.null(history)) "a history" else "values on the nodes"
    if(!is.null(seeds))
      snet_abort(paste("{.arg seeds} cannot be used here, because {.var {attribute}}",
                       "already gives the start state ({case}).",
                       "Please change the attribute instead, or use another {.arg attribute}."))
    if(!is.null(immune))
      snet_abort(paste("{.arg immune} cannot be used here, because {.var {attribute}}",
                       "already gives the start state ({case}).",
                       "Please give the immune nodes the recovered value of the attribute instead."))
    state <- if(has_attr) node_attribute(.data, attribute) else
      rep(NA, n)
    if(is.factor(state)) state <- as.character(state)
    time <- .network_start(.data)
    if(!is.null(history)){
      # the last value of each node in the history is its start state
      values <- .unlist_values(history$value)
      ord <- order(history$time)
      last <- !duplicated(history$node[ord], fromLast = TRUE)
      state <- .as_state(state, values)
      state[history$node[ord][last]] <- values[ord][last]
      time <- max(history$time)
    }
    comps <- .diffusion_compartments(compartments, state)
    roles <- .diffusion_roles(state, comps, attribute)
    list(infected = which(roles == "I"), latent = which(roles == "E"),
         recovered = which(roles == "R"), dead = which(roles == "D"),
         seeds = integer(0), time = time, compartments = comps,
         case = if(!is.null(history)) 1 else 2, given = !is.null(compartments))
  } else {
    if(is.null(seeds)) seeds <- 1
    seeds <- .resolve_nodes(.data, seeds, "seeds")
    if(!is.null(immune)) immune <- .resolve_nodes(.data, immune, "immune")
    comps <- .diffusion_compartments(compartments, character(0))
    list(infected = seeds, latent = NULL, recovered = immune, dead = integer(0),
         seeds = seeds, time = .network_start(.data), compartments = comps,
         case = 3, given = !is.null(compartments))
  }
}

# The first time of a network is the earliest time of its ties, or 0.
.network_start <- function(.data){
  if("time" %in% net_tie_attributes(.data)){
    times <- tie_attribute(.data, "time")
    if(is.numeric(times) && any(!is.na(times)))
      return(min(times, na.rm = TRUE))
  }
  0
}

# Reads seeds or immune nodes as indices: an index, a logical vector, labels,
# or the name of a logical node attribute.
.resolve_nodes <- function(.data, nodes, arg = "seeds"){
  if(is.logical(nodes)) return(which(nodes))
  if(is.character(nodes)){
    labels <- node_labels(.data)
    if(length(nodes) == 1 && nodes %in% net_node_attributes(.data)){
      marks <- node_attribute(.data, nodes)
      if(is.logical(marks)){
        if(nodes %in% labels)
          snet_warn(paste("{.val {nodes}} is both a node attribute and a node label.",
                          "The attribute is used for {.arg {arg}}."))
        return(which(marks))
      }
    }
    found <- nodes %in% labels
    if(!all(found))
      snet_abort(paste("{.arg {arg}} must be node labels or the name of a",
                       "logical node attribute, but {.val {nodes[!found]}}",
                       "{?is/are} neither."))
    return(which(labels %in% nodes))
  }
  nodes
}

# Gives the compartments as a named list, checked for the roles it uses.
.diffusion_compartments <- function(compartments, state){
  if(is.null(compartments)){
    if(is.logical(state)) return(list(S = FALSE, I = TRUE))
    roles <- c("S", "E", "I", "R", "D")
    return(stats::setNames(as.list(roles), roles))
  }
  comps <- as.list(compartments)
  if(is.null(names(comps)) || any(names(comps) == ""))
    snet_abort("{.arg compartments} must name the role of each value, as in {.code c(S = \"unaware\", I = \"adopter\")}.")
  unknown <- setdiff(names(comps), c("S", "E", "I", "R", "D"))
  if(length(unknown))
    snet_abort(paste("The roles in {.arg compartments} must be S, E, I, R, or D,",
                     "but {.val {unknown}} {?is/are} not."))
  if(anyDuplicated(names(comps)))
    snet_abort("Each role can be named only once in {.arg compartments}.")
  values <- unlist(comps, use.names = FALSE)
  if(anyDuplicated(values))
    snet_abort("A value can have only one role in {.arg compartments}.")
  comps
}

# Maps the values of the state onto roles. Missing values start as S.
.diffusion_roles <- function(state, comps, attribute){
  roles <- rep(NA_character_, length(state))
  for(role in names(comps))
    roles[!is.na(state) & state %in% comps[[role]]] <- role
  unknown <- unique(state[!is.na(state) & is.na(roles)])
  if(length(unknown))
    snet_abort(paste("{.var {attribute}} holds the value{?s} {.val {unknown}},",
                     "which {?has/have} no role in {.arg compartments}.",
                     "Please give each value a role, as in",
                     "{.code compartments = c(S = \"unaware\", I = \"adopter\")}."))
  missing_state <- sum(is.na(state))
  if(missing_state > 0){
    if(!"S" %in% names(comps))
      snet_abort("{.arg compartments} needs the role {.val S} for the nodes whose state is missing.")
    snet_info("{missing_state} node{?s} with a missing state start{?s/} as susceptible.")
    roles[is.na(state)] <- "S"
  }
  roles
}

# Stops where an argument needs a role that the compartments do not define.
.diffusion_roles_needed <- function(comps, recovery = 0, latency = 0,
                                    fatality = 0, waning = 0, immune = NULL){
  needs <- c(S = TRUE, I = TRUE,
             R = any(recovery > 0) || length(immune) > 0,
             E = any(latency > 0),
             D = any(fatality > 0))
  why <- c(S = "every model", I = "every model",
           R = "{.arg recovery} or {.arg immune}", E = "{.arg latency}",
           D = "{.arg fatality}")
  for(role in names(needs)[needs]){
    if(!role %in% names(comps))
      snet_abort(paste0("The role {.val ", role, "} is needed for ", why[[role]],
                        ", but it is not in {.arg compartments}."))
  }
  invisible(comps)
}

# Writes the events of a diffusion onto the network as changes.
.diffusion_record <- function(.data, events, begin, attribute){
  comps <- begin$compartments
  if(begin$case == 3){
    # all nodes start in S, except the immune, which start in R
    start <- rep(comps$S[1], net_nodes(.data))
    if(length(begin$recovered)) start[begin$recovered] <- comps$R[1]
    .data <- add_node_attribute(.data, attribute, start)
  }
  # a stocnet keeps the compartments that were given, so that others can read them
  if(inherits(.data, "stocnet") && isTRUE(begin$given))
    .data$info$compartments <- comps
  died <- events$event == "D"
  states <- events[!died, , drop = FALSE]
  values <- unlist(lapply(states$event, function(r) comps[[r]][1]))
  if(is.null(values)) values <- rep(comps$S[1], 0)
  changes <- data.frame(time = states$t, node = as.integer(states$nodes),
                        var = rep(attribute, nrow(states)))
  changes$value <- values
  if(nrow(changes) > 0) .data <- bind_changes(.data, changes)
  if(any(died)){
    if(!"active" %in% net_node_attributes(.data))
      .data <- mutate_nodes(.data, active = TRUE)
    deaths <- data.frame(time = events$t[died], node = as.integer(events$nodes[died]),
                         var = "active", value = FALSE)
    .data <- bind_changes(.data, deaths)
  }
  .data
}

.unlist_values <- function(v){
  if(is.list(v) && all(lengths(v) == 1L)) unlist(v, use.names = FALSE) else v
}

# Gives the state the type of the values of its history.
.as_state <- function(state, values){
  if(all(is.na(state))){
    out <- rep(values[1], length(state))
    out[] <- NA
    return(out)
  }
  if(is.character(values) && !is.character(state)) state <- as.character(state)
  state
}

# Stops where `...` holds anything, and suggests the nearest argument names.
# The error names the function that was called, not this one.
.check_dots_empty <- function(dots, choices, fn, call = parent.frame()){
  if(!length(dots)) return(invisible())
  given <- names(dots)
  if(is.null(given)) given <- rep("", length(dots))
  if(any(given == ""))
    snet_abort(paste0("{.fn ", fn, "} takes the arguments after {.arg steps} ",
                      "only by their full name."), .envir = call)
  hints <- vapply(given, function(x){
    near <- choices[startsWith(choices, x)]
    if(!length(near)){
      dist <- utils::adist(x, choices, ignore.case = TRUE)[1,]
      if(min(dist) <= max(2, nchar(x) %/% 3)) near <- choices[dist == min(dist)]
    }
    out <- paste0("{.arg ", x, "} is not an argument of {.fn ", fn, "}.")
    if(length(near)) out <- paste0(out, " Did you mean ",
                                   paste0("{.arg ", near, "}", collapse = " or "), "?")
    out
  }, character(1))
  snet_abort(paste(hints, collapse = " "), .envir = call)
}

# Exposure ####

# The contact matrix for exposure, with missing ties as absent ties.
# A cognitive social structure is read as its locally aggregated structure,
# where a tie exists if both of its ends report it, as in `{netrics}`.
.exposure_matrix <- function(.data, inform = FALSE){
  if(is_cognitive(.data)){
    if(inform) snet_info("Combining the perceivers' reports of this cognitive",
                         "social structure into its locally aggregated structure,",
                         "where a tie exists if both of its ends report it.")
    .data <- to_aggregated(.data, over = "by", rule = "min", reporters = "both")
  }
  out <- as_matrix(.data)
  missing_ties <- sum(is.na(out))
  if(missing_ties > 0){
    if(inform) snet_info("{missing_ties} missing tie{?s} count{?s/} as absent for exposure.")
    out[is.na(out)] <- 0
  }
  out
}

# The shared exposure kernel. For each node, it counts the marked nodes that
# expose it, along ties in the `direction` from the view of the node at risk:
# "in" counts the ties that marked nodes send it, "out" the ties that it sends
# them, and "all" both. Unweighted ties count each marked node once. Weighted or
# signed ties sum the weights of the ties from the marked nodes.
# A marked node is not exposed. `mat` is a one-mode adjacency matrix.
.exposure_kernel <- function(mat, mark, direction = "in", weighted = NULL){
  n <- nrow(mat)
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(0, n)
  if(!length(mark)) return(out)
  if(is.null(weighted)) weighted <- !all(mat %in% c(0, 1))
  if(direction == "out") mat <- t(mat) else if(direction == "all"){
    if(weighted){
      if(!isSymmetric(unname(mat))) mat <- mat + t(mat)
    } else mat <- (mat != 0) | (t(mat) != 0)
  }
  if(weighted){
    mk <- rep(FALSE, n)
    mk[mark] <- TRUE
    out <- colSums(mat[mk, , drop = FALSE])
    out[mk] <- 0
  } else {
    out <- colSums(mat[mark, , drop = FALSE] != 0)
    out[mark] <- 0
  }
  unname(out)
}

# The exposure of each node to the marked nodes, as the diffusion counts it.
.exposure_count <- function(mat, mark, direction = "in", twomode = FALSE){
  if(twomode) return(c(.node_exposure_twomode(as_tidygraph(mat), mark)))
  .exposure_kernel(mat, mark, direction)
}

# Whether each node is exposed to the marked nodes.
.exposure_exposed <- function(mat, mark, direction = "in", twomode = FALSE){
  if(twomode) return(c(.node_is_exposed_twomode(as_tidygraph(mat), mark)))
  .exposure_kernel(mat, mark, direction, weighted = FALSE) > 0
}

.node_exposure <- function(.data, mark, time = 0, direction = "in"){
  .data <- expect_nodes(.data)
  if(missing(mark)){
    if(inherits(.data, "diff_model")){
      mark <- .node_is_infected(.data, time = time)
      .data <- attr(.data, "network")
    } else {
      mark <- .node_is_infected(.data, time = time)
    }
  }
  .data <- manynet::as_tidygraph(.data)
  if(manynet::is_twomode(.data))
    return(.node_exposure_twomode(.data, mark))
  out <- .exposure_kernel(.exposure_matrix(.data), mark, direction,
                          weighted = manynet::is_weighted(.data) ||
                            manynet::is_signed(.data))
  make_node_measure(out, .data)
}

# Two-mode exposure goes through the shared events, as before the kernel.
.node_exposure_twomode <- function(.data, mark){
  if(manynet::is_weighted(.data) || manynet::is_signed(.data)){
    if(is.numeric(mark)){
      mk <- rep(FALSE, manynet::net_nodes(.data))
      mk[mark] <- TRUE
    } else mk <- mark
    out <- manynet::as_matrix(.data)
    out <- colSums(out * matrix(mk, nrow(out), ncol(out)) *
                     matrix(!mk, nrow(out), ncol(out), byrow = TRUE))
  } else {
    if(is.logical(mark)) mark <- which(mark)
    if(!length(mark)) return(make_node_measure(rep(0, manynet::net_nodes(.data)), .data))
    if(mark[1]>manynet::mode_nodes(.data)[1]){
      dat <- manynet::to_mode2(.data)
      mark <- mark - manynet::mode_nodes(.data)[1]
      contacts <- unlist(lapply(igraph::neighborhood(dat, nodes = mark, mode = "out"),
                                function(x) setdiff(x, mark)))
      contacts <- contacts + manynet::mode_nodes(.data)[1]
    } else {
      dat <- manynet::to_mode1(.data)
      contacts <- unlist(lapply(igraph::neighborhood(dat),
                                function(x) setdiff(x, mark)))
      contacts <- contacts + manynet::mode_nodes(.data)[2]
    }
    # count exposures for each node:
    tabcontact <- table(contacts)
    out <- rep(0, manynet::net_nodes(.data))
    out[as.numeric(names(tabcontact))] <- unname(tabcontact)
  }
  make_node_measure(out, .data)
}

.node_is_latent <- function(.data, time = 0){
  if(is_changing(.data)){
    t <- time
    latent <- as_changelist(.data) |> 
      dplyr::filter(time <= t & value %in% c("E", "I")) |>
      dplyr::group_by(node) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::filter(n == 1 & value == "E")
    if (is_labelled(.data)) {
      out <- seq_len(net_nodes(.data)) %in% latent$node
      names(out) <- node_labels(.data)
    } else {
      out <- seq_len(net_nodes(.data)) %in% latent$node
    }
    make_node_mark(out, .data)
  } else if(inherits(.data, "diff_model")){
    latent <- summary(.data) |>
      dplyr::filter(t <= time & event %in% c("E", "I")) |>
      group_by(nodes) |>
      mutate(n = dplyr::n()) |>
      filter(n == 1 & event == "E")
    net <- attr(.data, "network")
    if (is_labelled(net)) {
      out <- seq_len(net_nodes(net)) %in% latent$nodes
      names(out) <- node_labels(net)
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

.node_is_infected <- function(.data, time = 0) {
  if(is_changing(.data)){
    t <- time
    infected <- as_changelist(.data) |> 
      dplyr::filter(time <= t & value %in% c("I", "R")) |>
      dplyr::group_by(node) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::filter(n == 1 & value == "I")
    if (is_labelled(.data)) {
      out <- seq_len(net_nodes(.data)) %in% infected$node
      names(out) <- node_labels(.data)
    } else {
      out <- seq_len(net_nodes(.data)) %in% infected$node
    }
    make_node_mark(out, .data)
  } else if(inherits(.data, "diff_model")){
    infected <- summary(.data) |> 
      dplyr::filter(t <= time & event %in% c("I", "R")) |>
      group_by(nodes) |>
      mutate(n = dplyr::n()) |>
      filter(n == 1 & event == "I")
    net <- attr(.data, "network")
    if (is_labelled(net)) {
      out <- seq_len(net_nodes(net)) %in% infected$nodes
      names(out) <- node_labels(net)
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

.node_is_recovered <- function(.data, time = 0){
  if(is_changing(.data)){
    t <- time
    recovered <- as_changelist(.data) |> 
      dplyr::filter(time <= t & value %in% "R") |>
      dplyr::group_by(node) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::filter(n == 1 & value == "R")
    if (is_labelled(.data)) {
      out <- seq_len(net_nodes(.data)) %in% recovered$node
      names(out) <- node_labels(.data)
    } else {
      out <- seq_len(net_nodes(.data)) %in% recovered$node
    }
    make_node_mark(out, .data)
  } else if(inherits(.data, "diff_model")){
    recovered <- summary(.data) |> 
      dplyr::filter(t <= time & event == "R") |>
      group_by(nodes) |>
      mutate(n = dplyr::n()) |>
      filter(n == 1)
    net <- attr(.data, "network")
    if (is_labelled(net)) {
      out <- seq_len(net_nodes(net)) %in% recovered$nodes
      names(out) <- node_labels(net)
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

.node_is_exposed <- function(.data, mark, time = 0, direction = "in"){
  if (missing(mark)){
    if(is_changing(.data)){
      t <- time
      return(make_node_mark(.node_exposure(.data, time = t, 
                                           direction = direction)>0, .data))
    } else if(inherits(.data, "diff_model")){
      mark <- summary(.data) |> 
        dplyr::filter(t == 0 & event == "I") |> 
        dplyr::select(nodes) |> unlist()
      .data <- attr(.data, "network")
    }    
  }
  if(is_twomode(.data)) return(.node_is_exposed_twomode(.data, mark))
  out <- .exposure_kernel(.exposure_matrix(.data), mark, direction,
                          weighted = FALSE) > 0
  make_node_mark(out, .data)
}

# In a two-mode network, a node is exposed by the nodes it shares a tie with.
.node_is_exposed_twomode <- function(.data, mark){
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(F, manynet::net_nodes(.data))
  out[unique(setdiff(unlist(igraph::neighborhood(as_igraph(.data), nodes = mark)),
                     mark))] <- TRUE
  make_node_mark(out, .data)
}
