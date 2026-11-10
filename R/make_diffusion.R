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
#' @section Transitions:
#'
#' A diffusion moves the nodes between the values of `attribute`.
#' `transitions` gives these moves as a data frame, with one row for each
#' transition and these columns:
#'
#' - `from` and `to`: the value that a node leaves, and the value it goes to.
#' - `rate`: the hazard per step, 0 or more (`Inf` is certain).
#'   A number, a number for each node, the name of a numeric node attribute,
#'   or a function of `state`, `exposure`, `time` and `.data` that returns
#'   a hazard for each node at each step.
#'   A function replaces the exposure model of its row.
#' - `source`: the value that causes the transition, or `NA` (the default)
#'   for a spontaneous transition.
#'   Several values, as in `"E+I"` or as a list column, count together.
#' - `exposure`: how the contacts in `source` give the hazard.
#'   `"threshold"` (the default) gives `rate` to each node with at least
#'   `threshold` such contacts.
#'   `"cascade"` gives each such contact its own chance, so the hazard is
#'   `rate` times the number of contacts.
#'   `"voter"` gives `rate` times the share of contacts in `source`.
#' - `threshold`: the number of contacts in `source` (1 or more), or their
#'   share (less than 1), that a node needs. By default 1.
#'   A number, a number for each node, or the name of a node attribute.
#' - `prevalence`: how much the share of all nodes in `source` lowers
#'   the threshold, as in `threshold * (1 - prevalence * share)`.
#'   By default 0.
#'
#' A node moves with probability \eqn{1 - e^{-\sum h}} over the rows that
#' it can take, and its destination is drawn in proportion to their hazards.
#' Rows with the same `from` and `to` add their hazards.
#' In each step, the spontaneous transitions (rows with no `source`) occur
#' first, from the state at the start of the step.
#' The other transitions then count only the contacts that are still in
#' `source`, so a node that recovers or dies in a step does not infect others
#' in that step.
#' Each value that the attribute holds must be in at least one row.
#' For a value that never changes, add a row with `rate = 0`.
#'
#' Without `transitions`, the arguments after `...` give the table.
#' They are chances per step, and each chance \eqn{p} becomes the hazard
#' \eqn{-\log(1 - p)}.
#' With `transitions`, these arguments are ignored, with a warning.
#'
#' For example, the voter model on two values:
#' `data.frame(from = c("Liberal", "Conservative"), to = c("Conservative", "Liberal"),`
#' `rate = 1, source = c("Conservative", "Liberal"), exposure = "voter")`.
#' @section Time and composition:
#'
#' A diffusion follows the time of the ties.
#' On a panel, each wave stays in force until the next wave,
#' and the last wave stays in force after the end.
#' A list of networks with the same nodes is a panel, with one wave for each step.
#' On an event stream, a tie can transmit only once it occurs,
#' and an interval tie only while it is active.
#' A network whose ties or changes are dated plays one step for each day.
#' By default, data that change in time run until one step after their last
#' moment, and a list of networks runs one step for each network.
#'
#' In a two-mode network, a state that is known on one mode only
#' (on the nodes, in their history, or from `seeds` and `immune`)
#' diffuses on that mode only, through co-affiliation:
#' a node is exposed by the infected nodes with which it shares a node of the
#' other mode, while that node is active.
#' The nodes of the other mode have no state.
#' A state on both modes diffuses along the ties between the two modes.
#'
#' A node with `active = FALSE` is not at risk and does not transmit.
#' Changes to `active` take effect at their time.
#' Use `active = FALSE` to exclude a node whose state is not known.
#' A node that dies (D) becomes inactive in the record.
#'
#' A 'stocnet' keeps the table of transitions in `info$transitions`.
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
#' @param transitions A data frame of the transitions between the values of
#'   `attribute`. See the section "Transitions".
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
#'   A panel, an event stream, or a list of networks is followed in time,
#'   as ".data" is.
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
#' @param prevalence How much the share of infected nodes in the whole
#'   network lowers each node's threshold, from 0 to 1.
#'   The threshold becomes `thresholds * (1 - prevalence * share)`,
#'   where `share` is the share of the live nodes that are infected
#'   at the start of the step.
#'   By default 0, which means that there is no global mechanism.
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
#' @param latency The probability that those who have been exposed
#'   stay exposed (not yet infectious) at a step, \eqn{\sigma} or \eqn{\kappa}.
#'   An exposed node becomes infectious at a later step.
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
#'   By default the number of nodes in the network,
#'   or the period of data that change in time.
#'   If `steps = Inf` then the diffusion process will continue until
#'   a step in which no node changes state,
#'   after the last wave and the last change to `active`.
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
#'   # A spontaneous transition, from a table of transitions
#'   play_diffusion(ison_lawfirm, "status", steps = 5,
#'                  transitions = data.frame(from = "associate", to = "partner",
#'                                           rate = 0.1))
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
  direction <- match.arg(direction)
  if(old_version)
    snet_warn(paste("Please use the new version of this function (since v1.4.0), with new features.",
                    "The old version will no longer be maintained and is retained here only for backwards compatibility."))

  # A list of networks with the same nodes is a panel, with one wave for each
  # step. Its first wave gives the nodes.
  waves <- is_list(.data)
  if(waves && length(unique(vapply(.data, net_nodes, numeric(1)))) > 1)
    snet_abort(paste("{.fn play_diffusion} reads a list of networks as a panel,",
                     "so every network in the list must have the same nodes."))
  first <- if(waves) .data[[1]] else .data
  n <- net_nodes(first)
  steps_given <- !missing(steps)

  # The model ####
  shortcuts <- c("prevalence", "background", "thresholds", "transmissibility",
                 "latency", "recovery", "waning", "fatality")
  env <- environment()
  table_given <- !is.null(transitions)
  if(table_given){
    supplied <- shortcuts[!vapply(shortcuts, function(a)
      eval(call("missing", as.name(a)), envir = env), logical(1))]
    if(length(supplied))
      snet_warn("{.arg {supplied}} {?is/are} ignored, because {.arg transitions} sets the model.")
    if(old_version)
      snet_abort("{.arg old_version} works only with the shortcuts, not with {.arg transitions}.")
    known <- c(if(!is.null(compartments)) as.character(unlist(compartments)),
               if(attribute %in% net_node_attributes(first))
                 as.character(node_attribute(first, attribute)))
    transitions <- make_transitions(transitions, values = known)
  }

  # In a two-mode network, a state on one mode only diffuses on that mode,
  # through the nodes of the other mode that they share.
  affiliation <- .diffusion_affiliation(first, attribute,
                                        if(seeds_given) seeds else 1, immune)

  # Start state ####
  begin <- .diffusion_start(first, attribute = attribute,
                            seeds = if(seeds_given) seeds else NULL,
                            compartments = compartments, immune = immune,
                            values = if(table_given) .transition_values(transitions),
                            exclude = affiliation$exclude)
  comps <- begin$compartments
  if(!table_given){
    .diffusion_roles_needed(comps, recovery = recovery, latency = latency,
                            fatality = fatality, waning = waning,
                            immune = begin$recovered)
    transitions <- make_transitions(compartments = comps,
                                    transmissibility = transmissibility,
                                    recovery = recovery, latency = latency,
                                    waning = waning, fatality = fatality,
                                    background = background,
                                    thresholds = thresholds,
                                    prevalence = prevalence)
    begin$dead_value <- if(any(fatality > 0)) comps$D[1] else NULL
  } else {
    .transitions_cover(transitions, begin$state, attribute)
    begin$dead_value <- if(isTRUE(begin$given) && "D" %in% names(comps))
      as.character(comps$D[1]) else NULL
  }
  state <- as.character(begin$state)
  start <- begin$time
  time <- start

  # The contact matrices are made once, before the loop:
  # one for each wave of a panel, or for each moment of an event stream.
  twomode <- is_twomode(first)
  timeline <- .diffusion_contacts(if(waves) .data else
    if(is.null(contact)) .data else contact, start,
    if(steps_given) steps else Inf)
  # Composition: an inactive node is not at risk and does not transmit.
  presence <- .diffusion_presence(first, start)
  degree <- NULL
  if(!is.null(affiliation)){
    timeline <- .coaffiliation_timeline(timeline, presence, affiliation$mode,
                                        start)
    twomode <- FALSE
    degree <- .exposure_degree(timeline$contacts[[1]], "all")
  }
  contacts <- timeline$contacts
  engine <- .transitions_engine(transitions, first, n, contacts, direction,
                                twomode, degree)
  died <- if(is.null(affiliation)) rep(FALSE, n) else affiliation$exclude
  # Data that change in time set the period, and the number of nodes otherwise.
  if(!steps_given)
    steps <- .diffusion_period(timeline, presence, first, attribute, start, n)

  # initialise events table: the seeds at the start
  seeded <- begin$seeds
  events <- list(data.frame(t = rep(time, length(seeded)), nodes = seeded,
                            event = state[seeded],
                            exposure = rep(NA, length(seeded))))
  report <- NULL
  if(old_version){
    roles <- .diffusion_inverse(comps)
    report <- list(.diffusion_old_report(time, state, roles, contacts[[1]],
                                         direction, twomode, length(seeded)))
  }

  repeat{ # At each time step:
    # a wave stays in force until the next one, and the last after that
    wave <- .diffusion_wave(timeline, time, start)
    active <- .diffusion_active(presence, time) & !died
    step <- .transitions_step(engine, state, wave, time, begin, active)
    # with no move, stop only when no later wave or change of `active` can
    # start one again
    if(is.infinite(steps) && length(step$moves) == 0 &&
       wave == length(contacts) && !isTRUE(presence$last > time)) break
    time <- time + 1
    if(length(step$moves)){
      events[[length(events) + 1]] <- data.frame(t = time, nodes = step$moves,
                                                 event = step$to,
                                                 exposure = step$exposure)
      state[step$moves] <- step$to
      # feedback: a node that moves to the D value becomes inactive
      if(!is.null(begin$dead_value))
        died[step$moves[step$to == as.character(begin$dead_value)]] <- TRUE
    }
    if(old_version)
      report[[length(report) + 1]] <-
        .diffusion_old_report(time, state, roles, contacts[[wave]], direction,
                              twomode, sum(step$to == comps$I[1]))
    if(time - start == steps) break
  }
  events <- do.call(rbind, events)
  if(old_version){
    events$event <- unname(roles[as.character(events$event)])
    make_diff_model(events, do.call(rbind, report), .data)
  } else {
    if(waves) lapply(.data, .diffusion_record, events = events, begin = begin,
                     attribute = attribute, transitions = transitions) else
      .diffusion_record(.data, events, begin, attribute, transitions)
  }
}

# contagion_function = attrib ~ 1 + prevalence + threshold + contact + equivalence

# Transitions ####

# The table of transitions that the diffusion engine runs on, with one row
# for each transition: `from`, `to`, `rate` (a hazard per step), `source`,
# `exposure`, `threshold` and `prevalence`.
# Given a data frame, it checks the table and fills the defaults.
# `values` are the other known values of the attribute, which decide
# whether a text source such as "E+I" is split.
# Given no table, it compiles the shortcuts of play_diffusion() into a table,
# in the first value of each role in `compartments`.
make_transitions <- function(transitions = NULL, values = NULL,
                             compartments = NULL, transmissibility = 1,
                             recovery = 0, latency = 0, waning = 0,
                             fatality = 0, background = 0, thresholds = 1,
                             prevalence = 0){
  if(is.null(transitions)){
    if(is.null(compartments)) compartments <- .diffusion_compartments(NULL, character(0))
    role <- function(r) as.character(compartments[[r]][1])
    infect_to <- if(any(latency > 0)) role("E") else role("I")
    rows <- list(list(from = role("S"), to = infect_to,
                      rate = .chance_hazard(transmissibility, "transmissibility"),
                      source = role("I"), exposure = "threshold",
                      threshold = thresholds, prevalence = prevalence))
    add <- function(chance, arg, from, to)
      if(any(chance > 0)) rows[[length(rows) + 1]] <<-
        list(from = role(from), to = to, rate = .chance_hazard(chance, arg),
             source = NULL, exposure = NA_character_, threshold = NA,
             prevalence = NA_real_)
    add(background, "background", "S", infect_to)
    if(any(latency > 0)) add(1 - latency, "latency", "E", role("I"))
    add(recovery, "recovery", "I", role("R"))
    add(fatality, "fatality", "I", role("D"))
    add(waning, "waning", "R", role("S"))
    out <- dplyr::tibble(from = vapply(rows, `[[`, "", "from"),
                         to = vapply(rows, `[[`, "", "to"),
                         rate = lapply(rows, `[[`, "rate"),
                         source = lapply(rows, `[[`, "source"),
                         exposure = vapply(rows, `[[`, "", "exposure"),
                         threshold = lapply(rows, `[[`, "threshold"),
                         prevalence = vapply(rows, function(x) as.numeric(x$prevalence), 1))
    class(out) <- c("diffusionTransitions", class(out))
    return(out)
  }
  if(inherits(transitions, "diffusionTransitions")) return(transitions)
  if(!is.data.frame(transitions))
    snet_abort("{.arg transitions} must be a data frame, with one row for each transition.")
  columns <- c("from", "to", "rate", "source", "exposure", "threshold", "prevalence")
  unknown <- setdiff(names(transitions), columns)
  if(length(unknown))
    snet_abort(paste("{.arg transitions} can only have the columns {.val {columns}},",
                     "but {.val {unknown}} {?is/are} not one of them."))
  needed <- setdiff(c("from", "to", "rate"), names(transitions))
  if(length(needed))
    snet_abort("{.arg transitions} needs the column{?s} {.val {needed}}.")
  k <- nrow(transitions)
  if(k == 0) snet_abort("{.arg transitions} needs at least one row.")
  from <- as.character(transitions$from)
  to <- as.character(transitions$to)
  if(anyNA(from) || anyNA(to))
    snet_abort("{.arg transitions} cannot have a missing {.var from} or {.var to}.")
  as_rows <- function(x) if(is.list(x)) x else as.list(x)
  # rate: a hazard, the name of a node attribute, or a function
  rate <- as_rows(transitions$rate)
  for(r in seq_len(k)){
    x <- rate[[r]]
    ok <- is.function(x) || (is.character(x) && length(x) == 1 && !is.na(x)) ||
      (is.numeric(x) && length(x) > 0 && !anyNA(x) && all(x >= 0))
    if(!ok)
      snet_abort(paste("The {.var rate} in row {r} of {.arg transitions} must be",
                       "a hazard of 0 or more, the name of a numeric node attribute,",
                       "or a function."))
  }
  # source: NULL for a spontaneous transition, or one or more values
  source <- if("source" %in% names(transitions)) as_rows(transitions$source) else
    vector("list", k)
  known <- unique(c(from, to, values,
                    unlist(lapply(source, function(x)
                      if(length(x) > 1) as.character(x)))))
  source <- lapply(source, .transition_source, known = known)
  spontaneous <- vapply(source, is.null, logical(1))
  exposure <- if("exposure" %in% names(transitions))
    as.character(transitions$exposure) else rep(NA_character_, k)
  exposure[is.na(exposure) & !spontaneous] <- "threshold"
  exposure[spontaneous] <- NA_character_
  models <- c("threshold", "cascade", "voter")
  wrong <- setdiff(exposure[!spontaneous], models)
  if(length(wrong))
    snet_abort(paste("The {.var exposure} of a transition must be {.or {.val {models}}},",
                     "but {.val {wrong}} {?is/are} not."))
  threshold <- if("threshold" %in% names(transitions))
    as_rows(transitions$threshold) else as.list(rep(1, k))
  threshold <- lapply(threshold, function(x)
    if(length(x) == 0 || all(is.na(x))) 1 else x)
  threshold[spontaneous] <- list(NA)
  prevalence <- if("prevalence" %in% names(transitions))
    as.numeric(transitions$prevalence) else rep(0, k)
  prevalence[is.na(prevalence)] <- 0
  if(any(prevalence < 0 | prevalence > 1))
    snet_abort("The {.var prevalence} of a transition must be between 0 and 1.")
  prevalence[spontaneous] <- NA_real_
  out <- dplyr::tibble(from = from, to = to, rate = rate, source = source,
                       exposure = exposure, threshold = threshold,
                       prevalence = prevalence)
  class(out) <- c("diffusionTransitions", class(out))
  out
}

# Reads one source: NA is spontaneous; text such as "E+I" is split on "+"
# only when every part is a known value and the whole is not.
.transition_source <- function(x, known){
  if(is.null(x) || length(x) == 0 || all(is.na(x))) return(NULL)
  x <- as.character(x)
  if(length(x) == 1 && grepl("+", x, fixed = TRUE)){
    parts <- strsplit(x, "+", fixed = TRUE)[[1]]
    if(all(parts %in% known)){
      if(x %in% known)
        snet_abort(paste("The source {.val {x}} is a value, and its parts",
                         "{.val {parts}} are values too.",
                         "Please give the source as a list, as in {.code list(c(\"E\", \"I\"))},",
                         "or rename the value."))
      x <- parts
    }
  }
  x
}

# A chance per step becomes a hazard per step; a chance of 1 is certain.
.chance_hazard <- function(chance, arg){
  if(!is.numeric(chance) || anyNA(chance) || any(chance < 0 | chance > 1))
    snet_abort("{.arg {arg}} must be a chance between 0 and 1.")
  -log1p(-chance)
}

# The values that a table of transitions uses.
.transition_values <- function(transitions)
  unique(c(transitions$from, transitions$to, unlist(transitions$source)))

# Stops where the start state holds a value that no transition uses.
.transitions_cover <- function(transitions, state, attribute){
  unused <- setdiff(unique(as.character(state[!is.na(state)])),
                    .transition_values(transitions))
  if(length(unused))
    snet_abort(paste("{.var {attribute}} holds the {cli::qty(length(unused))}value{?s}",
                     "{.val {unused}}, which no transition uses.",
                     "For a value that never changes, add a row with {.code rate = 0},",
                     "as in {.code data.frame(from = \"{unused[1]}\", to = \"{unused[1]}\", rate = 0)}."))
  invisible(transitions)
}

# Readies each row for the loop: rates, thresholds and degrees as vectors.
.transitions_engine <- function(transitions, .data, n, contacts, direction,
                                twomode, degree = NULL){
  attr_vector <- function(x, what, r){
    if(is.character(x)){
      if(!x %in% net_node_attributes(.data))
        snet_abort("The {what} in row {r} of {.arg transitions} names {.val {x}}, which is not a node attribute.")
      x <- node_attribute(.data, x)
      if(!is.numeric(x) || anyNA(x))
        snet_abort("The node attribute that gives the {what} in row {r} must be numeric, with no missing values.")
      if(what == "rate" && any(x < 0))
        snet_abort("The node attribute that gives the rate in row {r} must be 0 or more.")
    }
    rep_len(x, n)
  }
  # the degree in the co-affiliation network, where only one mode diffuses
  deg <- if(is.null(degree)) .node_deg(.data) else degree
  rows <- lapply(seq_len(nrow(transitions)), function(r){
    rate <- transitions$rate[[r]]
    if(!is.function(rate)) rate <- attr_vector(rate, "rate", r)
    source <- transitions$source[[r]]
    thr <- NULL
    if(!is.null(source)){
      thr <- attr_vector(transitions$threshold[[r]], "threshold", r)
      # a threshold of a share of the contacts, as before
      if(all(thr <= 1) && !all(thr == 1)) thr <- thr * deg
    }
    list(from = transitions$from[r], to = transitions$to[r], rate = rate,
         source = source, exposure = transitions$exposure[r], threshold = thr,
         prevalence = transitions$prevalence[r])
  })
  voter <- any(transitions$exposure %in% "voter")
  degrees <- if(voter) lapply(contacts, function(mat)
    if(twomode) deg else .exposure_degree(mat, direction))
  list(rows = rows, n = n, contacts = contacts, direction = direction,
       twomode = twomode, degrees = degrees, .data = .data)
}

# The number (or weight) of the contacts of each node, in `direction`.
.exposure_degree <- function(mat, direction = "in"){
  weighted <- !all(mat %in% c(0, 1))
  if(direction == "out") mat <- t(mat) else if(direction == "all"){
    if(weighted){
      if(!isSymmetric(unname(mat))) mat <- mat + t(mat)
    } else mat <- (mat != 0) | (t(mat) != 0)
  }
  diag(mat) <- 0
  unname(if(weighted) colSums(mat) else colSums(mat != 0))
}

# One step of the engine, in two parts.
# First the spontaneous transitions (rows with no source), from the state at
# the start of the step. Then the transitions that a source causes, for the
# nodes that have not moved, where a source counts only if it was in a source
# value at the start of the step and is still in it.
# So a node that recovers or dies in a step does not infect in that step.
# In each part, a node at risk moves with probability 1 - exp(-sum(h)),
# and its destination is drawn in proportion to h.
.transitions_step <- function(engine, state, wave, time, begin,
                              active = NULL){
  n <- engine$n
  rows <- engine$rows
  if(is.null(active)) active <- rep(TRUE, n)
  spontaneous <- vapply(rows, function(row) is.null(row$source), logical(1))
  # an inactive node is not at risk
  alive <- !is.na(state) & active
  if(!is.null(begin$dead_value))
    alive <- alive & state != as.character(begin$dead_value)
  first <- .transitions_draw(engine, which(spontaneous), state, state, alive,
                             wave, time, begin, active)
  after <- state
  after[first$moves] <- first$to
  stayed <- alive
  stayed[first$moves] <- FALSE
  second <- .transitions_draw(engine, which(!spontaneous), state, after, stayed,
                              wave, time, begin, active)
  list(moves = c(first$moves, second$moves), to = c(first$to, second$to),
       exposure = c(first$exposure, second$exposure))
}

# Draws the moves of the nodes at risk (`at_risk`) over the rows `which`.
# `state` is the state at the start of the step, and `after` the state after
# the spontaneous transitions: a source must be in a source value in both.
.transitions_draw <- function(engine, which, state, after, at_risk, wave,
                              time, begin, active = rep(TRUE, engine$n)){
  n <- engine$n
  none <- list(moves = integer(0), to = character(0), exposure = numeric(0))
  if(!length(which)) return(none)
  mat <- engine$contacts[[wave]]
  # an inactive node does not transmit
  live <- !is.na(after) & active
  if(!is.null(begin$dead_value))
    live <- live & after != as.character(begin$dead_value)
  H <- matrix(0, n, length(which))
  K <- matrix(NA_real_, n, length(which))
  typed <- NULL
  for(j in seq_along(which)){
    r <- which[j]
    row <- engine$rows[[r]]
    risk <- at_risk & state == row$from
    if(!any(risk)) next
    k <- NULL
    if(!is.null(row$source)){
      mark <- which(live & state %in% row$source & after %in% row$source)
      k <- .exposure_count(mat, mark, engine$direction, engine$twomode)
      K[, j] <- k
    }
    if(is.function(row$rate)){
      if(is.null(typed)) typed <- .state_values(after, begin$template)
      h <- row$rate(state = typed, exposure = k, time = time,
                    .data = engine$.data)
      if(!is.numeric(h) || anyNA(h) || any(h < 0) || !length(h) %in% c(1, n))
        snet_abort(paste("The function in the {.var rate} of row {r} must return",
                         "one hazard of 0 or more, or one for each node."))
      h <- rep_len(h, n)
    } else if(is.null(k)){
      h <- row$rate
    } else {
      thr <- row$threshold
      if(row$prevalence > 0)
        thr <- thr * (1 - row$prevalence * mean(state[live] %in% row$source))
      h <- switch(row$exposure,
                  threshold = row$rate,
                  cascade = ifelse(k > 0, row$rate * k, 0),
                  voter = ifelse(k > 0, row$rate * k /
                                   pmax(engine$degrees[[wave]], 1), 0))
      h[k < thr] <- 0
    }
    h <- rep_len(h, n)
    h[!risk] <- 0
    H[, j] <- h
  }
  total <- rowSums(H)
  chance <- ifelse(is.infinite(total), 1, 1 - exp(-total))
  moves <- which(total > 0 & stats::runif(n) < chance)
  if(!length(moves)) return(none)
  chosen <- vapply(moves, function(i){
    h <- H[i, ]
    if(any(is.infinite(h))) h <- as.numeric(is.infinite(h))
    options <- which(h > 0)
    if(length(options) == 1) options else sample(options, 1, prob = h[options])
  }, integer(1))
  list(moves = moves,
       to = vapply(engine$rows[which[chosen]], `[[`, "", "to"),
       exposure = K[cbind(moves, chosen)])
}

# The role of each value of the compartments, for the old report.
.diffusion_inverse <- function(comps){
  values <- unlist(lapply(comps, as.character))
  stats::setNames(rep(names(comps), lengths(comps)), values)
}

# One row of the report of the old version.
.diffusion_old_report <- function(time, state, roles, mat, direction, twomode,
                                  new_infected){
  role <- roles[as.character(state)]
  infected <- which(role == "I")
  data.frame(t = time, n = length(state), S = sum(role == "S", na.rm = TRUE),
             s = sum(.exposure_exposed(mat, infected, direction, twomode)),
             E = sum(role == "E", na.rm = TRUE), I_new = new_infected,
             I = length(infected), R = sum(role == "R", na.rm = TRUE))
}


# Start state ####

# Finds the start of a diffusion on `.data`, in one of three cases:
# a history of `attribute` in the changes (1), the values of `attribute` on the
# nodes (2), or no `attribute`, where `seeds` and `immune` give the start (3).
# Returns the state of each node and the nodes in each role at the start,
# the start time, the seeds that the record writes at the start (case 3 only),
# and the compartments as a list.
# With a table of transitions, `values` holds its values, and the state is
# read in these values directly, with no roles.
.diffusion_start <- function(.data, attribute = "diffusion", seeds = NULL,
                             compartments = NULL, immune = NULL, values = NULL,
                             exclude = NULL){
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
    template <- state
    if(is.null(values)){
      roles <- .diffusion_roles(state, comps, attribute, exclude)
      # the engine holds each role in the first value of that role
      state <- vapply(roles, function(r) if(is.na(r)) NA_character_ else
        as.character(comps[[r]][1]), character(1), USE.NAMES = FALSE)
    } else {
      roles <- rep(NA_character_, n)
      unknown <- is.na(state)
      if(!is.null(exclude)) unknown <- unknown & !exclude
      missing_state <- sum(unknown)
      if(missing_state > 0){
        susceptible <- comps$S[1]
        if(!is.null(susceptible) && as.character(susceptible) %in% values){
          snet_info("{missing_state} node{?s} with a missing state start{?s/} as {.val {susceptible}}.")
          state[unknown] <- susceptible
        } else snet_info("{missing_state} node{?s} with a missing state do{?es/} not take part.")
      }
    }
    list(state = state, template = template,
         infected = which(roles == "I"), latent = which(roles == "E"),
         recovered = which(roles == "R"), dead = which(roles == "D"),
         seeds = integer(0), time = time, compartments = comps,
         case = if(!is.null(history)) 1 else 2, given = !is.null(compartments),
         exclude = exclude)
  } else {
    if(is.null(seeds)) seeds <- 1
    seeds <- .resolve_nodes(.data, seeds, "seeds")
    if(!is.null(immune)) immune <- .resolve_nodes(.data, immune, "immune")
    comps <- .diffusion_compartments(compartments, character(0))
    state <- rep(comps$S[1], n)
    if(length(immune) && !is.null(comps$R)) state[immune] <- comps$R[1]
    state[seeds] <- comps$I[1]
    if(!is.null(exclude)) state[exclude] <- NA
    list(state = state, template = unlist(comps, use.names = FALSE),
         infected = seeds, latent = NULL, recovered = immune, dead = integer(0),
         seeds = seeds, time = .network_start(.data), compartments = comps,
         case = 3, given = !is.null(compartments), exclude = exclude)
  }
}

# The first time of a network is the earliest time of its ties, or 0.
# The time of the ties is their stamp, or the begin of an interval tie.
# A network whose ties have no time but whose changes are dated starts at
# its first change, because 0 cannot be a date.
.network_start <- function(.data){
  atts <- net_tie_attributes(.data)
  stamp <- intersect(c("time", "begin", "beg", "start", "wave", "panel"), atts)[1]
  if(!is.na(stamp)){
    times <- .bare_time(tie_attribute(.data, stamp))
    if((is.numeric(times) || inherits(times, "Date")) && any(!is.na(times)))
      return(min(times, na.rm = TRUE))
  }
  if(is_changing(.data)){
    times <- as_changelist(.data)$time
    if(inherits(times, "Date") && any(!is.na(times)))
      return(min(times, na.rm = TRUE))
  }
  0
}

# Time and composition ####

# The contact matrices of a diffusion, made once, before the loop.
# A list of networks is a panel, with one wave for each step.
# A panel network has a matrix for each wave, made with to_time().
# An event stream has a matrix for each moment: the ties accumulated to that
# moment (to_slices()), or the interval ties active at it (to_time()).
# `times` holds the moment of each matrix, or NULL for one matrix each step.
# Only the moments that the steps can reach are made.
.diffusion_contacts <- function(src, start, steps){
  if(is_list(src))
    return(list(contacts = lapply(src, .exposure_matrix, inform = TRUE),
                times = NULL))
  whole <- list(contacts = list(.exposure_matrix(src, inform = TRUE)),
                times = NULL)
  rule <- .time_rule(src)
  if(rule == "none") return(whole)
  atts <- net_tie_attributes(src)
  if(rule == "interval"){
    stamp <- intersect(c("begin", "beg", "start"), atts)[1]
    moments <- .bare_time(tie_attribute(src, stamp))
    if("end" %in% atts) moments <- c(moments, .bare_time(tie_attribute(src, "end")))
  } else {
    stamp <- intersect(c("time", "wave", "panel"), atts)[1]
    # a network that changes only in its nodes has one set of ties
    if(is.na(stamp)) return(whole)
    moments <- .bare_time(tie_attribute(src, stamp))
  }
  moments <- sort(unique(moments[!is.na(moments)]))
  if(length(moments) < 2) return(whole)
  ordered <- is.numeric(moments) || inherits(moments, "Date")
  if(ordered){
    # the moment in force at the start, and the later ones that a step reaches
    keep <- moments >= max(c(moments[1], moments[moments <= start]))
    if(is.finite(steps)) keep <- keep & moments <= start + steps
    moments <- moments[keep]
  }
  # node changes are not ties: to_time() would remove the inactive nodes
  static <- .without_changes(src)
  nets <- if(rule == "increment"){
    out <- to_slices(static, attribute = stamp, slice = moments)
    if(length(moments) == 1) list(out) else unname(out)
  } else lapply(moments, function(m) to_time(static, m))
  list(contacts = lapply(nets, .exposure_matrix, inform = FALSE),
       times = if(ordered) moments else NULL)
}

# Which mode of a two-mode network diffuses. A state that is known on one
# mode only (in the attribute, its history, or the seeds and immune nodes)
# diffuses on that mode, through the nodes of the other mode that they share.
# Returns NULL for a one-mode network, or where the state is known on both
# modes, which then diffuse along their ties to each other.
.diffusion_affiliation <- function(.data, attribute, seeds = 1, immune = NULL){
  if(!is_twomode(.data) || is_multilevel(.data)) return(NULL)
  dims <- net_dims(.data)
  n <- sum(dims)
  first_mode <- seq_len(n) <= dims[1]
  known <- NULL
  if(attribute %in% net_node_attributes(.data))
    known <- !is.na(node_attribute(.data, attribute))
  if(is_changing(.data)){
    history <- as_changelist(.data)
    history <- history[history$var == attribute, , drop = FALSE]
    if(nrow(history)){
      if(is.null(known)) known <- rep(FALSE, n)
      known[history$node] <- TRUE
    }
  }
  if(is.null(known)){
    nodes <- c(.resolve_nodes(.data, seeds, "seeds"),
               if(!is.null(immune)) .resolve_nodes(.data, immune, "immune"))
    known <- seq_len(n) %in% nodes
  }
  on_first <- any(known & first_mode)
  on_second <- any(known & !first_mode)
  if(on_first == on_second) return(NULL)
  list(mode = if(on_first) 1 else 2,
       exclude = if(on_first) !first_mode else first_mode)
}

# The co-affiliation matrix of one mode of a two-mode (biadjacency) matrix,
# in a square matrix of all the nodes, with no ties for the other mode.
# Unweighted, two nodes are tied if they share a node of the other mode.
# Weighted or signed, the tie sums the products of their weights.
.coaffiliation_matrix <- function(mat, mode = 1){
  weighted <- !all(mat %in% c(0, 1))
  proj <- if(mode == 1) mat %*% t(mat) else t(mat) %*% mat
  if(!weighted) proj <- (proj > 0) * 1
  diag(proj) <- 0
  n1 <- nrow(mat)
  n2 <- ncol(mat)
  idx <- if(mode == 1) seq_len(n1) else n1 + seq_len(n2)
  out <- matrix(0, n1 + n2, n1 + n2)
  out[idx, idx] <- unname(proj)
  out
}

# The co-affiliation matrices over time: a pair of nodes shares a node of the
# other mode only while that node is active, so there is a matrix for each
# moment of the ties and each change of `active`.
.coaffiliation_timeline <- function(timeline, presence, mode, start){
  times <- timeline$times
  # c() with NULL first would drop the class of dates
  if(!is.null(presence))
    times <- if(is.null(times)) presence$times else
      sort(unique(c(times, presence$times)))
  if(is.null(timeline$times) && length(timeline$contacts) > 1)
    times <- NULL
  if(is.null(times))
    return(list(contacts = lapply(timeline$contacts, .coaffiliation_matrix,
                                  mode = mode), times = timeline$times))
  contacts <- lapply(times, function(t){
    mat <- timeline$contacts[[.diffusion_wave(timeline, t, start)]]
    active <- .diffusion_active(presence, t)
    if(length(active) > 1){
      n1 <- nrow(mat)
      if(mode == 1) mat[, !active[n1 + seq_len(ncol(mat))]] <- 0 else
        mat[!active[seq_len(n1)], ] <- 0
    }
    .coaffiliation_matrix(mat, mode)
  })
  list(contacts = contacts, times = times)
}

# The exposure of each node to the marked nodes of a two-mode network.
# Marked nodes in one mode expose the nodes of that mode that share a node
# of the other mode with them. Marked nodes in both modes expose the nodes
# of the other mode that they are tied to.
.twomode_exposure <- function(mat, mark, weighted = NULL){
  if(is.logical(mark)) mark <- which(mark)
  n1 <- nrow(mat)
  n <- n1 + ncol(mat)
  if(!length(mark)) return(rep(0, n))
  if(all(mark <= n1) || all(mark > n1))
    return(.exposure_kernel(.coaffiliation_matrix(mat, if(all(mark <= n1)) 1 else 2),
                            mark, "all", weighted))
  .exposure_kernel(.bipartite_square(mat), mark, "all", weighted)
}

# The number of steps over which data that change in time run: one step for
# each network of a list, and otherwise until one step after the last moment
# of the ties, of `active`, or of the changes to other attributes.
# The number of nodes where nothing changes in time.
.diffusion_period <- function(timeline, presence, .data, attribute, start, n){
  if(is.null(timeline$times) && length(timeline$contacts) > 1)
    return(length(timeline$contacts))
  ends <- list(timeline$times, if(!is.null(presence)) presence$times)
  if(is_changing(.data)){
    chg <- as_changelist(.data)
    ends[[3]] <- chg$time[chg$var != attribute]
  }
  span <- unlist(lapply(ends, function(x){
    if(!length(x)) return(NULL)
    d <- tryCatch(suppressWarnings(as.numeric(x - start)),
                  error = function(e) NULL)
    if(is.null(d)) NULL else d
  }))
  span <- span[is.finite(span)]
  if(length(span) && max(span) > 0) max(span) + 1 else n
}

# The network without its node changes.
.without_changes <- function(.data){
  if(!is_changing(.data)) return(.data)
  if(inherits(.data, "stocnet")){
    .data$changes <- NULL
    return(.data)
  }
  out <- igraph::delete_graph_attr(as_igraph(.data), "changes")
  if(is_tidygraph(.data)) as_tidygraph(out) else out
}

# The matrix in force at `time`: the latest moment at or before it,
# the first before the first moment, and the last after the last moment.
# With no moments, the matrix of the step, and the last after the last step.
.diffusion_wave <- function(timeline, time, start){
  if(is.null(timeline$times))
    return(min(as.numeric(time - start) + 1, length(timeline$contacts)))
  max(1, sum(timeline$times <= time))
}

# Which nodes are active at each moment that `active` changes, from the
# `active` node attribute and its changes. NULL when no node is inactive.
.diffusion_presence <- function(.data, start){
  has_col <- "active" %in% net_node_attributes(.data)
  st <- if(inherits(.data, "stocnet")) .data else as_stocnet(.data)
  chg <- if(is_changing(st)) as_changelist(st) else NULL
  if(!is.null(chg)) chg <- chg[as.character(chg$var) == "active", , drop = FALSE]
  if(!has_col && (is.null(chg) || !nrow(chg))) return(NULL)
  times <- start
  if(!is.null(chg) && nrow(chg))
    times <- sort(unique(c(start, chg$time)))
  list(times = times, matrix = .node_state(st, "active", times, default = TRUE),
       last = times[length(times)])
}

# Whether each node is active at `time`.
.diffusion_active <- function(presence, time){
  if(is.null(presence)) return(TRUE)
  presence$matrix[, max(1, sum(presence$times <= time))]
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
.diffusion_roles <- function(state, comps, attribute, exclude = NULL){
  roles <- rep(NA_character_, length(state))
  for(role in names(comps))
    roles[!is.na(state) & state %in% comps[[role]]] <- role
  unknown <- unique(state[!is.na(state) & is.na(roles)])
  if(length(unknown))
    snet_abort(paste("{.var {attribute}} holds the value{?s} {.val {unknown}},",
                     "which {?has/have} no role in {.arg compartments}.",
                     "Please give each value a role, as in",
                     "{.code compartments = c(S = \"unaware\", I = \"adopter\")}."))
  # the nodes of the other mode of a co-affiliation diffusion have no state
  unknown <- is.na(state)
  if(!is.null(exclude)) unknown <- unknown & !exclude
  missing_state <- sum(unknown)
  if(missing_state > 0){
    if(!"S" %in% names(comps))
      snet_abort("{.arg compartments} needs the role {.val S} for the nodes whose state is missing.")
    snet_info("{missing_state} node{?s} with a missing state start{?s/} as susceptible.")
    roles[unknown] <- "S"
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
.diffusion_record <- function(.data, events, begin, attribute,
                              transitions = NULL){
  comps <- begin$compartments
  # a stocnet keeps the model, so that netrics and infernet can read it
  if(inherits(.data, "stocnet") && !is.null(transitions))
    .data$info$transitions <- transitions
  if(begin$case == 3){
    # all nodes start in S, except the immune, which start in R
    start <- rep(comps$S[1], net_nodes(.data))
    if(length(begin$recovered)) start[begin$recovered] <- comps$R[1]
    # the other mode of a co-affiliation diffusion has no state
    if(!is.null(begin$exclude)) start[begin$exclude] <- NA
    .data <- add_node_attribute(.data, attribute, start)
  }
  # a stocnet keeps the compartments that were given, so that others can read them
  if(inherits(.data, "stocnet") && isTRUE(begin$given))
    .data$info$compartments <- comps
  # a node that dies keeps its last value, and becomes inactive
  dead <- begin$dead_value
  died <- if(is.null(dead)) rep(FALSE, nrow(events)) else
    as.character(events$event) == as.character(dead)
  states <- events[!died, , drop = FALSE]
  values <- .state_values(as.character(states$event), begin$template)
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

# Gives the values that the engine holds as text the type of the attribute.
.state_values <- function(keys, template){
  if(is.logical(template)) as.logical(keys) else
    if(is.numeric(template)) as.numeric(keys) else keys
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
# In a two-mode network, a node is exposed by the marked nodes of the other
# mode that it is tied to, as a woman by the events she attends.
.exposure_count <- function(mat, mark, direction = "in", twomode = FALSE){
  if(twomode) return(.exposure_kernel(.bipartite_square(mat), mark, "all"))
  .exposure_kernel(mat, mark, direction)
}

# Whether each node is exposed to the marked nodes.
.exposure_exposed <- function(mat, mark, direction = "in", twomode = FALSE){
  if(twomode)
    return(.exposure_kernel(.bipartite_square(mat), mark, "all",
                            weighted = FALSE) > 0)
  .exposure_kernel(mat, mark, direction, weighted = FALSE) > 0
}

# The square matrix of all the nodes of a two-mode (biadjacency) matrix,
# with the first mode in the rows and the second in the columns.
.bipartite_square <- function(mat){
  n1 <- nrow(mat)
  n2 <- ncol(mat)
  out <- matrix(0, n1 + n2, n1 + n2)
  out[seq_len(n1), n1 + seq_len(n2)] <- mat
  out[n1 + seq_len(n2), seq_len(n1)] <- t(mat)
  out
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
  weighted <- manynet::is_weighted(.data) || manynet::is_signed(.data)
  if(manynet::is_twomode(.data) && !manynet::is_multilevel(.data))
    return(make_node_measure(.twomode_exposure(.exposure_matrix(.data), mark,
                                               weighted = weighted), .data))
  out <- .exposure_kernel(.exposure_matrix(.data), mark, direction,
                          weighted = weighted)
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
  if(is_twomode(.data) && !is_multilevel(.data))
    return(make_node_mark(.twomode_exposure(.exposure_matrix(.data), mark,
                                            weighted = FALSE) > 0, .data))
  out <- .exposure_kernel(.exposure_matrix(.data), mark, direction,
                          weighted = FALSE) > 0
  make_node_mark(out, .data)
}

