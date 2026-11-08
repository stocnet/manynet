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
#' @inheritParams mark_is
#' @param seeds A valid mark vector the length of the
#'   number of nodes in the network.
#' @param contact A matrix or network that replaces ".data" with some 
#'   other explicit contact network, e.g.
#'   `create_components(.data, membership = node_in_structural(.data))`.
#'   Can be of arbitrary complexity, but must of the same dimensions
#'   as .data.
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
#' @param immune A logical or numeric vector identifying nodes
#'   that begin the diffusion process as already recovered.
#'   This could be interpreted as those who are vaccinated or equivalent.
#'   Note however that a waning parameter will affect these nodes too.
#'   By default NULL, indicating that no nodes begin immune.
#' @param steps The number of steps forward in the diffusion to play.
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
#' @export
play_diffusion <- function(.data, 
                           seeds = 1,
                           contact = NULL,
                           prevalence = 0,
                           thresholds = 1,
                           transmissibility = 1,
                           latency = 0,
                           recovery = 0,
                           waning = 0,
                           fatality = 0,
                           immune = NULL,
                           steps,
                           old_version = FALSE) {
  if(old_version) 
    cli::cli_warn(paste("Please use the new version of this function (since v1.4.0), with new features.",
                        "The old version will no longer be maintained and is retained here only for backwards compatibility."))
  n <- net_nodes(.data)
  recovered <- NULL
  if(missing(steps)) steps <- n
  if(is.character(thresholds)) thresholds <- node_attribute(.data, thresholds)
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  if(all(thresholds <= 1) & !all(thresholds == 1)) 
    thresholds <- thresholds * 
      .node_deg(.data)
  if(is.character(seeds)) seeds <- which(node_labels(.data)==seeds)
  if(is.logical(seeds)) seeds <- which(seeds)
  if(!is.null(immune)){
    if(is.logical(immune)) immune <- which(immune)
    recovered <- immune
  }
  
  infected <- seeds # seeds are initial infected
  latent <- NULL # latent compartment starts empty
  time <- 0 # starting at 0
  # initialise events table
  events <- data.frame(t = time, nodes = seeds, event = "I", exposure = NA)
  if(!is_list(.data)) sinit <- sum(.node_is_exposed(.data, infected)) else 
    if(is_list(.data)) sinit <- sum(.node_is_exposed(.data[[1]], infected))
  # initialise report table
  report <- data.frame(t = time,
                       n = n,
                       S = n - (length(latent) + length(infected) + length(recovered)),
                       s = sinit,
                       E = length(latent),
                       I_new = length(seeds),
                       I = length(infected),
                       R = length(recovered))
  
  repeat{ # At each time step:
    # update network, if necessary
    if(is_list(.data)){
      if(time > length(.data)) break
      net<- .data[[max(time,1)]]
    } else {
      if(is.null(contact)) net <- as_tidygraph(.data) else 
        net <- as_tidygraph(contact)
    }
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
    
    # add any global/prevalence feedback
    new_prev <- as_matrix(net) + ((report$I_new[length(report$I_new)] - 
                         length(recovers)) * prevalence)
    if(!is_twomode(.data) & !is_complex(.data)) diag(new_prev) <- 0
    net <- as_tidygraph(new_prev)
    
    # at main infection stage, get currently exposed to infection:
    exposed <- .node_is_exposed(net, infected)
    # count exposures for each node:
    exposure <- .node_exposure(net, infected)
    # identify those nodes who are exposed at or above their threshold
    
    open_to_it <- which(exposure >= thresholds)
    newinf <- open_to_it[stats::rbinom(length(open_to_it), 1, transmissibility)==1]
    if(!is.null(recovery) & length(recovered)>0) 
      newinf <- setdiff(newinf, recovered) # recovered can't be reinfected
    if(!is.null(latent) & length(latent)>0) 
      newinf <- setdiff(newinf, latent) # latent already infected
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
      events <- rbind(events, 
                    data.frame(t = time, nodes = infectious, event = "I", 
                               exposure = exposure[infectious]))
    # record exposures
    if(!is.null(newinf) & length(newinf)>0)
      events <- rbind(events,
                      data.frame(t = time, nodes = newinf, event = "E", 
                                 exposure = exposure[newinf]))
    # record recoveries
    if(!is.null(recovers) & length(recovers)>0)
      events <- rbind(events,
                      data.frame(t = time, nodes = recovers, event = "R", exposure = NA))
    # record wanings
    if(!is.null(waned) & length(waned)>0)
      events <- rbind(events,
                      data.frame(t = time, nodes = waned, event = "S", exposure = NA))
    # record fatalities
    if(!is.null(fatality) & length(deceased)>0)
      events <- rbind(events,
                      data.frame(t = time, nodes = deceased, event = "D", exposure = NA))
    # Update report table ####
    report <- rbind(report,
                    data.frame(t = time,
                               n = n,
                         S = n - (length(latent) + length(infected) + length(recovered)),
                         s = sum(exposed),
                         E = length(latent),
                         I_new = length(infectious),
                         I = length(infected),
                         R = length(recovered)))
    
    if(is.infinite(steps) & length(infected)==n) break
    if(time==steps) break
  }
  if(old_version){
    make_diff_model(events, report, .data)
  } else {
    .data <- .data |> mutate_nodes(diffusion = "S")
    changes <- data.frame(time = events$t, node = as.integer(events$nodes), 
                         var = "diffusion", value = events$event)
    if(fatality > 0)
      changes <- changes |> mutate(var = ifelse(value=="D", "active", var),
                                    value = ifelse(value=="D", FALSE, value))
    .data <- bind_changes(.data, changes)
    .data
  }
}

# contagion_function = attrib ~ 1 + prevalence + threshold + contact + equivalence

.node_exposure <- function(.data, mark, time = 0){
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
    if(manynet::is_twomode(.data)){
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
    } else {
      dat <- .data
      contacts <- unlist(lapply(igraph::neighborhood(dat, nodes = mark, mode = "out"),
                                function(x) setdiff(x, mark)))
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

.node_is_exposed <- function(.data, mark, time = 0){
  if (missing(mark)){
    if(is_changing(.data)){
      t <- time
      return(make_node_mark(.node_exposure(.data, time = t)>0, .data))
    } else if(inherits(.data, "diff_model")){
      mark <- summary(.data) |> 
        dplyr::filter(t == 0 & event == "I") |> 
        dplyr::select(nodes) |> unlist()
      .data <- attr(.data, "network")
    }    
  }
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(F, manynet::net_nodes(.data))
  out[unique(setdiff(unlist(igraph::neighborhood(as_igraph(.data), nodes = mark)),
                     mark))] <- TRUE
  make_node_mark(out, .data)
}

