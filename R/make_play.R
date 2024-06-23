# Diffusions ####

#' Making diffusion models on networks
#' @description
#' These functions simulate diffusion or compartment models upon a network.
#' 
#' - `play_diffusion()` runs a single simulation of a compartment model,
#' allowing the results to be visualised and examined.
#' - `play_diffusions()` runs multiple simulations of a compartment model
#' for more robust inference.
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
#' @inheritParams is
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
#' @name play
NULL

#' @rdname play
#' @examples 
#'   smeg <- generate_smallworld(15, 0.025)
#'   plot(play_diffusion(smeg, recovery = 0.4))
#'   #graphr(play_diffusion(ison_karateka))
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
                           immune = NULL,
                           steps) {
  thisRequires("migraph")
  n <- net_nodes(.data)
  recovered <- NULL
  if(missing(steps)) steps <- n
  if(is.character(thresholds)) thresholds <- node_attribute(.data, thresholds)
  if(length(thresholds)==1) thresholds <- rep(thresholds, n)
  if(all(thresholds <= 1) & !all(thresholds == 1)) 
    thresholds <- thresholds * 
      node_deg(.data)
  if(is.character(seeds)) seeds <- which(node_names(.data)==seeds)
  if(is.logical(seeds)) seeds <- which(seeds)
  if(!is.null(immune)){
    if(is.logical(immune)) immune <- which(immune)
    recovered <- immune
  }
  infected <- seeds # seeds are initial infected
  latent <- NULL # latent compartment starts empty
  time = 0 # starting at 0
  # initialise events table
  events <- data.frame(t = time, nodes = seeds, event = "I", exposure = NA)
  if(!is_list(.data)) sinit <- sum(node_is_exposed(.data, infected)) else 
    if(is_list(.data)) sinit <- sum(node_is_exposed(.data[[1]], infected))
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
    
    # add any global/prevalence feedback
    new_prev <- as_matrix(net) + ((report$I_new[length(report$I_new)] - 
                         length(recovers)) * prevalence)
    if(!is_twomode(.data) & !is_complex(.data)) diag(new_prev) <- 0
    net <- as_tidygraph(new_prev)
    
    # at main infection stage, get currently exposed to infection:
    exposed <- node_is_exposed(net, infected)
    # count exposures for each node:
    exposure <- node_exposure(net, infected)
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
  make_diff_model(events, report, .data)
}

#' @rdname play
#' @param times Integer indicating number of simulations. 
#'   By default `times=5`, but 1,000 - 10,000 simulations recommended for publication-ready results.
#' @param strategy If `{furrr}` is installed, then multiple cores can be used to accelerate the simulations. 
#'   By default "sequential", but if multiple cores available, then "multisession" or "multicore" may be useful. 
#'   Generally this is useful only when times > 1000. See `{furrr}` for more.
#' @param verbose Whether the function should report on its progress. 
#'   By default FALSE. See `{progressr}` for more.
#' @examples 
#'   plot(play_diffusions(smeg, times = 10))
#' @export
play_diffusions <- function(.data,
                            seeds = 1,
                            contact = NULL,
                            prevalence = 0,
                            thresholds = 1,
                            transmissibility = 1,
                            latency = 0,
                            recovery = 0,
                            waning = 0,
                            immune = NULL,
                            steps,
                            times = 5,
                            strategy = "sequential",
                            verbose = FALSE) {
  thisRequires("future")
  thisRequires("furrr")
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  
  if(missing(steps)) steps <- net_nodes(.data)
  
  out <- furrr::future_map_dfr(1:times, function(j){
      data.frame(sim = j,
                 play_diffusion(.data, 
                     seeds = seeds, contact = contact, prevalence = prevalence, 
                     thresholds = thresholds, transmissibility = transmissibility,
                     latency = latency, recovery = recovery, waning = waning,
                     immune = immune, steps = steps))
    }, .progress = verbose, .options = furrr::furrr_options(seed = T))
  make_diffs_model(out, .data)
}

# contagion_function = attrib ~ 1 + prevalence + threshold + contact + equivalence

# Learning ####

#' Making learning models on networks
#' 
#' @description
#' These functions allow learning games to be played upon networks.
#' 
#' - `play_learning()` plays a DeGroot learning model upon a network.
#' - `play_segregation()` plays a Schelling segregation model upon a network.
#' 
#' @name learning
#' @family makes
#' @family models
#' @inheritParams is
#' @param steps The number of steps forward in learning.
#'   By default the number of nodes in the network.
#' @param beliefs A vector indicating the probabilities nodes
#'   put on some outcome being 'true'.
#' @param epsilon The maximum difference in beliefs accepted
#'   for convergence to a consensus.
#' @examples 
#'   play_learning(ison_networkers, 
#'       rbinom(net_nodes(ison_networkers),1,prob = 0.25))
#' @export
play_learning <- function(.data, 
                          beliefs,
                          steps,
                          epsilon = 0.0005){
  n <- net_nodes(.data)
  if(length(beliefs)!=n) 
    stop("'beliefs' must be a vector the same length as the number of nodes in the network.")
  if(is.logical(beliefs)) beliefs <- beliefs*1
  if(missing(steps)) steps <- n
  
  t = 0
  out <- matrix(NA,steps+1,length(beliefs))
  out[1,] <- beliefs
  trust_mat <- as_matrix(.data)/rowSums(as_matrix(.data))
  
  repeat{
    old_beliefs <- beliefs
    beliefs <- trust_mat %*% beliefs
    if(all(abs(old_beliefs - beliefs) < epsilon)) break
    t = t+1
    out[t+1,] <- beliefs
    if(t==steps) break
  }
  out <- stats::na.omit(out)
  
  make_learn_model(out, .data)
}

#' @rdname learning
#' @param attribute A string naming some nodal attribute in the network.
#'   Currently only tested for binary attributes.
#' @param heterophily A score ranging between -1 and 1 as a threshold for 
#'   how heterophilous nodes will accept their neighbours to be.
#'   A single proportion means this threshold is shared by all nodes,
#'   but it can also be a vector the same length of the nodes in the network
#'   for issuing different thresholds to different nodes.
#'   By default this is 0, meaning nodes will be dissatisfied if more than half
#'   of their neighbours differ on the given attribute.
#' @param who_moves One of the following options:
#'   "ordered" (the default) checks each node in turn for whether they are
#'   dissatisfied and there is an available space that they can move to,
#'   "random" will check a node at random, 
#'   and "most_dissatisfied" will check (one of) the most dissatisfied nodes first.
#' @param choice_function One of the following options:
#'   "satisficing" (the default) will move the node to any coordinates that satisfy
#'   their heterophily threshold,
#'   "optimising" will move the node to coordinates that are most homophilous,
#'   and "minimising" distance will move the node to the next nearest unoccupied coordinates.
#' @examples 
#'   startValues <- rbinom(100,1,prob = 0.5)
#'   startValues[sample(seq_len(100), round(100*0.2))] <- NA
#'   latticeEg <- create_lattice(100)
#'   latticeEg <- add_node_attribute(latticeEg, "startValues", startValues)
#'   latticeEg
#'   play_segregation(latticeEg, "startValues", 0.5)
#'   # graphr(latticeEg, node_color = "startValues", node_size = 5) + 
#'   # graphr(play_segregation(latticeEg, "startValues", 0.2), 
#'   #            node_color = "startValues", node_size = 5)
#' @export
play_segregation <- function(.data, 
                             attribute,
                             heterophily = 0,
                             who_moves = c("ordered","random","most_dissatisfied"),
                             choice_function = c("satisficing","optimising", "minimising"),
                             steps) {
  thisRequires("migraph")
  n <- net_nodes(.data)
  if(missing(steps)) steps <- n
  who_moves <- match.arg(who_moves)
  choice_function <- match.arg(choice_function)
  if(length(heterophily)==1) heterophily <- rep(heterophily, n)
  if(length(heterophily)!=n) stop("Heterophily threshold must be the same length as the number of nodes in the network.")
  swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 
  
  t = 0
  temp <- .data
  moved <- NULL
  while(steps > t){
    t <- t+1
    current <- node_attribute(temp, attribute)
    heterophily_scores <- node_heterophily(temp, attribute)
    dissatisfied <- which(heterophily_scores > heterophily)
    unoccupied <- which(is.na(current))
    dissatisfied <- setdiff(dissatisfied, unoccupied)
    dissatisfied <- setdiff(dissatisfied, moved)
    if(length(dissatisfied)==0) break
    dissatisfied <- switch(who_moves,
                           ordered = dissatisfied[1],
                           random = sample(dissatisfied, 1),
                           most_dissatisfied = dissatisfied[
                             which(heterophily_scores[dissatisfied] == 
                                     max(heterophily_scores[dissatisfied]))[1]])
    options <- vapply(unoccupied, function(u){
      test <- add_node_attribute(temp, "test", 
                                          swtch(current, dissatisfied, u))
      node_heterophily(test, "test")[u]
    }, FUN.VALUE = numeric(1))
    if(length(options)==0) next
    move_to <- switch(choice_function,
                      satisficing = unoccupied[sample(which(options <= heterophily[unoccupied]), 1)],
                      optimising = unoccupied[which.min(options)[1]],
                      minimising = unoccupied[which.min(igraph::distances(temp, 
                                                                          igraph::V(temp)[dissatisfied], 
                                                                          igraph::V(temp)[unoccupied]))])
    if(is.na(move_to)) next
    print(paste("Moving node", dissatisfied, "to node", move_to))
    temp <- add_node_attribute(temp, attribute, 
                                        swtch(current, dissatisfied, move_to))
    moved <- c(dissatisfied, moved)
  }
  temp
}
