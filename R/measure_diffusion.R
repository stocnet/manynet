# net_diffusion ####

#' Measures of network diffusion
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process at the network level:
#'   
#'   - `net_transmissibility()` measures the average transmissibility observed
#'   in a diffusion simulation, or the number of new infections over
#'   the number of susceptible nodes.
#'   - `net_recovery()` measures the average number of time steps 
#'   nodes remain infected once they become infected.
#'   - `net_reproduction()` measures the observed reproductive number
#'   in a diffusion simulation as the network's transmissibility over
#'   the network's average infection length.
#'   - `net_immunity()` measures the proportion of nodes that would need
#'   to be protected through vaccination, isolation, or recovery for herd immunity to be reached.
#'   
#' @param .data Network data with nodal changes,
#'   as created by `play_diffusion()`,
#'   or a valid network diffusion model,
#'   as created by `as_diffusion()`.
#' @inheritParams measure_central_degree
#' @family measures
#' @family diffusion
#' @name measure_diffusion_net
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#' @references
#' ## On epidemiological models
#'   Kermack, William O., and Anderson Gray McKendrick. 1927. 
#'   "A contribution to the mathematical theory of epidemics". 
#'   _Proc. R. Soc. London A_ 115: 700-721.
#'   \doi{10.1098/rspa.1927.0118}
NULL

#' @rdname measure_diffusion_net 
#' @section Transmissibility: 
#'   `net_transmissibility()` measures how many directly susceptible nodes
#'   each infected node will infect in each time period, on average.
#'   That is:
#'   \deqn{T = \frac{1}{n}\sum_{j=1}^n \frac{i_{j}}{s_{j}}}
#'   where \eqn{i} is the number of new infections in each time period, \eqn{j \in n},
#'   and \eqn{s} is the number of nodes that could have been infected in that time period
#'   (note that \eqn{s \neq S}, or 
#'   the number of nodes that are susceptible in the population).
#'   \eqn{T} can be interpreted as the proportion of susceptible nodes that are
#'   infected at each time period.
#' @examples
#'   # To calculate the average transmissibility for a given diffusion model
#'   net_transmissibility(smeg_diff)
#' @export
net_transmissibility <- function(.data){
  diff_model <- as_diffusion(.data)
  out <- diff_model$I_new/diff_model$s
  out <- out[-1]
  out <- out[!is.infinite(out)]
  out <- out[!is.nan(out)]
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  make_network_measure(mean(out, na.rm = TRUE),
                       net,
                       call = deparse(sys.call()))
}

#' @rdname measure_diffusion_net 
#' @param censor Where some nodes have not yet recovered by the end
#'   of the simulation, right censored values can be replaced by the number of steps. 
#'   By default TRUE.
#'   Note that this will likely still underestimate recovery.
#' @section Recovery time: 
#'   `net_recovery()` measures the average number of time steps that
#'   nodes in a network remain infected.
#'   Note that in a diffusion model without recovery, average infection length
#'   will be infinite.
#'   This will also be the case where there is right censoring.
#'   The longer nodes remain infected, the longer they can infect others.
#' @examples
#'   # To calculate the average infection length for a given diffusion model
#'   net_recovery(smeg_diff)
#' @export
net_recovery <- function(.data, censor = TRUE){
  diff_model <- as_diffusion(.data)
  recovs <- node_recovery(.data)
  if(censor && any(!is.infinite(recovs) & !is.na(recovs)))
    recovs[is.infinite(recovs)] <- nrow(diff_model)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  make_network_measure(mean(recovs, na.rm = TRUE),
                       net,
                       call = deparse(sys.call()))
}

#' @rdname measure_diffusion_net 
#' @section Reproduction number: 
#'   `net_reproduction()` measures a given diffusion's reproductive number.
#'   Here it is calculated as:
#'   \deqn{R = \min\left(\frac{T}{1/L}, \bar{k}\right)}
#'   where \eqn{T} is the observed transmissibility in a diffusion
#'   and \eqn{L} is the observed recovery length in a diffusion.
#'   Since \eqn{L} can be infinite where there is no recovery
#'   or there is right censoring,
#'   and since network structure places an upper limit on how many
#'   nodes each node may further infect (their degree),
#'   this function returns the minimum of \eqn{R_0}
#'   and the network's average degree.
#'   
#'   Interpretation of the reproduction number is oriented around R = 1.
#'   Where \eqn{R > 1}, the 'disease' will 'infect' more and more
#'   nodes in the network.
#'   Where \eqn{R < 1}, the 'disease' will not sustain itself and eventually
#'   die out.
#'   Where \eqn{R = 1}, the 'disease' will continue as endemic,
#'   if conditions allow.
#' @references
#' ## On the basic reproduction number
#' Diekmann, Odo, Hans J.A.P. Heesterbeek, and Hans J.A.J. Metz. 1990. 
#' "On the definition and the computation of the basic reproduction ratio R0 in models for infectious diseases in heterogeneous populations". 
#' _Journal of Mathematical Biology_, 28(4): 365–82.
#' \doi{10.1007/BF00178324}
#' 
#' Kenah, Eben, and James M. Robins. 2007. 
#' "Second look at the spread of epidemics on networks". 
#' _Physical Review E_, 76(3 Pt 2): 036113.
#' \doi{10.1103/PhysRevE.76.036113}
#' @examples
#'   # To calculate the reproduction number for a given diffusion model
#'   net_reproduction(smeg_diff)
#' @export
net_reproduction <- function(.data){
  # diff_model <- as_diffusion(.data)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
    out <- net_transmissibility(.data)/
      (1/net_recovery(.data))
    out <- min(out, mean(node_deg(net)))
  make_network_measure(out, net,
                       call = deparse(sys.call()))
}

#' @rdname measure_diffusion_net 
#' @section Herd immunity: 
#'   `net_immunity()` estimates the proportion of a network
#'   that need to be protected from infection for herd immunity
#'   to be achieved.
#'   This is known as the Herd Immunity Threshold or HIT:
#'   \deqn{1 - \frac{1}{R}}
#'   where \eqn{R} is the reproduction number from `net_reproduction()`.
#'   The HIT indicates the threshold at which
#'   the reduction of susceptible members of the network means
#'   that infections will no longer keep increasing.
#'   Note that there may still be more infections after this threshold has been reached,
#'   but there should be fewer and fewer.
#'   These excess infections are called the _overshoot_.
#'   This function does _not_ take into account the structure
#'   of the network, instead using the average degree.
#'   
#'   Interpretation is quite straightforward.
#'   A HIT or immunity score of 0.75 would mean that 75% of the nodes in the network
#'   would need to be vaccinated or otherwise protected to achieve herd immunity.
#'   To identify how many nodes this would be, multiply this proportion with the number
#'   of nodes in the network.
#' @references
#' ## On herd immunity
#' Garnett, G.P. 2005.
#' "Role of herd immunity in determining the effect of vaccines against sexually transmitted disease".
#' _The Journal of Infectious Diseases_, 191(1): S97-106.
#' \doi{10.1086/425271}
#' @examples
#'   # Calculating the proportion required to achieve herd immunity
#'   net_immunity(smeg_diff)
#'   # To find the number of nodes to be vaccinated
#'   net_immunity(smeg_diff, normalized = FALSE)
#' @export
net_immunity <- function(.data, normalized = TRUE){
  # diff_model <- as_diffusion(.data)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  out <- 1 - 1/net_reproduction(.data)
  if(!normalized) out <- ceiling(out * net_nodes(net))
  make_network_measure(out, net,
                       call = deparse(sys.call()))
}

# net_infection ####

#' Measures of network infection
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process at the network level:
#'   
#'   - `net_infection_complete()` measures the number of time steps until
#'   (the first instance of) complete infection.
#'   For diffusions that are not observed to complete,
#'   this function returns the value of `Inf` (infinity).
#'   This makes sure that at least ordinality is respected.
#'   - `net_infection_total()` measures the proportion or total number of nodes
#'   that are infected/activated at some time by the end of the diffusion process.
#'   This includes nodes that subsequently recover.
#'   Where reinfection is possible, the proportion may be higher than 1.
#'   - `net_infection_peak()` measures the number of time steps until the
#'   highest infection rate is observed.
#'   
#' @inheritParams measure_diffusion_net
#' @family measures
#' @family diffusion
#' @name measure_diffusion_infection

#' @rdname measure_diffusion_infection 
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg)
#'   net_infection_complete(smeg_diff)
#' @export
net_infection_complete <- function(.data){
  diff_model <- as_diffusion(.data)
  out <- which(diff_model$I == diff_model$n)[1]
  if(is.na(out)) out <- Inf
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  make_network_measure(out, net,
                       call = deparse(sys.call()))
}

#' @rdname measure_diffusion_infection 
#' @examples
#'   net_infection_total(smeg_diff)
#' @export
net_infection_total <- function(.data, normalized = TRUE){
  if(inherits(.data, "diff_model")){
    diff_model <- as_diffusion(.data)
    out <- sum(diff_model$I_new)
    if(normalized) out <- out / diff_model$n[length(diff_model$n)]
    make_network_measure(out, attr(diff_model, "network"),
                         call = deparse(sys.call()))
  } else {
    out <- sum(as_changelist(.data)$value == "I")
    if(normalized) out <- out / net_nodes(.data)
    make_network_measure(out, .data,
                         call = deparse(sys.call()))
  }
}

#' @rdname measure_diffusion_infection 
#' @examples
#'   net_infection_peak(smeg_diff)
#' @export
net_infection_peak <- function(.data){
  diff_model <- as_diffusion(.data)
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  out <- which(diff_model$I_new == max(diff_model$I_new))[1]
  make_network_measure(out, net,
                       call = deparse(sys.call()))
}

# node_diffusion ####

#' Measures of nodes in a diffusion
#' @description
#'   These functions allow measurement of various features of
#'   a diffusion process:
#'   
#'   - `node_adoption_time()`: Measures the number of time steps until
#'   nodes adopt/become infected
#'   - `node_thresholds()`: Measures nodes' thresholds from the amount
#'   of exposure they had when they became infected
#'   - `node_infection_length()`: Measures the average length nodes that become
#'   infected remain infected in a compartmental model with recovery
#'   - `node_exposure()`: Measures how many exposures nodes have to 
#'   a given mark
#'   
#' @inheritParams mark_is
#' @inheritParams measure_diffusion_net
#' @family measures
#' @family diffusion
#' @name measure_diffusion_node
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   plot(smeg_diff)
#' @references
#' ## On diffusion measures
#'   Valente, Tom W. 1995. _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname measure_diffusion_node 
#' @section Adoption time: 
#'   `node_adoption_time()` measures the time units it took 
#'   until each node became infected.
#'   Note that an adoption time of 0 indicates that this was a seed node.
#' @examples
#'   # To measure when nodes adopted a diffusion/were infected
#'   (times <- node_adoption_time(smeg_diff))
#' @export
node_adoption_time <- function(.data){

  if(inherits(.data, "diff_model")){
    net <- attr(.data, "network") 
    out <- summary(.data) %>% dplyr::filter(event == "I") %>% 
      dplyr::distinct(nodes, .keep_all = TRUE) %>% 
      dplyr::select(nodes,t)
    if(!is_labelled(net))
      out <- dplyr::arrange(out, nodes) else if (is.numeric(out$nodes))
        out$nodes <- node_names(net)[out$nodes]
    out <- stats::setNames(out$t, out$nodes)
    if(length(out) != net_nodes(net)){
      full <- rep(Inf, net_nodes(net))
      names(full) <- `if`(is_labelled(net), 
                          node_names(net), 
                          as.character(seq_len(net_nodes(net))))
      full[match(names(out), names(full))] <- out
      out <- `if`(is_labelled(net), full, unname(full))
    }
  } else {
    net <- .data
    out <- as_changelist(.data) %>% dplyr::filter(value == "I") %>% 
      dplyr::distinct(node, .keep_all = TRUE) %>% 
      dplyr::select(node,time)
    if(!is_labelled(net))
      out <- dplyr::arrange(out, node) else if (is.numeric(out$node))
        out$node <- node_names(net)[out$node]
    out <- stats::setNames(out$time, out$node)
    if(length(out) != net_nodes(net)){
      full <- rep(Inf, net_nodes(net))
      names(full) <- `if`(is_labelled(net), 
                          node_names(net), 
                          as.character(seq_len(net_nodes(net))))
      full[match(names(out), names(full))] <- out
      out <- `if`(is_labelled(net), full, unname(full))
    }
  }
      
  if(!is_labelled(net)) out <- unname(out)
  make_node_measure(out, net)
}

#' @rdname measure_diffusion_node 
#' @param lag The number of time steps back upon which the thresholds are
#'   inferred.
#' @section Thresholds:
#'   `node_thresholds()` infers nodes' thresholds based on how much
#'   exposure they had when they were infected.
#'   This inference is of course imperfect,
#'   especially where there is a sudden increase in exposure,
#'   but it can be used heuristically.
#'   In a threshold model, 
#'   nodes activate when \eqn{\sum_{j:\text{active}} w_{ji} \geq \theta_i},
#'   where \eqn{w} is some (potentially weighted) matrix,
#'   \eqn{j} are some already activated nodes,
#'   and \eqn{theta} is some pre-defined threshold value.
#'   Where a fractional threshold is used, the equation is 
#'   \eqn{\frac{\sum_{j:\text{active}} w_{ji}}{\sum_{j} w_{ji}} \geq \theta_i}.
#'   That is, \eqn{theta} is now a proportion,
#'   and works regardless of whether \eqn{w} is weighted or not.
#' @examples
#'   # To infer nodes' thresholds
#'   node_thresholds(smeg_diff)
#' @export
node_thresholds <- function(.data, normalized = TRUE, lag = 1){
  if(inherits(.data, "diff_model")){
    net <- attr(.data, "network") 
    diff_model <- as_diffusion(.data)
    out <- summary(diff_model)
    if(!"exposure" %in% names(out)){
      out[,'exposure'] <- NA_integer_
      for(v in unique(out$t)){
        out$exposure[out$t == v] <- node_exposure(diff_model, 
                                                  time = v-lag)[out$nodes[out$t == v]]
      }
    }
    if(any(out$event == "E")) 
      out <- out %>% dplyr::filter(event == "E") else 
        out <- out %>% dplyr::filter(event == "I")
    out <- out %>% dplyr::distinct(nodes, .keep_all = TRUE) %>% 
      dplyr::select(nodes, exposure)
    if(is_labelled(net))
      out <- stats::setNames(out$exposure, node_names(net)[out$nodes]) else
        out <- stats::setNames(out$exposure, out$nodes)
  } else {
    net <- .data
    diff_model <- as_diffusion(.data)
    out <- as_changelist(.data)
    if(!"exposure" %in% names(out)){
      out[,'exposure'] <- NA_integer_
      for(v in unique(out$time)){
        out$exposure[out$time == v] <- node_exposure(.data, 
                                                  time = v-lag)[out$node[out$time == v]]
      }
    }
    if(any(out$value == "E")) 
      out <- out %>% dplyr::filter(value == "E") else 
        out <- out %>% dplyr::filter(value == "I")
    out <- out %>% dplyr::distinct(node, .keep_all = TRUE) %>% 
      dplyr::select(node, exposure)
    if(is_labelled(net))
      out <- stats::setNames(out$exposure, node_names(net)[out$node]) else
        out <- stats::setNames(out$exposure, out$node)
  }
  if(length(out) != net_nodes(net)){
    if(is_labelled(net)){
      full <- stats::setNames(rep(Inf, net_nodes(net)), 
                              node_names(net))
    } else {
      full <- stats::setNames(rep(Inf, net_nodes(net)), 
                              1:net_nodes(net))
    } 
    full[match(names(out), names(full))] <- out
    out <- full
  }
  if(is_labelled(net))
    out <- out[match(node_names(net), names(out))] else {
      out <- unname(out[order(as.numeric(names(out)))])
    }
  if(normalized) out <- out / node_deg(net)
  make_node_measure(out, net)
}

#' @rdname measure_diffusion_node 
#' @section Infection length:
#'   `node_infection_length()` measures the average length of time that nodes 
#'   that become infected remain infected in a compartmental model with recovery.
#'   Infections that are not concluded by the end of the study period are
#'   calculated as infinite.
#' @examples
#'   # To measure how long each node remains infected for
#'   node_recovery(smeg_diff)
#' @export
node_recovery <- function(.data){
  if(inherits(.data, "diff_model")){
    net <- attr(.data, "network")
    events <- attr(.data, "events")
    out <- vapply(seq_len(.data$n[1]), 
                  function(x) ifelse("I" %in% dplyr::filter(events, nodes == x)$event,
                                     ifelse("R" %in% dplyr::filter(events, nodes == x)$event,
                                            mean(diff(dplyr::filter(events, nodes == x)$t)),
                                            Inf),
                                     NA),
                  FUN.VALUE = numeric(1))
  } else {
    net <- .data
    events <- as_changelist(.data)
    out <- vapply(seq_len(net_nodes(net)), 
                  function(x) ifelse("I" %in% dplyr::filter(events, node == x)$value,
                                     ifelse("R" %in% dplyr::filter(events, node == x)$value,
                                            mean(diff(dplyr::filter(events, node == x)$time)),
                                            Inf),
                                     NA),
                  FUN.VALUE = numeric(1))
  }
  make_node_measure(out, net)
}

#' @rdname measure_diffusion_node
#' @param mark A valid 'node_mark' object or
#'   logical vector (TRUE/FALSE) of length equal to 
#'   the number of nodes in the network.
#' @param time A time point until which infections/adoptions should be
#'   identified. By default `time = 0`.
#' @section Exposure:
#'   `node_exposure()` calculates the number of infected/adopting nodes
#'   to which each susceptible node is exposed.
#'   It usually expects network data and 
#'   an index or mark (TRUE/FALSE) vector of those nodes which are currently infected,
#'   but if a diff_model is supplied instead it will return
#'   nodes exposure at \eqn{t = 0}.
#' @examples
#'   # To measure how much exposure nodes have to a given mark
#'   node_exposure(smeg, mark = c(1,3))
#'   node_exposure(smeg_diff)
#' @export
node_exposure <- function(.data, mark, time = 0){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(missing(mark)){ 
    if(inherits(.data, "diff_model")){
      mark <- node_is_infected(.data, time = time)
      .data <- attr(.data, "network")
    } else {
      mark <- node_is_infected(.data, time = time)
    }
  }
  .data <- as_tidygraph(.data)
  if(is_weighted(.data) || is_signed(.data)){
    if(is.numeric(mark)){
      mk <- rep(FALSE, net_nodes(.data))
      mk[mark] <- TRUE
    } else mk <- mark
    out <- as_matrix(.data)
    out <- colSums(out * matrix(mk, nrow(out), ncol(out)) * 
                     matrix(!mk, nrow(out), ncol(out), byrow = TRUE))
  } else {
    if(is.logical(mark)) mark <- which(mark)
    if(is_twomode(.data)){
      if(mark[1]>net_dims(.data)[1]){ 
        dat <- to_mode2(.data)
        mark <- mark - net_dims(.data)[1]
        contacts <- unlist(lapply(igraph::neighborhood(dat, nodes = mark, mode = "out"),
                                  function(x) setdiff(x, mark)))
        contacts <- contacts + net_dims(.data)[1]
      } else {
        dat <- to_mode1(.data)
        contacts <- unlist(lapply(igraph::neighborhood(dat),
                                  function(x) setdiff(x, mark)))
        contacts <- contacts + net_dims(.data)[2]
      }
    } else {
      dat <- .data
      contacts <- unlist(lapply(igraph::neighborhood(dat, nodes = mark, mode = "out"),
                                function(x) setdiff(x, mark)))
    }
    # count exposures for each node:
    tabcontact <- table(contacts)
    out <- rep(0, net_nodes(.data))
    out[as.numeric(names(tabcontact))] <- unname(tabcontact)
  }
  make_node_measure(out, .data)
}

# node_diffusion ####

#' Membership of nodes in a diffusion
#' @description
#'   `node_in_adopter()` classifies membership of nodes into diffusion categories
#'   by where on the distribution of adopters they fell.
#'   Valente (1995) defines five memberships:
#'   
#'   - _Early adopter_: those with an adoption time less than 
#'   the average adoption time minus one standard deviation of adoptions times
#'   - _Early majority_: those with an adoption time between
#'   the average adoption time and 
#'   the average adoption time minus one standard deviation of adoptions times
#'   - _Late majority_: those with an adoption time between
#'   the average adoption time and 
#'   the average adoption time plus one standard deviation of adoptions times
#'   - _Laggard_: those with an adoption time greater than 
#'   the average adoption time plus one standard deviation of adoptions times
#'   - _Non-adopter_: those without an adoption time,
#'   i.e. never adopted
#'   
#' @inheritParams mark_is
#' @inheritParams measure_diffusion_net
#' @family measures
#' @family diffusion
#' @name member_diffusion
#' @references
#' ## On adopter classes
#'   Valente, Tom W. 1995. 
#'   _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname member_diffusion
#' @examples
#'   smeg <- generate_smallworld(15, 0.025)
#'   smeg_diff <- play_diffusion(smeg, recovery = 0.2)
#'   # To classify nodes by their position in the adoption curve
#'   (adopts <- node_in_adopter(smeg_diff))
#'   summary(adopts)
#' @export
node_in_adopter <- function(.data){
  toa <- node_adoption_time(.data)
  toa[is.infinite(toa)] <- NA
  avg <- mean(toa, na.rm = TRUE)
  sdv <- stats::sd(toa, na.rm = TRUE)
  out <- ifelse(toa < (avg - sdv) | toa == 0, "Early Adopter", 
                ifelse(toa > (avg + sdv), "Laggard",
                       ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                              ifelse(avg < toa & toa <= avg + sdv, "Late Majority", 
                                     "Non-Adopter"))))
  out[is.na(out)] <- "Non-Adopter"
  if(inherits(.data, "diff_model")) 
    net <- attr(.data, "network") else 
      net <- .data
  make_node_member(out, net)
}
