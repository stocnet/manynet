# Brokerage ####

#' Motifs of brokerage
#' 
#' @description
#'   These functions include ways to take a census of the brokerage positions of nodes
#'   in a network: 
#'   
#'   - `node_by_brokerage()` returns the Gould-Fernandez brokerage
#'   roles played by nodes in a network.
#'   - `net_by_brokerage()` returns the Gould-Fernandez brokerage
#'   roles in a network.
#'   - `node_brokering_activity()` measures nodes' brokerage activity.
#'   - `node_brokering_exclusivity()` measures nodes' brokerage exclusivity. 
#'   
#' @name motif_brokerage
#' @family motifs
#' @inheritParams motif_node
#' @param membership A vector of partition membership as integers.
#' @param standardized Whether the score should be standardized
#'   into a _z_-score indicating how many standard deviations above
#'   or below the average the score lies.
NULL

#' @rdname motif_brokerage 
#' @references 
#' ## On brokerage motifs
#' Gould, Roger V., and Roberto M. Fernandez. 1989. 
#' “Structures of Mediation: A Formal Approach to Brokerage in Transaction Networks.” 
#' _Sociological Methodology_, 19: 89-126.
#' \doi{10.2307/270949}
#' 
#' Jasny, Lorien, and Mark Lubell. 2015. 
#' “Two-Mode Brokerage in Policy Networks.” 
#' _Social Networks_ 41:36–47. 
#' \doi{10.1016/j.socnet.2014.11.005}
#' @examples 
#' # node_by_brokerage(ison_networkers, "Discipline")
#' @export
node_by_brokerage <- function(.data, membership, standardized = FALSE){
  thisRequires("sna")
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(!manynet::is_twomode(.data)){
    out <- sna::brokerage(manynet::as_network(.data),
                          manynet::node_attribute(.data, membership))
    out <- if(standardized) out$z.nli else out$raw.nli
    colnames(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                       "Representative", "Liaison", "Total")
  } else {
    out <- suppressWarnings(sna::brokerage(manynet::as_network(manynet::to_mode1(.data)),
                                           manynet::node_attribute(.data, membership)))
    out <- if(standardized) out$z.nli else out$raw.nli
    out <- out[,-4]
    colnames(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                       "Liaison", "Total")
  }
  make_node_motif(out, .data)
}

#' @rdname motif_brokerage 
#' @examples 
#' # net_by_brokerage(ison_networkers, "Discipline")
#' @export
net_by_brokerage <- function(.data, membership, standardized = FALSE){
  thisRequires("sna")
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  if(!manynet::is_twomode(.data)){
    out <- sna::brokerage(manynet::as_network(.data),
                          manynet::node_attribute(.data, membership))
    out <- if(standardized) out$z.gli else out$raw.gli
    names(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                    "Representative", "Liaison", "Total")
  } else {
    out <- suppressWarnings(sna::brokerage(manynet::as_network(manynet::to_mode1(.data)),
                                           manynet::node_attribute(.data, membership)))
    out <- if(standardized) out$z.gli else out$raw.gli
    names(out) <- c("Coordinator", "Itinerant", "Gatekeeper", 
                    "Representative", "Liaison", "Total")
  }
  make_network_motif(out, .data)
}

#' @rdname motif_brokerage 
#' @references
#' ## On brokerage activity and exclusivity
#'   Hamilton, Matthew, Jacob Hileman, and Orjan Bodin. 2020.
#'   "Evaluating heterogeneous brokerage: New conceptual and methodological approaches
#'   and their application to multi-level environmental governance networks"
#'   _Social Networks_ 61: 1-10.
#'   \doi{10.1016/j.socnet.2019.08.002}
#' @export
node_brokering_activity <- function(.data, membership){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  from <- to.y <- to_memb <- from_memb <- NULL
  twopaths <- .to_twopaths(.data)
  if(!missing(membership)){
    twopaths$from_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                                          match(twopaths$from, manynet::node_names(.data)),
                                                                          twopaths$from)]
    twopaths$to_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                                        match(twopaths$to.y, manynet::node_names(.data)),
                                                                        twopaths$to.y)]
    twopaths <- dplyr::filter(twopaths, from_memb != to_memb)
  }
  # tabulate brokerage
  out <- c(table(twopaths$to))
  # correct ordering for named data
  if(manynet::is_labelled(.data)) out <- out[match(manynet::node_names(.data), names(out))]
  # missings should be none
  out[is.na(out)] <- 0
  make_node_measure(out, .data)
}

#' @rdname motif_brokerage
#' @examples
#' node_brokering_exclusivity(ison_networkers, "Discipline")
#' @export
node_brokering_exclusivity <- function(.data, membership){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  from <- to.y <- to_memb <- from_memb <- NULL
  twopaths <- .to_twopaths(.data)
  if(!missing(membership)){
    twopaths$from_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                                          match(twopaths$from, manynet::node_names(.data)),
                                                                          twopaths$from)]
    twopaths$to_memb <- manynet::node_attribute(.data, membership)[`if`(manynet::is_labelled(.data),
                                                                        match(twopaths$to.y, manynet::node_names(.data)),
                                                                        twopaths$to.y)]
    twopaths <- dplyr::filter(twopaths, from_memb != to_memb)
  }
  # get only exclusive paths
  out <- twopaths %>% dplyr::group_by(from, to.y) %>% dplyr::filter(dplyr::n()==1)
  # tabulate brokerage
  out <- c(table(out$to))
  # correct ordering for named data
  if(manynet::is_labelled(.data)) out <- out[match(manynet::node_names(.data), names(out))]
  # missings should be none
  out[is.na(out)] <- 0
  make_node_measure(out, .data)
}

#' Memberships of brokerage
#' 
#' @description
#'   These functions include ways to take a census of the brokerage positions of nodes
#'   in a network: 
#'   
#'   - `node_in_brokerage()` returns nodes membership as a powerhouse,
#'   connector, linchpin, or sideliner according to Hamilton et al. (2020).
#'   
#' @name member_brokerage
#' @family memberships
#' @inheritParams motif_brokerage
NULL

#' @rdname member_brokerage 
#' @export
node_in_brokering <- function(.data, membership){
  if(missing(.data)) {expect_nodes(); .data <- .G()} # nocov
  activ <- node_brokering_activity(.data, membership)
  exclusiv <- node_brokering_exclusivity(.data, membership)
  activ <- activ - mean(activ)
  exclusiv <- exclusiv - mean(exclusiv)
  out <- dplyr::case_when(activ > 0 & exclusiv > 0 ~ "Powerhouse",
                          activ > 0 & exclusiv < 0 ~ "Connectors",
                          activ < 0 & exclusiv > 0 ~ "Linchpins",
                          activ < 0 & exclusiv < 0 ~ "Sideliners")
  make_node_member(out, .data)
}

.to_twopaths <- function(.data){
  to <- from <- to.y <- NULL
  if(!manynet::is_directed(.data)){
    el <- manynet::as_edgelist(manynet::to_reciprocated(.data)) 
  } else el <- manynet::as_edgelist(.data)
  twopaths <- dplyr::full_join(el, el, 
                               by = dplyr::join_by(to == from), 
                               relationship = "many-to-many")
  # remove non two-paths
  twopaths <- dplyr::filter(twopaths, !(is.na(from) | is.na(to.y)))
  # remove reciprocated paths
  twopaths <- dplyr::filter(twopaths, from != to.y)
  # remove triads
  twopaths <- dplyr::filter(twopaths, !paste(from, to.y) %in% paste(from, to))
  twopaths
}
