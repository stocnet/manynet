# nocov start
#' Functions that have been renamed, superseded, moved, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded, renamed, and/or moved.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

fn_moved <- function(old, new, old_pkg = "manynet", new_pkg = "netrics", 
                     version = NULL) {
  if(!is.null(version)) version <- paste0(" ", version)
  if(is.null(version)) preposition <- "out of" else preposition <- "since"
  if(!nzchar(system.file(package = new_pkg))) {
    cli::cli_inform(c(i = "{.fn {old}} was moved {preposition} {.pkg {old_pkg}}{.version {version}} to {.pkg {new_pkg}}."))
    cli::cli_inform("Install {.pkg {new_pkg}} with: {.code install.packages('{new_pkg}')}")
  } else {
    new <- paste0(new_pkg, "::", new)
    cli::cli_inform(c(i = "{.fn {old}} was moved {preposition} {.pkg {old_pkg}}{.version {version}}. Please use {.fn {new}}"))
  }
  stop("Please update your code to use the new function.", call. = FALSE)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
pkg_data <- function(pkg = "manynet") {
  .Deprecated("table_data", package = "migraph",
              old = "pkg_data")
  table_data(pkg = pkg)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_nodes <- function(.data) {
  .Deprecated("net_nodes", package = "manynet",
              old = "network_nodes")
  net_nodes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_ties <- function(.data) {
  .Deprecated("net_ties", package = "manynet",
              old = "network_ties")
  net_ties(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims <- function(.data) {
  .Deprecated("net_dims", package = "manynet",
              old = "network_dims")
  net_dims(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.data.frame <- function(.data) {
  .Deprecated("net_dims.data.frame", package = "manynet",
              old = "network_dims.data.frame")
  net_dims.data.frame(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.matrix <- function(.data) {
  .Deprecated("net_dims.matrix", package = "manynet",
              old = "network_dims.matrix")
  net_dims.matrix(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.igraph <- function(.data) {
  .Deprecated("net_dims.igraph", package = "manynet",
              old = "network_dims.igraph")
  net_dims.igraph(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.network <- function(.data) {
  .Deprecated("net_dims.network", package = "manynet",
              old = "network_dims.network")
  net_dims.network(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_node_attributes <- function(.data) {
  .Deprecated("net_node_attributes", package = "manynet",
              old = "network_node_attributes")
  net_node_attributes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_tie_attributes <- function(.data) {
  .Deprecated("net_tie_attributes", package = "manynet",
              old = "network_tie_attributes")
  net_tie_attributes(.data)
}

# Moved to netrics from 2.0.0 ####

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_degree <- function(...) fn_moved("node_degree", "node_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_deg <- function(...) fn_moved("node_deg", "node_by_deg", version = "2.0.0")

export(net_by_quad)
export(net_hazard)
export(node_automorphic_equivalence)
export(node_brokerage_census)
export(node_brokering)
export(node_by_quad)
export(node_components)
export(node_core)
export(node_edge_betweenness)
export(node_equivalence)
export(node_fast_greedy)
export(node_fluid)
export(node_infomap)
export(node_kernighanlin)
export(node_leading_eigen)
export(node_leiden)
export(node_louvain)
export(node_mode)
export(node_optimal)
export(node_path_census)
export(node_quad_census)
export(node_regular_equivalence)
export(node_roulette)
export(node_spinglass)
export(node_strong_components)
export(node_structural_equivalence)
export(node_tie_census)
export(node_triad_census)
export(node_walktrap)
export(node_weak_components)
export(node_is_core)
export(node_is_cutpoint)
export(node_is_exposed)
export(node_is_fold)
export(node_is_independent)
export(node_is_infected)
export(node_is_isolate)
export(node_is_latent)
export(node_is_max)
export(node_is_mean)
export(node_is_mentor)
export(node_is_min)
export(node_is_neighbor)
export(node_is_pendant)
export(node_is_random)
export(node_is_recovered)
export(node_is_universal)
export(tie_is_bridge)
export(tie_is_cyclical)
export(tie_is_feedback)
export(tie_is_forbidden)
export(tie_is_imbalanced)
export(tie_is_loop)
export(tie_is_max)
export(tie_is_min)
export(tie_is_multiple)
export(tie_is_path)
export(tie_is_random)
export(tie_is_reciprocated)
export(tie_is_simmelian)
export(tie_is_transitive)
export(tie_is_triangular)
export(tie_is_triplet)
export(net_adhesion)
export(net_assortativity)
export(net_balance)
export(net_betweenness)
export(net_change)
export(net_closeness)
export(net_cohesion)
export(net_components)
export(net_congruency)
export(net_connectedness)
export(net_core)
export(net_correlation)
export(net_degree)
export(net_density)
export(net_diameter)
export(net_diversity)
export(net_efficiency)
export(net_eigenvector)
export(net_equivalency)
export(net_factions)
export(net_harmonic)
export(net_heterophily)
export(net_homophily)
export(net_immunity)
export(net_indegree)
export(net_independence)
export(net_infection_complete)
export(net_infection_peak)
export(net_infection_total)
export(net_length)
export(net_modularity)
export(net_outdegree)
export(net_reach)
export(net_reciprocity)
export(net_recovery)
export(net_reproduction)
export(net_richclub)
export(net_richness)
export(net_scalefree)
export(net_smallworld)
export(net_spatial)
export(net_stability)
export(net_strength)
export(net_toughness)
export(net_transitivity)
export(net_transmissibility)
export(net_upperbound)
export(net_waves)
export(node_adoption_time)
export(node_alpha)
export(node_authority)
export(node_betweenness)
export(node_bridges)
export(node_closeness)
export(node_constraint)
export(node_coreness)
export(node_deg)
export(node_degree)
export(node_distance)
export(node_diversity)
export(node_eccentricity)
export(node_efficiency)
export(node_effsize)
export(node_eigenvector)
export(node_equivalency)
export(node_exposure)
export(node_flow)
export(node_harmonic)
export(node_heterophily)
export(node_hierarchy)
export(node_homophily)
export(node_hub)
export(node_indegree)
export(node_induced)
export(node_information)
export(node_kcoreness)
export(node_leverage)
export(node_multidegree)
export(node_neighbours_degree)
export(node_outdegree)
export(node_pagerank)
export(node_posneg)
export(node_power)
export(node_randomwalk)
export(node_reach)
export(node_reciprocity)
export(node_recovery)
export(node_redundancy)
export(node_richness)
export(node_stress)
export(node_subgraph)
export(node_thresholds)
export(node_transitivity)
export(node_vitality)
export(tie_betweenness)
export(tie_closeness)
export(tie_cohesion)
export(tie_degree)
export(tie_eigenvector)
export(node_in_adopter)
export(node_in_automorphic)
export(node_in_betweenness)
export(node_in_brokering)
export(node_in_community)
export(node_in_component)
export(node_in_core)
export(node_in_eigen)
export(node_in_equivalence)
export(node_in_fluid)
export(node_in_greedy)
export(node_in_infomap)
export(node_in_leiden)
export(node_in_louvain)
export(node_in_optimal)
export(node_in_partition)
export(node_in_regular)
export(node_in_roulette)
export(node_in_spinglass)
export(node_in_strong)
export(node_in_structural)
export(node_in_walktrap)
export(node_in_weak)
export(net_by_brokerage)
export(net_by_dyad)
export(net_by_hazard)
export(net_by_hierarchy)
export(net_by_mixed)
export(net_by_tetrad)
export(net_by_triad)
export(node_brokering_activity)
export(node_brokering_exclusivity)
export(node_by_brokerage)
export(node_by_dyad)
export(node_by_exposure)
export(node_by_path)
export(node_by_tetrad)
export(node_by_tie)
export(node_by_triad)



# nocov end