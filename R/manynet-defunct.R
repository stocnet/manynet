# nocov start
#' Functions that have been renamed, superseded, moved, or are no longer working
#' 
#' Generally these functions have been superseded, renamed, and/or moved.
#' Upon using them, a warning directs the user to the new function.
#' A function that is only superseded still works without a warning
#' for one minor release, since a released package may still call it.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
pkg_data <- function(pkg = "manynet") {
  snet_deprecated("pkg_data", "table_data")
  table_data(pkg = pkg)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_nodes <- function(.data) {
  snet_deprecated("network_nodes", "net_nodes")
  net_nodes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_ties <- function(.data) {
  snet_deprecated("network_ties", "net_ties")
  net_ties(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims <- function(.data) {
  snet_deprecated("network_dims", "mode_nodes")
  mode_nodes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.data.frame <- function(.data) {
  snet_deprecated("network_dims.data.frame", "mode_nodes.data.frame")
  mode_nodes.data.frame(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.matrix <- function(.data) {
  snet_deprecated("network_dims.matrix", "mode_nodes.matrix")
  mode_nodes.matrix(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.igraph <- function(.data) {
  snet_deprecated("network_dims.igraph", "mode_nodes.igraph")
  mode_nodes.igraph(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dims.network <- function(.data) {
  snet_deprecated("network_dims.network", "mode_nodes.network")
  mode_nodes.network(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_node_attributes <- function(.data) {
  snet_deprecated("network_node_attributes", "net_node_attributes")
  net_node_attributes(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_tie_attributes <- function(.data) {
  snet_deprecated("network_tie_attributes", "net_tie_attributes")
  net_tie_attributes(.data)
}

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_mode <- function(.data) {
  snet_deprecated("node_mode", "node_is_mode")
  node_is_mode(.data)
}

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
mutate_net <- function(.data, ...) {
  snet_deprecated("mutate_net", "mutate_info")
  mutate_info(.data, ...)
}

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_info <- function(.data) {
  snet_deprecated("net_info", "as_infolist")
  as_infolist(.data)
}

#' @describeIn defunct Deprecated on 2026-04-02.
#' @export
read_cran <- function(pkg = "all") {
  snet_deprecated("read_cran", "collect_cran")
  collect_cran(pkg = pkg)
}

#' @describeIn defunct Deprecated on 2026-04-02.
#' @export
read_pkg <- function(dir = getwd()) {
  snet_deprecated("read_pkg", "collect_pkg")
  collect_pkg(dir = dir)
}

#' @describeIn defunct Deprecated on 2026-04-02.
#' @export
collect_changes <- function(.data, time) {
  snet_deprecated("collect_changes", "gather_changes")
  gather_changes(.data, time)
}

#' @describeIn defunct Deprecated on 2026-04-04.
#' @export
net_node_names <- function(.data) {
  snet_deprecated("net_node_names", "mode_names")
  mode_names(.data)
}

#' @describeIn defunct Deprecated on 2026-04-04.
#' @export
net_tie_names <- function(.data) {
  snet_deprecated("net_tie_names", "layer_names")
  layer_names(.data)
}

#' @describeIn defunct Deprecated on 2026-06-12.
#' @export
add_changes <- function(.data, changes) {
  snet_deprecated("add_changes", "bind_changes")
  bind_changes(.data, changes)
}

#' @describeIn defunct Deprecated on 2026-08-09.
#' @export
to_ties <- function(.data) {
  snet_deprecated("to_ties", "to_linegraph")
  to_linegraph(.data)
}

#' @describeIn defunct Deprecated on 2026-08-09.
#' @export
to_no_isolates <- function(.data) {
  snet_deprecated("to_no_isolates", "delete_isolates")
  delete_isolates(.data)
}

#' @describeIn defunct Deprecated on 2026-08-09.
#' @export
to_no_missing <- function(.data) {
  snet_deprecated("to_no_missing", "delete_incomplete")
  delete_incomplete(.data)
}

#' @describeIn defunct Deprecated on 2026-08-09.
#' @export
to_blocks <- function(.data, membership, FUN = mean) {
  snet_deprecated("to_blocks", "to_blockmodel")
  to_blockmodel(.data, membership, FUN)
}


# Moved to netrics from 2.0.0 ####

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_degree <- function(...) snet_moved("node_degree", "node_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_deg <- function(...) snet_moved("node_deg", "node_by_deg", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_quad <- function(...) snet_moved("net_by_quad", "net_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_hazard <- function(...) snet_moved("net_hazard", "net_x_hazard", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_automorphic_equivalence <- function(...) snet_moved("node_automorphic_equivalence", "node_in_automorphic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokerage_census <- function(...) snet_moved("node_brokerage_census", "node_x_brokerage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokering <- function(...) snet_moved("node_brokering", "node_in_brokering", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_quad <- function(...) snet_moved("node_by_quad", "node_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_components <- function(...) snet_moved("node_components", "node_in_component", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_core <- function(...) snet_moved("node_core", "node_in_core", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_edge_betweenness <- function(...) snet_moved("node_edge_betweenness", "node_in_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_equivalence <- function(...) snet_moved("node_equivalence", "node_in_equivalence", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_fast_greedy <- function(...) snet_moved("node_fast_greedy", "node_in_greedy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_fluid <- function(...) snet_moved("node_fluid", "node_in_fluid", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_infomap <- function(...) snet_moved("node_infomap", "node_in_infomap", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_kernighanlin <- function(...) snet_moved("node_kernighanlin", "node_in_partition", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_leading_eigen <- function(...) snet_moved("node_leading_eigen", "node_in_eigen", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_leiden <- function(...) snet_moved("node_leiden", "node_in_leiden", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_louvain <- function(...) snet_moved("node_louvain", "node_in_louvain", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_optimal <- function(...) snet_moved("node_optimal", "node_in_optimal", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_path_census <- function(...) snet_moved("node_path_census", "node_x_path", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_quad_census <- function(...) snet_moved("node_quad_census", "node_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_regular_equivalence <- function(...) snet_moved("node_regular_equivalence", "node_in_regular", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_roulette <- function(...) snet_moved("node_roulette", "node_in_roulette", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_spinglass <- function(...) snet_moved("node_spinglass", "node_in_spinglass", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_strong_components <- function(...) snet_moved("node_strong_components", "node_in_strong", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_structural_equivalence <- function(...) snet_moved("node_structural_equivalence", "node_in_structural", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_tie_census <- function(...) snet_moved("node_tie_census", "node_x_tie", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_triad_census <- function(...) snet_moved("node_triad_census", "node_x_triad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_walktrap <- function(...) snet_moved("node_walktrap", "node_in_walktrap", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_weak_components <- function(...) snet_moved("node_weak_components", "node_in_weak", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_adhesion <- function(...) snet_moved("net_adhesion", "net_by_adhesion", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_assortativity <- function(...) snet_moved("net_assortativity", "net_by_assortativity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_balance <- function(...) snet_moved("net_balance", "net_by_balance", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_betweenness <- function(...) snet_moved("net_betweenness", "net_by_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_change <- function(...) snet_moved("net_change", "net_x_change", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_closeness <- function(...) snet_moved("net_closeness", "net_by_closeness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_cohesion <- function(...) snet_moved("net_cohesion", "net_by_cohesion", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_components <- function(...) snet_moved("net_components", "net_by_components", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_congruency <- function(...) snet_moved("net_congruency", "net_by_congruency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_connectedness <- function(...) snet_moved("net_connectedness", "net_by_connectedness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_core <- function(...) snet_moved("net_core", "net_by_core", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_correlation <- function(...) snet_moved("net_correlation", "net_x_correlation", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_degree <- function(...) snet_moved("net_degree", "net_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_density <- function(...) snet_moved("net_density", "net_by_density", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_diameter <- function(...) snet_moved("net_diameter", "net_by_diameter", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_diversity <- function(...) snet_moved("net_diversity", "net_by_diversity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_efficiency <- function(...) snet_moved("net_efficiency", "net_by_efficiency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_eigenvector <- function(...) snet_moved("net_eigenvector", "net_by_eigenvector", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_equivalency <- function(...) snet_moved("net_equivalency", "net_by_equivalency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_factions <- function(...) snet_moved("net_factions", "net_by_factions", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_harmonic <- function(...) snet_moved("net_harmonic", "net_by_harmonic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_heterophily <- function(...) snet_moved("net_heterophily", "net_by_heterophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_homophily <- function(...) snet_moved("net_homophily", "net_by_homophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_immunity <- function(...) snet_moved("net_immunity", "net_by_immunity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_indegree <- function(...) snet_moved("net_indegree", "net_by_indegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_independence <- function(...) snet_moved("net_independence", "net_by_independence", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_infection_complete <- function(...) snet_moved("net_infection_complete", "net_by_infection_complete", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_infection_peak <- function(...) snet_moved("net_infection_peak", "net_by_infection_peak", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_infection_total <- function(...) snet_moved("net_infection_total", "net_by_infection_total", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_length <- function(...) snet_moved("net_length", "net_by_length", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_modularity <- function(...) snet_moved("net_modularity", "net_by_modularity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_outdegree <- function(...) snet_moved("net_outdegree", "net_by_outdegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_reach <- function(...) snet_moved("net_reach", "net_by_reach", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_reciprocity <- function(...) snet_moved("net_reciprocity", "net_by_reciprocity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_recovery <- function(...) snet_moved("net_recovery", "net_by_recovery", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_reproduction <- function(...) snet_moved("net_reproduction", "net_by_reproduction", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_richclub <- function(...) snet_moved("net_richclub", "net_by_richclub", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_richness <- function(...) snet_moved("net_richness", "net_by_richness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_scalefree <- function(...) snet_moved("net_scalefree", "net_by_scalefree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_smallworld <- function(...) snet_moved("net_smallworld", "net_by_smallworld", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_spatial <- function(...) snet_moved("net_spatial", "net_by_spatial", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_stability <- function(...) snet_moved("net_stability", "net_x_stability", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_strength <- function(...) snet_moved("net_strength", "net_by_strength", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_toughness <- function(...) snet_moved("net_toughness", "net_by_toughness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_transitivity <- function(...) snet_moved("net_transitivity", "net_by_transitivity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_transmissibility <- function(...) snet_moved("net_transmissibility", "net_by_transmissibility", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_upperbound <- function(...) snet_moved("net_upperbound", "net_by_upperbound", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_adoption_time <- function(...) snet_moved("node_adoption_time", "node_by_adopt_time", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_alpha <- function(...) snet_moved("node_alpha", "node_by_alpha", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_authority <- function(...) snet_moved("node_authority", "node_by_authority", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_betweenness <- function(...) snet_moved("node_betweenness", "node_by_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_bridges <- function(...) snet_moved("node_bridges", "node_by_bridges", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_closeness <- function(...) snet_moved("node_closeness", "node_by_closeness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_constraint <- function(...) snet_moved("node_constraint", "node_by_constraint", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_coreness <- function(...) snet_moved("node_coreness", "node_by_coreness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_distance <- function(...) snet_moved("node_distance", "node_by_distance", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_diversity <- function(...) snet_moved("node_diversity", "node_by_diversity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_eccentricity <- function(...) snet_moved("node_eccentricity", "node_by_eccentricity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_efficiency <- function(...) snet_moved("node_efficiency", "node_by_efficiency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_effsize <- function(...) snet_moved("node_effsize", "node_by_effsize", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_eigenvector <- function(...) snet_moved("node_eigenvector", "node_by_eigenvector", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_equivalency <- function(...) snet_moved("node_equivalency", "node_by_equivalency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_exposure <- function(...) snet_moved("node_exposure", "node_by_adopt_exposure", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_flow <- function(...) snet_moved("node_flow", "node_by_flow", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_harmonic <- function(...) snet_moved("node_harmonic", "node_by_harmonic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_heterophily <- function(...) snet_moved("node_heterophily", "node_by_heterophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_hierarchy <- function(...) snet_moved("node_hierarchy", "node_by_hierarchy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_homophily <- function(...) snet_moved("node_homophily", "node_by_homophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_hub <- function(...) snet_moved("node_hub", "node_by_hub", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_indegree <- function(...) snet_moved("node_indegree", "node_by_indegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_induced <- function(...) snet_moved("node_induced", "node_by_induced", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_information <- function(...) snet_moved("node_information", "node_by_information", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_kcoreness <- function(...) snet_moved("node_kcoreness", "node_by_kcoreness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_leverage <- function(...) snet_moved("node_leverage", "node_by_leverage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_multidegree <- function(...) snet_moved("node_multidegree", "node_by_multidegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_neighbours_degree <- function(...) snet_moved("node_neighbours_degree", "node_by_neighbours_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_outdegree <- function(...) snet_moved("node_outdegree", "node_by_outdegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_pagerank <- function(...) snet_moved("node_pagerank", "node_by_pagerank", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_posneg <- function(...) snet_moved("node_posneg", "node_by_posneg", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_power <- function(...) snet_moved("node_power", "node_by_power", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_randomwalk <- function(...) snet_moved("node_randomwalk", "node_by_randomwalk", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_reach <- function(...) snet_moved("node_reach", "node_by_reach", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_reciprocity <- function(...) snet_moved("node_reciprocity", "node_by_reciprocity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_recovery <- function(...) snet_moved("node_recovery", "node_by_adopt_recovery", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_redundancy <- function(...) snet_moved("node_redundancy", "node_by_redundancy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_richness <- function(...) snet_moved("node_richness", "node_by_richness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_stress <- function(...) snet_moved("node_stress", "node_by_stress", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_subgraph <- function(...) snet_moved("node_subgraph", "node_by_subgraph", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_thresholds <- function(...) snet_moved("node_thresholds", "node_by_adopt_threshold", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_transitivity <- function(...) snet_moved("node_transitivity", "node_by_transitivity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_vitality <- function(...) snet_moved("node_vitality", "node_by_vitality", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_betweenness <- function(...) snet_moved("tie_betweenness", "tie_by_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_closeness <- function(...) snet_moved("tie_closeness", "tie_by_closeness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_cohesion <- function(...) snet_moved("tie_cohesion", "tie_by_cohesion", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_degree <- function(...) snet_moved("tie_degree", "tie_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_eigenvector <- function(...) snet_moved("tie_eigenvector", "tie_by_eigenvector", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_brokerage <- function(...) snet_moved("net_by_brokerage", "net_x_brokerage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_dyad <- function(...) snet_moved("net_by_dyad", "net_x_dyad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_hazard <- function(...) snet_moved("net_by_hazard", "net_x_hazard", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_hierarchy <- function(...) snet_moved("net_by_hierarchy", "net_x_hierarchy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_mixed <- function(...) snet_moved("net_by_mixed", "net_x_mixed", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_tetrad <- function(...) snet_moved("net_by_tetrad", "net_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_triad <- function(...) snet_moved("net_by_triad", "net_x_triad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokering_activity <- function(...) snet_moved("node_brokering_activity", "node_by_brokering_activity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokering_exclusivity <- function(...) snet_moved("node_brokering_exclusivity", "node_by_brokering_exclusivity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_brokerage <- function(...) snet_moved("node_by_brokerage", "node_x_brokerage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_dyad <- function(...) snet_moved("node_by_dyad", "node_x_dyad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_exposure <- function(...) snet_moved("node_by_exposure", "node_x_exposure", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_path <- function(...) snet_moved("node_by_path", "node_x_path", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_tetrad <- function(...) snet_moved("node_by_tetrad", "node_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_tie <- function(...) snet_moved("node_by_tie", "node_x_tie", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_triad <- function(...) snet_moved("node_by_triad", "node_x_triad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-08-18.
#' @export
na_to_zero <- function(.data) {
  snet_deprecated("na_to_zero", "impute_ties")
  impute_ties(.data, rule = "zero")
}

#' @describeIn defunct Deprecated on 2026-08-18.
#' @export
na_to_mean <- function(.data) {
  snet_deprecated("na_to_mean", "impute_ties")
  # It did one of two jobs depending on the network: it filled an unknown
  # value with the observed average where the network was weighted, and drew a
  # missing tie at the observed density where it was not. Each of those is now
  # a rule of its own.
  if (is_weighted(.data)) impute_ties(.data, rule = "mean") else
    impute_ties(.data, rule = "density")
}

#' @describeIn defunct Deprecated on 2026-11-09.
#'   `play_segregation()` is now `play_mobility()`,
#'   with `heterophily` renamed to `preference`.
#'   It gives the same results as before for a seed,
#'   and so does not move the occupant's other attributes.
#' @export
play_segregation <- function(.data,
                             attribute,
                             heterophily = 0,
                             who_moves = c("ordered","random","most_dissatisfied"),
                             choice_function = c("satisficing","optimising", "minimising"),
                             steps) {
  snet_deprecated("play_segregation", "play_mobility")
  # This keeps the former code, rather than calling `play_mobility()`,
  # so that a seed gives the same result as before.
  # `play_mobility()` differs in three ways:
  # it does not skip a place that an occupant has left,
  # it does not draw from 1:k when only place k satisfies,
  # and it checks a mover's threshold rather than that of the empty place.
  n <- net_nodes(.data)
  if(missing(steps)) steps <- n
  who_moves <- match.arg(who_moves)
  choice_function <- match.arg(choice_function)
  if(length(heterophily)==1) heterophily <- rep(heterophily, n)
  if(length(heterophily)!=n) snet_abort("Heterophily threshold must be the same length as the number of nodes in the network.")
  swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x}

  t <- 0
  temp <- .data
  moved <- NULL
  while(steps > t){
    t <- t+1
    current <- node_attribute(temp, attribute)
    heterophily_scores <- .node_heterophily(temp, attribute)
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
      .node_heterophily(test, "test")[u]
    }, FUN.VALUE = numeric(1))
    if(length(options)==0) next
    move_to <- switch(choice_function,
                      satisficing = unoccupied[sample(which(options <= heterophily[unoccupied]), 1)],
                      optimising = unoccupied[which.min(options)[1]],
                      minimising = unoccupied[which.min(igraph::distances(temp,
                                                                          igraph::V(temp)[dissatisfied],
                                                                          igraph::V(temp)[unoccupied]))])
    if(is.na(move_to)) next
    snet_minor_info("Moving node {dissatisfied} to node {move_to}")
    temp <- add_node_attribute(temp, attribute,
                                        swtch(current, dissatisfied, move_to))
    moved <- c(dissatisfied, moved)
  }
  temp
}

#' @describeIn defunct Deprecated on 2026-11-09.
#'   Use `play_opinion()`, whose `opinions`, `confidence`, and `tolerance`
#'   replace `beliefs`, `closeness`, and `epsilon`.
#'   A finite `closeness` plays the `"hegselmann_krause"` rule,
#'   and the result is the table that `as_opinion()` returns.
#' @param beliefs A vector of the opinions of the nodes at the start.
#' @param closeness A threshold at which beliefs are too different to
#'   influence each other. By default `Inf`, i.e. there is no threshold.
#' @param epsilon The maximum difference in beliefs accepted
#'   for convergence to a consensus.
#' @export
play_learning <- function(.data,
                          beliefs,
                          closeness = Inf,
                          steps,
                          epsilon = 0.0005){
  snet_deprecated("play_learning", "play_opinion")
  n <- net_nodes(.data)
  if(length(beliefs) != n)
    snet_abort("{.arg beliefs} must be a vector the same length as the number",
               "of nodes in the network.")
  if(missing(steps)) steps <- n
  rule <- if(is.finite(closeness)) "hegselmann_krause" else "degroot"
  # A stocnet holds the record whatever the class given, as a matrix cannot
  out <- .play_opinion(as_stocnet(.data), beliefs, rule, stubbornness = 0,
                       confidence = closeness, weights = NULL, steps = steps,
                       tolerance = epsilon, record = 0)
  as_opinion(out, "opinions")
}

# nocov end

#' @describeIn defunct Deprecated on 2026-08-20.
#' @param .data A manynet-consistent network.
#' @param time A moment to gather the changes in force at.
#' @export
gather_changes <- function(.data, time) {
  snet_deprecated("gather_changes", "as_changelist")
  as_changelist(.data, time = time)
}

#' @describeIn defunct Deprecated on 2026-08-20.
#' @export
apply_changes <- function(.data, time) {
  snet_deprecated("apply_changes", "to_time")
  .apply_changes_upto(.data, as_changelist(.data), time)
}

#' @describeIn defunct Deprecated on 2026-09-21.
#' @export
is_egonet <- function(.data) {
  snet_deprecated("is_egonet", "is_egolist")
  is_egolist(.data)
}

#' @rdname defunct
#' @param x A network of the 'mnet' class, saved by a version before 2.5.0.
#' @param n Number of observations to print across all network components.
#' @section The 'mnet' class:
#'   Deprecated on 2026-11-08.
#'   manynet 2.5.0 no longer makes 'mnet' objects:
#'   `as_tidygraph()` returns a plain 'tbl_graph'.
#'   A network saved with the class still prints as before, with a warning;
#'   `as_stocnet()` converts it.
#' @export
print.mnet <- function(x, ..., n = 12) {
  snet_deprecated(msg = paste("The 'mnet' class is deprecated.",
                              "Use {.fn as_stocnet} or {.fn as_tidygraph} to convert this network."))
  if(!is.null(net_name(x)) && net_name(x) != "")
    cli::cli_h1("# {net_name(x)}")
  net_desc <- describe_network(x)
  tie_desc <- describe_ties(x)
  node_desc <- describe_nodes(x)
  change_desc <- describe_changes(x)
  cli::cli_par()
  cli_div(theme = list(.emph = list(color = "#4576B5")))
  cli::cli_text("{.emph # {net_desc} of {node_desc} and {tie_desc}{change_desc}}")
  cli::cli_end()
  top <- dplyr::as_tibble(tidygraph::activate(x, "nodes"))
  bottom <- dplyr::as_tibble(tidygraph::activate(x, "edges"))
  if(!is.null(igraph::graph_attr(x, "changes"))) n <- ceiling(n/3) else
    n <- ceiling(n/2)
  if (ncol(top)>0){
    cli::cli_par()
    cli::cli_h3("Nodes")
    print(top, n = n)
    cli::cli_end()
  }
  if(!is.null(igraph::graph_attr(x, "changes"))){
    cli::cli_par()
    cli::cli_h3("Changes")
    print(dplyr::as_tibble(igraph::graph_attr(x, "changes")), n = n)
    cli::cli_end()
  }
  if (ncol(bottom)>0){
    cli::cli_h3("Ties")
    print(bottom, n = n, max_footer_lines = 1)
  }
  invisible(x)
}

# Superseded in 2.5.0 (#182) ####
# These old names still work without a warning, since infernet and migraph
# 1.7.2 call some of them and are released apart from manynet. They warn from
# 2.6.0, once those packages use the new names.

#' @describeIn defunct Superseded by `is_looped()` on 2026-11-12.
#'   This name gives a warning from 2.6.0.
#' @export
is_complex <- function(.data) {
  snet_deprecated("is_complex", "is_looped", "superseded")
  is_looped(.data)
}

#' @describeIn defunct Superseded by `to_unlooped()` on 2026-11-12.
#'   This name gives a warning from 2.6.0.
#' @export
to_simplex <- function(.data) {
  snet_deprecated("to_simplex", "to_unlooped", "superseded")
  to_unlooped(.data)
}

#' @describeIn defunct Superseded by `to_layer()` on 2026-11-12.
#'   This name gives a warning from 2.6.0.
#' @export
to_uniplex <- function(.data, layer, tie) {
  snet_deprecated("to_uniplex", "to_layer", "superseded")
  if(!missing(tie)) return(to_layer(.data, tie = tie))
  if(missing(layer)) to_layer(.data) else to_layer(.data, layer)
}

#' @describeIn defunct Superseded by `to_aggregated()` on 2026-11-12,
#'   which combines layers by default and takes the `rule` third.
#'   This name gives a warning from 2.6.0.
#' @export
to_flat <- function(.data, rule = c("max","min","mean","sum","product"),
                    over = "layer") {
  snet_deprecated("to_flat", "to_aggregated", "superseded")
  to_aggregated(.data, over = over, rule = match.arg(rule))
}
