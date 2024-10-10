#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
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
  .Deprecated("table_data", package = "migraph",
              old = "pkg_data")
  table_data(pkg = pkg)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
node_mode <- function(.data) {
  .Deprecated("node_is_mode", package = "migraph",
              old = "node_mode")
  node_is_mode(.data)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
autographr <- function(.data, layout, labels = TRUE,
    node_color, node_shape, node_size, node_group,
    edge_color, edge_size, ...) {
  .Deprecated("graphr", package = "migraph",
              old = "autographr")
  graphr(.data, layout, labels,
         node_color, node_shape, node_size, node_group,
         edge_color, edge_size, ...)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
autographs <- function(netlist, waves, based_on = c("first", "last", "both"), ...) {
  .Deprecated("graphs", package = "migraph",
              old = "autographs")
  graphs(netlist, waves, based_on, ...)
}

#' @describeIn defunct Deprecated on 2024-06-17.
#' @export
autographd <- function(tlist, layout, labels = TRUE,
    node_color, node_shape, node_size, edge_color, edge_size,
    keep_isolates = TRUE, ...) {
  .Deprecated("grapht", package = "migraph",
              old = "autographd")
  grapht(tlist, layout, labels,
         node_color, node_shape, node_size, edge_color, edge_size,
         keep_isolates, ...)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_optimal <- function(.data) {
  .Deprecated("node_in_optimal", package = "migraph",
              old = "node_optimal")
  node_in_optimal(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_kernighanlin <- function(.data) {
  .Deprecated("node_in_partition", package = "migraph",
              old = "node_kernighanlin")
  node_in_partition(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_edge_betweenness <- function(.data) {
  .Deprecated("node_in_betweenness", package = "migraph",
              old = "node_edge_betweenness")
  node_in_betweenness(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_fast_greedy <- function(.data) {
  .Deprecated("node_in_greedy", package = "migraph",
              old = "node_fast_greedy")
  node_in_greedy(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_leading_eigen <- function(.data) {
  .Deprecated("node_in_eigen", package = "migraph",
              old = "node_leading_eigen")
  node_in_eigen(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_walktrap <- function(.data) {
  .Deprecated("node_in_walktrap", package = "migraph",
              old = "node_walktrap")
  node_in_walktrap(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_infomap <- function(.data) {
  .Deprecated("node_in_infomap", package = "migraph",
              old = "node_infomap")
  node_in_infomap(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_spinglass <- function(.data) {
  .Deprecated("node_in_spinglass", package = "migraph",
              old = "node_spinglass")
  node_in_spinglass(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_fluid <- function(.data) {
  .Deprecated("node_in_fluid", package = "migraph",
              old = "node_fluid")
  node_in_fluid(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_leiden <- function(.data) {
  .Deprecated("node_in_leiden", package = "migraph",
              old = "node_leiden")
  node_in_leiden(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_louvain <- function(.data) {
  .Deprecated("node_in_louvain", package = "migraph",
              old = "node_louvain")
  node_in_louvain(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_roulette <- function(.data) {
  .Deprecated("node_in_roulette", package = "migraph",
              old = "node_roulette")
  node_in_roulette(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_components <- function(.data) {
  .Deprecated("node_in_component", package = "migraph",
              old = "node_components")
  node_in_component(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_weak_components <- function(.data) {
  .Deprecated("node_in_weak", package = "migraph",
              old = "node_weak_components")
  node_in_weak(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_strong_components <- function(.data) {
  .Deprecated("node_in_strong", package = "migraph",
              old = "node_strong_components")
  node_in_strong(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_equivalence <- function(.data) {
  .Deprecated("node_in_equivalence", package = "migraph",
              old = "node_equivalence")
  node_in_equivalence(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_structural_equivalence <- function(.data) {
  .Deprecated("node_in_structural", package = "migraph",
              old = "node_structural_equivalence")
  node_in_structural(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_regular_equivalence <- function(.data) {
  .Deprecated("node_in_regular", package = "migraph",
              old = "node_regular_equivalence")
  node_in_regular(.data)
}

#' @describeIn defunct Deprecated on 2024-06-14.
#' @export
node_automorphic_equivalence <- function(.data) {
  .Deprecated("node_in_automorphic", package = "migraph",
              old = "node_automorphic_equivalence")
  node_in_automorphic(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_transmissibility <- function(diff_model) {
  .Deprecated("net_transmissibility", package = "migraph",
              old = "network_transmissibility")
  net_transmissibility(diff_model)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_infection_length <- function(diff_model) {
  .Deprecated("net_recovery", package = "migraph",
              old = "network_infection_length")
  net_recovery(diff_model)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_reproduction <- function(diff_model) {
  .Deprecated("net_reproduction", package = "migraph",
              old = "network_reproduction")
  net_reproduction(diff_model)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_immunity <- function(diff_model, normalized = TRUE) {
  .Deprecated("net_immunity", package = "migraph",
              old = "network_immunity")
  net_immunity(diff_model, normalized)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_hazard <- function(diff_model) {
  .Deprecated("net_hazard", package = "migraph",
              old = "network_hazard")
  net_hazard(diff_model)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_density <- function(.data) {
  .Deprecated("net_density", package = "migraph",
              old = "network_density")
  net_density(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_components <- function(.data) {
  .Deprecated("net_components", package = "migraph",
              old = "network_components")
  net_components(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_cohesion <- function(.data) {
  .Deprecated("net_cohesion", package = "migraph",
              old = "network_cohesion")
  net_cohesion(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_adhesion <- function(.data) {
  .Deprecated("net_adhesion", package = "migraph",
              old = "network_adhesion")
  net_adhesion(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_diameter <- function(.data) {
  .Deprecated("net_diameter", package = "migraph",
              old = "network_diameter")
  net_diameter(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_length <- function(.data) {
  .Deprecated("net_length", package = "migraph",
              old = "network_length")
  net_length(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_independence <- function(.data) {
  .Deprecated("net_independence", package = "migraph",
              old = "network_independence")
  net_independence(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_change <- function(.data, object2) {
  .Deprecated("net_change", package = "migraph",
              old = "network_change")
  net_change(.data, object2)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_stability <- function(.data, object2) {
  .Deprecated("net_stability", package = "migraph",
              old = "network_stability")
  net_stability(.data, object2)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_reciprocity <- function(.data, method = "default") {
  .Deprecated("net_reciprocity", package = "migraph",
              old = "network_reciprocity")
  net_reciprocity(.data, method)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_transitivity <- function(.data) {
  .Deprecated("net_transitivity", package = "migraph",
              old = "network_transitivity")
  net_transitivity(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_equivalency <- function(.data) {
  .Deprecated("net_equivalency", package = "migraph",
              old = "network_equivalency")
  net_equivalency(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_transitivity <- function(.data) {
  .Deprecated("net_transitivity", package = "migraph",
              old = "network_transitivity")
  net_transitivity(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_congruency <- function(.data) {
  .Deprecated("net_congruency", package = "migraph",
              old = "network_congruency")
  net_congruency(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_richness <- function(.data, attribute) {
  .Deprecated("net_richness", package = "migraph",
              old = "network_richness")
  net_richness(.data, attribute)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_diversity <- function(.data, attribute, clusters = NULL) {
  .Deprecated("net_diversity", package = "migraph",
              old = "network_diversity")
  net_diversity(.data, attribute, clusters)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_heterophily <- function(.data, attribute) {
  .Deprecated("net_heterophily", package = "migraph",
              old = "network_heterophily")
  net_heterophily(.data, attribute)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_assortativity <- function(.data) {
  .Deprecated("net_assortativity", package = "migraph",
              old = "network_assortativity")
  net_assortativity(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_spatial <- function(.data, attribute) {
  .Deprecated("net_spatial", package = "migraph",
              old = "network_spatial")
  net_spatial(.data, attribute)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_connectedness <- function(.data) {
  .Deprecated("net_connectedness", package = "migraph",
              old = "network_connectedness")
  net_connectedness(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_efficiency <- function(.data) {
  .Deprecated("net_efficiency", package = "migraph",
              old = "network_efficiency")
  net_efficiency(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_upperbound <- function(.data) {
  .Deprecated("net_upperbound", package = "migraph",
              old = "network_upperbound")
  net_upperbound(.data)
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

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_degree <- function(.data, normalized = TRUE,
                           direction = c("all", "out", "in")) {
  .Deprecated("net_degree", package = "migraph",
              old = "network_degree")
  net_degree(.data, normalized, direction)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_outdegree <- function(.data, normalized = TRUE) {
  .Deprecated("net_outdegree", package = "migraph",
              old = "network_outdegree")
  net_outdegree(.data, normalized)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_indegree <- function(.data, normalized = TRUE) {
  .Deprecated("net_indegree", package = "migraph",
              old = "network_indegree")
  net_indegree(.data, normalized)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_betweenness <- function(.data, normalized = TRUE,
                                direction = c("all", "out", "in")) {
  .Deprecated("net_betweenness", package = "migraph",
              old = "network_betweenness")
  net_betweenness(.data, normalized, direction)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_closeness <- function(.data, normalized = TRUE,
                              direction = c("all", "out", "in")) {
  .Deprecated("net_closeness", package = "migraph",
              old = "network_closeness")
  net_closeness(.data, normalized, direction)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_reach <- function(.data, normalized = TRUE, k = 2) {
  .Deprecated("net_reach", package = "migraph",
              old = "network_reach")
  net_reach(.data, normalized, k)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_harmonic <- function(.data, normalized = TRUE, k = 2) {
  .Deprecated("net_harmonic", package = "migraph",
              old = "network_harmonic")
  net_harmonic(.data, normalized, k)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_eigenvector <- function(.data, normalized = TRUE) {
  .Deprecated("net_eigenvector", package = "migraph",
              old = "network_eigenvector")
  net_eigenvector(.data, normalized)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_core <- function(.data, mark = NULL) {
  .Deprecated("net_core", package = "migraph",
              old = "network_core")
  net_core(.data, mark)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_richclub <- function(.data) {
  .Deprecated("net_richclub", package = "migraph",
              old = "network_richclub")
  net_richclub(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_factions <- function(.data, membership = NULL) {
  .Deprecated("net_factions", package = "migraph",
              old = "network_factions")
  net_factions(.data, membership)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_modularity <- function(.data, membership = NULL, resolution = 1) {
  .Deprecated("net_modularity", package = "migraph",
              old = "network_modularity")
  net_modularity(.data, membership, resolution)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_smallworld <- function(.data, method = c("omega", "sigma", "SWI"),
                               times = 100) {
  .Deprecated("net_smallworld", package = "migraph",
              old = "network_smallworld")
  net_smallworld(.data, method, times)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_scalefree <- function(.data) {
  .Deprecated("net_scalefree", package = "migraph",
              old = "network_scalefree")
  net_scalefree(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_balance <- function(.data) {
  .Deprecated("net_balance", package = "migraph",
              old = "network_balance")
  net_balance(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
node_tie_census <- function(.data) {
  .Deprecated("node_by_tie", package = "migraph",
              old = "node_tie_census")
  node_by_tie(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
node_triad_census <- function(.data) {
  .Deprecated("node_by_triad", package = "migraph",
              old = "node_triad_census")
  node_by_triad(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
node_quad_census <- function(.data) {
  .Deprecated("node_by_quad", package = "migraph",
              old = "node_quad_census")
  node_by_quad(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
node_path_census <- function(.data) {
  .Deprecated("node_by_path", package = "migraph",
              old = "node_path_census")
  node_by_path(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_dyad_census <- function(.data) {
  .Deprecated("net_by_dyad", package = "migraph",
              old = "network_dyad_census")
  net_by_dyad(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_triad_census <- function(.data) {
  .Deprecated("net_by_triad", package = "migraph",
              old = "network_triad_census")
  net_by_triad(.data)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_mixed_census <- function(.data, object2) {
  .Deprecated("net_by_mixed", package = "migraph",
              old = "network_mixed_census")
  net_by_mixed(.data, object2)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
node_brokerage_census <- function(.data, membership, standardized = FALSE) {
  .Deprecated("node_by_brokerage", package = "migraph",
              old = "node_brokerage_census")
  node_by_brokerage(.data, membership, standardized)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
network_brokerage_census <- function(.data, membership, standardized = FALSE) {
  .Deprecated("net_by_brokerage", package = "migraph",
              old = "network_brokerage_census")
  net_by_brokerage(.data, membership, standardized = FALSE)
}

#' @describeIn defunct Deprecated on 2024-06-20.
#' @export
node_brokering <- function(.data, membership) {
  .Deprecated("node_in_brokering", package = "migraph",
              old = "node_brokering")
  node_in_brokering(.data, membership)
}

#' @describeIn defunct Deprecated on 2024-06-21.
#' @export
node_core <- function(.data) {
  .Deprecated("node_is_core", package = "migraph",
              old = "node_core")
  node_is_core(.data)
}

#' @describeIn defunct Deprecated on 2024-10-10.
#' @export
node_by_quad <- function(.data) {
  .Deprecated("node_by_quad", package = "manynet",
              old = "node_by_tetrad")
  node_by_tetrad(.data)
}

