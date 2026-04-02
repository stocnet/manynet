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

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_mode <- function(.data) {
  .Deprecated("node_is_mode", package = "manynet",
              old = "node_mode")
  node_is_mode(.data)
}

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
mutate_net <- function(.data, ...) {
  .Deprecated("mutate_info", package = "manynet",
              old = "mutate_net")
  mutate_info(.data, ...)
}

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_info <- function(.data) {
  .Deprecated("as_infolist", package = "manynet",
              old = "net_info")
  as_infolist(.data)
}

#' @describeIn defunct Deprecated on 2026-04-02.
#' @export
read_cran <- function(pkg = "all") {
  .Deprecated("collect_cran", package = "manynet",
              old = "read_cran")
  collect_cran(pkg = pkg)
}

#' @describeIn defunct Deprecated on 2026-04-02.
#' @export
read_pkg <- function(dir = getwd()) {
  .Deprecated("collect_pkg", package = "manynet",
              old = "read_pkg")
  collect_pkg(dir = dir)
}

# Moved to netrics from 2.0.0 ####

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_degree <- function(...) fn_moved("node_degree", "node_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_deg <- function(...) fn_moved("node_deg", "node_by_deg", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_quad <- function(...) fn_moved("net_by_quad", "net_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_hazard <- function(...) fn_moved("net_hazard", "net_x_hazard", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_automorphic_equivalence <- function(...) fn_moved("node_automorphic_equivalence", "node_in_automorphic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokerage_census <- function(...) fn_moved("node_brokerage_census", "node_x_brokerage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokering <- function(...) fn_moved("node_brokering", "node_in_brokering", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_quad <- function(...) fn_moved("node_by_quad", "node_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_components <- function(...) fn_moved("node_components", "node_in_component", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_core <- function(...) fn_moved("node_core", "node_in_core", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_edge_betweenness <- function(...) fn_moved("node_edge_betweenness", "node_in_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_equivalence <- function(...) fn_moved("node_equivalence", "node_in_equivalence", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_fast_greedy <- function(...) fn_moved("node_fast_greedy", "node_in_greedy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_fluid <- function(...) fn_moved("node_fluid", "node_in_fluid", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_infomap <- function(...) fn_moved("node_infomap", "node_in_infomap", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_kernighanlin <- function(...) fn_moved("node_kernighanlin", "node_in_partition", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_leading_eigen <- function(...) fn_moved("node_leading_eigen", "node_in_eigen", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_leiden <- function(...) fn_moved("node_leiden", "node_in_leiden", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_louvain <- function(...) fn_moved("node_louvain", "node_in_louvain", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_optimal <- function(...) fn_moved("node_optimal", "node_in_optimal", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_path_census <- function(...) fn_moved("node_path_census", "node_x_path", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_quad_census <- function(...) fn_moved("node_quad_census", "node_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_regular_equivalence <- function(...) fn_moved("node_regular_equivalence", "node_in_regular", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_roulette <- function(...) fn_moved("node_roulette", "node_in_roulette", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_spinglass <- function(...) fn_moved("node_spinglass", "node_in_spinglass", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_strong_components <- function(...) fn_moved("node_strong_components", "node_in_strong", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_structural_equivalence <- function(...) fn_moved("node_structural_equivalence", "node_in_structural", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_tie_census <- function(...) fn_moved("node_tie_census", "node_x_tie", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_triad_census <- function(...) fn_moved("node_triad_census", "node_x_triad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_walktrap <- function(...) fn_moved("node_walktrap", "node_in_walktrap", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_weak_components <- function(...) fn_moved("node_weak_components", "node_in_weak", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_core <- function(...) fn_moved("node_is_core", "node_is_core", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_cutpoint <- function(...) fn_moved("node_is_cutpoint", "node_is_cutpoint", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_exposed <- function(...) fn_moved("node_is_exposed", "node_is_exposed", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_fold <- function(...) fn_moved("node_is_fold", "node_is_fold", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_independent <- function(...) fn_moved("node_is_independent", "node_is_independent", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_infected <- function(...) fn_moved("node_is_infected", "node_is_infected", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_isolate <- function(...) fn_moved("node_is_isolate", "node_is_isolate", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_latent <- function(...) fn_moved("node_is_latent", "node_is_latent", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_max <- function(...) fn_moved("node_is_max", "node_is_max", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_mean <- function(...) fn_moved("node_is_mean", "node_is_mean", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_mentor <- function(...) fn_moved("node_is_mentor", "node_is_mentor", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_min <- function(...) fn_moved("node_is_min", "node_is_min", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_neighbor <- function(...) fn_moved("node_is_neighbor", "node_is_neighbor", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_pendant <- function(...) fn_moved("node_is_pendant", "node_is_pendant", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_random <- function(...) fn_moved("node_is_random", "node_is_random", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_recovered <- function(...) fn_moved("node_is_recovered", "node_is_recovered", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_is_universal <- function(...) fn_moved("node_is_universal", "node_is_universal", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_bridge <- function(...) fn_moved("tie_is_bridge", "tie_is_bridge", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_cyclical <- function(...) fn_moved("tie_is_cyclical", "tie_is_cyclical", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_feedback <- function(...) fn_moved("tie_is_feedback", "tie_is_feedback", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_imbalanced <- function(...) fn_moved("tie_is_imbalanced", "tie_is_imbalanced", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_loop <- function(...) fn_moved("tie_is_loop", "tie_is_loop", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_max <- function(...) fn_moved("tie_is_max", "tie_is_max", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_min <- function(...) fn_moved("tie_is_min", "tie_is_min", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_multiple <- function(...) fn_moved("tie_is_multiple", "tie_is_multiple", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_path <- function(...) fn_moved("tie_is_path", "tie_is_path", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_random <- function(...) fn_moved("tie_is_random", "tie_is_random", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_reciprocated <- function(...) fn_moved("tie_is_reciprocated", "tie_is_reciprocated", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_simmelian <- function(...) fn_moved("tie_is_simmelian", "tie_is_simmelian", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_transitive <- function(...) fn_moved("tie_is_transitive", "tie_is_transitive", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_triangular <- function(...) fn_moved("tie_is_triangular", "tie_is_triangular", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_is_triplet <- function(...) fn_moved("tie_is_triplet", "tie_is_triplet", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_adhesion <- function(...) fn_moved("net_adhesion", "net_by_adhesion", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_assortativity <- function(...) fn_moved("net_assortativity", "net_by_assortativity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_balance <- function(...) fn_moved("net_balance", "net_by_balance", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_betweenness <- function(...) fn_moved("net_betweenness", "net_by_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_change <- function(...) fn_moved("net_change", "net_x_change", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_closeness <- function(...) fn_moved("net_closeness", "net_by_closeness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_cohesion <- function(...) fn_moved("net_cohesion", "net_by_cohesion", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_components <- function(...) fn_moved("net_components", "net_by_components", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_congruency <- function(...) fn_moved("net_congruency", "net_by_congruency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_connectedness <- function(...) fn_moved("net_connectedness", "net_by_connectedness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_core <- function(...) fn_moved("net_core", "net_by_core", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_correlation <- function(...) fn_moved("net_correlation", "net_x_correlation", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_degree <- function(...) fn_moved("net_degree", "net_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_density <- function(...) fn_moved("net_density", "net_by_density", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_diameter <- function(...) fn_moved("net_diameter", "net_by_diameter", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_diversity <- function(...) fn_moved("net_diversity", "net_by_diversity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_efficiency <- function(...) fn_moved("net_efficiency", "net_by_efficiency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_eigenvector <- function(...) fn_moved("net_eigenvector", "net_by_eigenvector", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_equivalency <- function(...) fn_moved("net_equivalency", "net_by_equivalency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_factions <- function(...) fn_moved("net_factions", "net_by_factions", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_harmonic <- function(...) fn_moved("net_harmonic", "net_by_harmonic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_heterophily <- function(...) fn_moved("net_heterophily", "net_by_heterophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_homophily <- function(...) fn_moved("net_homophily", "net_by_homophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_immunity <- function(...) fn_moved("net_immunity", "net_by_immunity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_indegree <- function(...) fn_moved("net_indegree", "net_by_indegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_independence <- function(...) fn_moved("net_independence", "net_by_independence", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_infection_complete <- function(...) fn_moved("net_infection_complete", "net_by_infection_complete", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_infection_peak <- function(...) fn_moved("net_infection_peak", "net_by_infection_peak", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_infection_total <- function(...) fn_moved("net_infection_total", "net_by_infection_total", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_length <- function(...) fn_moved("net_length", "net_by_length", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_modularity <- function(...) fn_moved("net_modularity", "net_by_modularity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_outdegree <- function(...) fn_moved("net_outdegree", "net_by_outdegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_reach <- function(...) fn_moved("net_reach", "net_by_reach", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_reciprocity <- function(...) fn_moved("net_reciprocity", "net_by_reciprocity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_recovery <- function(...) fn_moved("net_recovery", "net_by_recovery", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_reproduction <- function(...) fn_moved("net_reproduction", "net_by_reproduction", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_richclub <- function(...) fn_moved("net_richclub", "net_by_richclub", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_richness <- function(...) fn_moved("net_richness", "net_by_richness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_scalefree <- function(...) fn_moved("net_scalefree", "net_by_scalefree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_smallworld <- function(...) fn_moved("net_smallworld", "net_by_smallworld", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_spatial <- function(...) fn_moved("net_spatial", "net_by_spatial", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_stability <- function(...) fn_moved("net_stability", "net_x_stability", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_strength <- function(...) fn_moved("net_strength", "net_by_strength", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_toughness <- function(...) fn_moved("net_toughness", "net_by_toughness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_transitivity <- function(...) fn_moved("net_transitivity", "net_by_transitivity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_transmissibility <- function(...) fn_moved("net_transmissibility", "net_by_transmissibility", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_upperbound <- function(...) fn_moved("net_upperbound", "net_by_upperbound", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_waves <- function(...) fn_moved("net_waves", "net_by_waves", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_adoption_time <- function(...) fn_moved("node_adoption_time", "node_by_adopt_time", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_alpha <- function(...) fn_moved("node_alpha", "node_by_alpha", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_authority <- function(...) fn_moved("node_authority", "node_by_authority", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_betweenness <- function(...) fn_moved("node_betweenness", "node_by_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_bridges <- function(...) fn_moved("node_bridges", "node_by_bridges", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_closeness <- function(...) fn_moved("node_closeness", "node_by_closeness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_constraint <- function(...) fn_moved("node_constraint", "node_by_constraint", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_coreness <- function(...) fn_moved("node_coreness", "node_by_coreness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_distance <- function(...) fn_moved("node_distance", "node_by_distance", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_diversity <- function(...) fn_moved("node_diversity", "node_by_diversity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_eccentricity <- function(...) fn_moved("node_eccentricity", "node_by_eccentricity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_efficiency <- function(...) fn_moved("node_efficiency", "node_by_efficiency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_effsize <- function(...) fn_moved("node_effsize", "node_by_effsize", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_eigenvector <- function(...) fn_moved("node_eigenvector", "node_by_eigenvector", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_equivalency <- function(...) fn_moved("node_equivalency", "node_by_equivalency", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_exposure <- function(...) fn_moved("node_exposure", "node_by_adopt_exposure", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_flow <- function(...) fn_moved("node_flow", "node_by_flow", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_harmonic <- function(...) fn_moved("node_harmonic", "node_by_harmonic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_heterophily <- function(...) fn_moved("node_heterophily", "node_by_heterophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_hierarchy <- function(...) fn_moved("node_hierarchy", "node_by_hierarchy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_homophily <- function(...) fn_moved("node_homophily", "node_by_homophily", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_hub <- function(...) fn_moved("node_hub", "node_by_hub", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_indegree <- function(...) fn_moved("node_indegree", "node_by_indegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_induced <- function(...) fn_moved("node_induced", "node_by_induced", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_information <- function(...) fn_moved("node_information", "node_by_information", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_kcoreness <- function(...) fn_moved("node_kcoreness", "node_by_kcoreness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_leverage <- function(...) fn_moved("node_leverage", "node_by_leverage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_multidegree <- function(...) fn_moved("node_multidegree", "node_by_multidegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_neighbours_degree <- function(...) fn_moved("node_neighbours_degree", "node_by_neighbours_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_outdegree <- function(...) fn_moved("node_outdegree", "node_by_outdegree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_pagerank <- function(...) fn_moved("node_pagerank", "node_by_pagerank", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_posneg <- function(...) fn_moved("node_posneg", "node_by_posneg", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_power <- function(...) fn_moved("node_power", "node_by_power", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_randomwalk <- function(...) fn_moved("node_randomwalk", "node_by_randomwalk", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_reach <- function(...) fn_moved("node_reach", "node_by_reach", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_reciprocity <- function(...) fn_moved("node_reciprocity", "node_by_reciprocity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_recovery <- function(...) fn_moved("node_recovery", "node_by_adopt_recovery", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_redundancy <- function(...) fn_moved("node_redundancy", "node_by_redundancy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_richness <- function(...) fn_moved("node_richness", "node_by_richness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_stress <- function(...) fn_moved("node_stress", "node_by_stress", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_subgraph <- function(...) fn_moved("node_subgraph", "node_by_subgraph", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_thresholds <- function(...) fn_moved("node_thresholds", "node_by_adopt_threshold", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_transitivity <- function(...) fn_moved("node_transitivity", "node_by_transitivity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_vitality <- function(...) fn_moved("node_vitality", "node_by_vitality", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_betweenness <- function(...) fn_moved("tie_betweenness", "tie_by_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_closeness <- function(...) fn_moved("tie_closeness", "tie_by_closeness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_cohesion <- function(...) fn_moved("tie_cohesion", "tie_by_cohesion", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_degree <- function(...) fn_moved("tie_degree", "tie_by_degree", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
tie_eigenvector <- function(...) fn_moved("tie_eigenvector", "tie_by_eigenvector", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_adopter <- function(...) fn_moved("node_in_adopter", "node_in_adopter", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_automorphic <- function(...) fn_moved("node_in_automorphic", "node_in_automorphic", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_betweenness <- function(...) fn_moved("node_in_betweenness", "node_in_betweenness", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_brokering <- function(...) fn_moved("node_in_brokering", "node_in_brokering", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_community <- function(...) fn_moved("node_in_community", "node_in_community", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_component <- function(...) fn_moved("node_in_component", "node_in_component", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_core <- function(...) fn_moved("node_in_core", "node_in_core", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_eigen <- function(...) fn_moved("node_in_eigen", "node_in_eigen", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_equivalence <- function(...) fn_moved("node_in_equivalence", "node_in_equivalence", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_fluid <- function(...) fn_moved("node_in_fluid", "node_in_fluid", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_greedy <- function(...) fn_moved("node_in_greedy", "node_in_greedy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_infomap <- function(...) fn_moved("node_in_infomap", "node_in_infomap", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_leiden <- function(...) fn_moved("node_in_leiden", "node_in_leiden", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_louvain <- function(...) fn_moved("node_in_louvain", "node_in_louvain", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_optimal <- function(...) fn_moved("node_in_optimal", "node_in_optimal", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_partition <- function(...) fn_moved("node_in_partition", "node_in_partition", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_regular <- function(...) fn_moved("node_in_regular", "node_in_regular", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_roulette <- function(...) fn_moved("node_in_roulette", "node_in_roulette", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_spinglass <- function(...) fn_moved("node_in_spinglass", "node_in_spinglass", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_strong <- function(...) fn_moved("node_in_strong", "node_in_strong", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_structural <- function(...) fn_moved("node_in_structural", "node_in_structural", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_walktrap <- function(...) fn_moved("node_in_walktrap", "node_in_walktrap", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_in_weak <- function(...) fn_moved("node_in_weak", "node_in_weak", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_brokerage <- function(...) fn_moved("net_by_brokerage", "net_x_brokerage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_dyad <- function(...) fn_moved("net_by_dyad", "net_x_dyad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_hazard <- function(...) fn_moved("net_by_hazard", "net_x_hazard", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_hierarchy <- function(...) fn_moved("net_by_hierarchy", "net_x_hierarchy", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_mixed <- function(...) fn_moved("net_by_mixed", "net_x_mixed", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_tetrad <- function(...) fn_moved("net_by_tetrad", "net_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
net_by_triad <- function(...) fn_moved("net_by_triad", "net_x_triad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokering_activity <- function(...) fn_moved("node_brokering_activity", "node_by_brokering_activity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_brokering_exclusivity <- function(...) fn_moved("node_brokering_exclusivity", "node_by_brokering_exclusivity", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_brokerage <- function(...) fn_moved("node_by_brokerage", "node_x_brokerage", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_dyad <- function(...) fn_moved("node_by_dyad", "node_x_dyad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_exposure <- function(...) fn_moved("node_by_exposure", "node_x_exposure", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_path <- function(...) fn_moved("node_by_path", "node_x_path", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_tetrad <- function(...) fn_moved("node_by_tetrad", "node_x_tetrad", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_tie <- function(...) fn_moved("node_by_tie", "node_x_tie", version = "2.0.0")

#' @describeIn defunct Deprecated on 2026-03-22.
#' @export
node_by_triad <- function(...) fn_moved("node_by_triad", "node_x_triad", version = "2.0.0")

# nocov end