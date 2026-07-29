#' @param connectivity Character string, "weak" treats a directed network's
#'   components as if the network were undirected, and "strong" requires ties
#'   in both directions between members.
#'   This is ignored for undirected networks, where the two notions coincide.
#'   Note that the default differs by function: marks that assert
#'   connectedness default to "strong", while functions that scope or split a
#'   network into components default to "weak".
