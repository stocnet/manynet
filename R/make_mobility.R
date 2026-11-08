# Mobility ####

#' Making mobility models on networks
#' 
#' @description
#' These functions allow mobility games to be played upon networks.
#' 
#' - `play_segregation()` plays a Schelling segregation model upon a network.
#' 
#' @name make_mobility
#' @family makes
#' @family models
#' @inheritParams mark_is
#' @param steps The number of steps forward in learning.
#'   By default the number of nodes in the network.
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
#' @export
play_segregation <- function(.data, 
                             attribute,
                             heterophily = 0,
                             who_moves = c("ordered","random","most_dissatisfied"),
                             choice_function = c("satisficing","optimising", "minimising"),
                             steps) {
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

.node_heterophily <- function(.data, attribute){
  .data <- manynet::expect_nodes(.data)
  m <- manynet::as_matrix(.data)
  if (length(attribute) == 1 && is.character(attribute)) {
    attribute <- manynet::node_attribute(.data, attribute)
  }
  if (is.character(attribute) | is.numeric(attribute)) {
    attribute <- as.factor(attribute)
  }
  if(anyNA(attribute)){
    m[is.na(attribute),] <- NA
    m[,is.na(attribute)] <- NA
  }
  same <- outer(attribute, attribute, "==")
  nInternal <- rowSums(m * same, na.rm = TRUE)
  nInternal[is.na(attribute)] <- NA
  nExternal <- rowSums(m, na.rm = TRUE) - nInternal
  ei <- (nExternal - nInternal) / rowSums(m, na.rm = TRUE)
  ei
}
