# Opinion ####

#' Making opinion models on networks
#' 
#' @description
#' These functions allow learning games to be played upon networks.
#' 
#' - `play_learning()` plays a learning model upon a network.
#' 
#' @section Learning models: 
#'   The default is a Degroot learning model,
#'   but if `closeness` is defined as anything less than infinity, 
#'   this becomes a Deffuant model.
#'   A Deffuant model is similar to a Degroot model, however nodes only learn
#'   from other nodes whose beliefs are not too dissimilar from their own.
#' @name make_opinion
#' @family makes
#' @family models
#' @inheritParams mark_is
#' @param steps The number of steps forward in learning.
#'   By default the number of nodes in the network.
#' @param beliefs A vector indicating the probabilities nodes
#'   put on some outcome being 'true'.
#' @param closeness A threshold at which beliefs are too different to
#'   influence each other. By default `Inf`, i.e. there is no threshold.
#' @param epsilon The maximum difference in beliefs accepted
#'   for convergence to a consensus.
#' @references
#' DeGroot, Morris H. 1974. 
#' "Reaching a consensus",
#' _Journal of the American Statistical Association_, 69(345): 118–21.
#' \doi{10.1080/01621459.1974.10480137}
#' 
#' Deffuant, Guillaume, David Neau, Frederic Amblard, and Gérard Weisbuch. 2000.
#' "Mixing beliefs among interacting agents",
#' _Advances in Complex Systems_, 3(1): 87-98.
#' \doi{10.1142/S0219525900000078}
#' 
#' Golub, Benjamin, and Matthew O. Jackson 2010. 
#' "Naive learning in social networks and the wisdom of crowds", 
#' _American Economic Journal_, 2(1): 112-49.
#' \doi{10.1257/mic.2.1.112}
#' @examples 
#'   play_learning(ison_networkers, 
#'       rbinom(net_nodes(ison_networkers),1,prob = 0.25))
#' @export
play_learning <- function(.data, 
                          beliefs,
                          closeness = Inf,
                          steps,
                          epsilon = 0.0005){
  n <- net_nodes(.data)
  if(length(beliefs)!=n) 
    snet_abort("'beliefs' must be a vector the same length as the number of nodes in the network.")
  if(is.logical(beliefs)) beliefs <- beliefs*1
  if(missing(steps)) steps <- n
  
  t <- 0
  out <- matrix(NA,steps+1,length(beliefs))
  out[1,] <- beliefs
  trust_mat <- as_matrix(.data)/rowSums(as_matrix(.data))

  repeat{
    old_beliefs <- beliefs
    listen_mat <- trust_mat * (as.matrix(stats::dist(beliefs)) <= closeness)
    listen_mat <- listen_mat/rowSums(listen_mat)
    listen_mat[is.nan(listen_mat)] <- 0
    beliefs <- listen_mat %*% beliefs
    if(all(abs(old_beliefs - beliefs) < epsilon)) break
    t <- t+1
    out[t+1,] <- beliefs
    if(t==steps) break
  }
  out <- stats::na.omit(out)
  
  make_learn_model(out, .data)
}
