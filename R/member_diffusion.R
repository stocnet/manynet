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
#' @inheritParams is
#' @inheritParams measure_net_diffusion
#' @family measures
#' @family diffusion
#' @name member_diffusion
#' @references
#'   Valente, Tom W. 1995. _Network models of the diffusion of innovations_
#'   (2nd ed.). Cresskill N.J.: Hampton Press.
NULL

#' @rdname member_diffusion
#' @examples
#'   smeg <- manynet::generate_smallworld(15, 0.025)
#'   smeg_diff <- manynet::play_diffusion(smeg, recovery = 0.2)
#'   # To classify nodes by their position in the adoption curve
#'   (adopts <- node_in_adopter(smeg_diff))
#'   summary(adopts)
#' @export
node_in_adopter <- function(diff_model){
  toa <- node_adoption_time(diff_model)
  toa[is.infinite(toa)] <- NA
  avg <- mean(toa, na.rm = TRUE)
  sdv <- stats::sd(toa, na.rm = TRUE)
  out <- ifelse(toa < (avg - sdv) | toa == 0, "Early Adopter", 
                ifelse(toa > (avg + sdv), "Laggard",
                       ifelse((avg - sdv) < toa & toa <= avg, "Early Majority", 
                              ifelse(avg < toa & toa <= avg + sdv, "Late Majority", 
                                     "Non-Adopter"))))
  out[is.na(out)] <- "Non-Adopter"
  make_node_member(out, attr(diff_model, "network"))
}

