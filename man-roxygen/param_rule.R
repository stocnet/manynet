#' @param rule How the networks' tie values are reconciled into a single value
#'   per dyad. A dyad tied in only one of the networks counts as untied in the
#'   others, which is what makes rules such as "min" and "product" meaningful.
#'   - "max" (the default) takes the largest of the values,
#'   so that a tie in any of the networks is a tie in the result.
#'   This is the union of the networks.
#'   - "min" takes the smallest, so that only ties present in all of them
#'   survive. This is their intersection.
#'   - "mean" averages the values, treating the networks as several readings
#'   of one underlying relationship.
#'   - "sum" adds them, so that ties reinforce one another.
#'   - "product" multiplies them, so that a tie survives only where every
#'   network records one, and strong ties are rewarded disproportionately.
#'
#'   Missing values propagate rather than being ignored, so that a dyad
#'   unobserved in any of the networks is unobserved in the result.
#'   Use the `na_to_*()` functions first to state a different assumption.
