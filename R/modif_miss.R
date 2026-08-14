# Missing ####

#' Modifying missing tie data
#' @name modif_miss
#' @description
#'   These functions offer tools for imputing missing tie data.
#'   Currently two options are available:
#'
#'   - `na_to_zero()` replaces any missing values with zeros,
#'   which are the modal value in sparse social networks.
#'   - `na_to_mean()` replaces missing values with the average non-missing value.
#'   Where the network is binary, so that an average would not be a value the
#'   network could hold, each missing tie is instead drawn from a Bernoulli
#'   distribution at the observed density.
#'   This makes `na_to_mean()` stochastic for binary networks,
#'   so use `set.seed()` where the result needs to be reproducible.
#'
#'   Where the network is one-mode and simplex, the diagonal is excluded when
#'   establishing the average or density, since a node's tie to itself is not
#'   usually a tie that could have been observed.
#'
#'   If there are no missing values,
#'   the network data is returned unaltered and no warning is given,
#'   so that these functions can be used to ensure conformance.
#' @template param_data
#' @template fam_modif
#' @references 
#' ## On missing data
#'   Krause, Robert, Mark Huisman, Christian Steglich, and Tom A.B. Snijders. 2020. 
#'   "Missing data in cross-sectional networks: An extensive comparison of missing data treatment methods". 
#'   _Social Networks_, 62: 99-112.
#'   \doi{10.1016/j.socnet.2020.02.004}
#' @examples 
#' missTest <- ison_adolescents |> 
#'    add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) |> 
#'    as_matrix()
#' missTest
#' na_to_zero(missTest)
#' na_to_mean(missTest)
NULL

#' @rdname modif_miss
#' @export
na_to_zero <- function(.data) UseMethod("na_to_zero")

#' @export
na_to_zero.default <- function(.data){
  as_input(.data, na_to_zero)
}

#' @export
na_to_zero.tbl_graph <- function(.data){
  weight <- NULL
  # an unweighted network has no tie values, and so none that can be missing
  if(!"weight" %in% igraph::edge_attr_names(.data)) return(.data)
  .data |> mutate_ties(weight = ifelse(is.na(weight), 0, weight))
}

#' @export
na_to_zero.igraph <- function(.data){
  as_igraph(na_to_zero(as_tidygraph(.data)))
}

#' @export
na_to_zero.network <- function(.data){
  as_network(na_to_zero(as_tidygraph(.data)))
}

#' @export
na_to_zero.matrix <- function(.data){
  .data[is.na(.data)] <- 0
  .data
}

#' @export
na_to_zero.data.frame <- function(.data){
  .data[is.na(.data[,3]),3] <- 0
  .data
}

#' @rdname modif_miss
#' @export
na_to_mean <- function(.data) UseMethod("na_to_mean")

#' @export
na_to_mean.default <- function(.data){
  as_input(.data, na_to_mean)
}

#' @export
na_to_mean.tbl_graph <- function(.data){
  weight <- NULL
  # an unweighted network has no tie values, and so none that can be missing
  if(!"weight" %in% igraph::edge_attr_names(.data)) return(.data)
  # `na.rm` is needed here because the missing values being imputed would
  # otherwise make the comparison itself missing, and the `if()` error
  avg <- .miss_average(.data)
  if(any(tie_weights(.data) > 1, na.rm = TRUE)){
    .data |> mutate_ties(weight = ifelse(is.na(weight), avg, weight))
  } else {
    # `rbinom()` is vectorised, and drawing the full length keeps each draw
    # aligned with the tie it might replace
    .data |> mutate_ties(weight = ifelse(is.na(weight),
                                         stats::rbinom(length(weight), 1, avg),
                                         weight))
  }
}

#' @export
na_to_mean.igraph <- function(.data){
  as_igraph(na_to_mean(as_tidygraph(.data)))
}

#' @export
na_to_mean.network <- function(.data){
  as_network(na_to_mean(as_tidygraph(.data)))
}

#' @export
na_to_mean.matrix <- function(.data){
  if(!anyNA(.data)) return(.data)
  avg <- .miss_average(.data)
  # note that this indexes `.data` and not the object the average was taken
  # over, since the diagonal blanked there is structural and not missing
  miss <- is.na(.data)
  if(any(.data > 1, na.rm = TRUE)){
    .data[miss] <- avg
  } else .data[miss] <- stats::rbinom(sum(miss), 1, avg)
  .data
}

#' @export
na_to_mean.data.frame <- function(.data){
  .data[is.na(.data[,3]),3] <- mean(.data[,3], na.rm = TRUE)
  .data
}

# Helper functions ------------------

# The average value a missing tie is imputed with, which for a binary network
# is the density. Taken over the matrix rather than over the tie list, so that
# cells the network records as absent count towards the average, and so that
# every class arrives at the same figure.
.miss_average <- function(.data){
  x <- as_matrix(.data)
  # a node's tie to itself is not usually a tie that could have been observed,
  # so counting the diagonal would bias the average down by (n-1)/n
  if(!is_twomode(.data) && !is_complex(.data)) diag(x) <- NA
  mean(x, na.rm = TRUE)
}

