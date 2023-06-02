# Missing ####

#' Tools for imputing missing tie data
#' 
#' These functions offer tools for imputing missing tie data.
#' Currently two options are available:
#' replacing the missing values with zeros, 
#' which are the modal value in sparse social networks,
#' and replacing the missing values with the average non-missing value for that vector.
#' @name miss
#' @family manipulations
#' @references 
#'   Krause, Robert, Mark Huisman, Christian Steglich, and Tom A.B. Snijders. 2020. 
#'   "Missing data in cross-sectional networks–An extensive comparison of missing data treatment methods". 
#'   _Social Networks_, 62, 99-112.
NULL

#' @describeIn miss Impute missing tie data as zero,
#'   the modal value in sparse social networks.
#' @examples 
#' missTest <- ison_adolescents %>% 
#'    add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) %>% 
#'    as_matrix
#' missTest
#' na_to_zero(missTest)
#' na_to_mean(missTest)
#' @export
na_to_zero <- function(object) UseMethod("na_to_zero")

#' @export
na_to_zero.tbl_graph <- function(object){
  object %>% activate(edges) %>% 
    dplyr::filter(!is.na(weight)) %>% 
    activate(nodes)
}

#' @export
na_to_zero.igraph <- function(object){
  as_igraph(na_to_zero(as_tidygraph(object)))
}

#' @export
na_to_zero.network <- function(object){
  as_network(na_to_zero(as_tidygraph(object)))
}

#' @export
na_to_zero.matrix <- function(object){
  object[is.na(object)] <- 0
  object
}

#' @export
na_to_zero.data.frame <- function(object){
  object[is.na(object[,3]),3] <- 0
  object
}

#' @describeIn miss Impute missing tie data as
#'   the mean value in the network.
#' @export
na_to_mean <- function(object) UseMethod("na_to_mean")

#' @export
na_to_mean.tbl_graph <- function(object){
  weight <- NULL
  if(is_weighted(object) & any(tie_weights(object)>1)){
    object %>% mutate_ties(weight = ifelse(is.na(weight), 
                             mean(weight, na.rm = TRUE), 
                             weight))
  } else {
    prob <- sum(tie_attribute(object, "weight"), na.rm = TRUE)/
      sum(!is.na(tie_attribute(object, "weight")))
    object %>% mutate_ties(weight = vapply(seq_len(weight),
                              function(x) ifelse(is.na(x),
                                                 stats::rbinom(1,1,prob),
                                             x),
                            numeric(1)))
  }
}

#' @export
na_to_mean.igraph <- function(object){
  as_igraph(na_to_mean(as_tidygraph(object)))
}

#' @export
na_to_mean.network <- function(object){
  as_network(na_to_mean(as_tidygraph(object)))
}

#' @export
na_to_mean.matrix <- function(object){
  if(any(object>1, na.rm = TRUE)){
    object[is.na(object)] <- mean(object, na.rm = TRUE)
    object
  } else {
    object[is.na(object)] <- stats::rbinom(sum(is.na(object)), 
                                    1, mean(object, na.rm = TRUE))
    object
  }
}

