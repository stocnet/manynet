# Missing ####

#' Modifying missing tie data
#' 
#' @description
#'   These functions offer tools for imputing missing tie data.
#'   Currently two options are available:
#' 
#'   - `na_to_zero()` replaces any missing values with zeros,
#'   which are the modal value in sparse social networks.
#'   - `na_to_mean()` replaces missing values with the average non-missing value.
#' @name manip_miss
#' @family modifications
#' @inheritParams is
#' @return A data object of the same class as the function was given.
#' @references 
#'   Krause, Robert, Mark Huisman, Christian Steglich, and Tom A.B. Snijders. 2020. 
#'   "Missing data in cross-sectional networks–An extensive comparison of missing data treatment methods". 
#'   _Social Networks_, 62, 99-112.
#' @examples 
#' missTest <- ison_adolescents %>% 
#'    add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) %>% 
#'    as_matrix
#' missTest
#' na_to_zero(missTest)
#' na_to_mean(missTest)
NULL

#' @rdname manip_miss
#' @export
na_to_zero <- function(.data) UseMethod("na_to_zero")

#' @export
na_to_zero.tbl_graph <- function(.data){
  weight <- NULL
  .data %>% filter_ties(!is.na(weight))
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

#' @rdname manip_miss
#' @export
na_to_mean <- function(.data) UseMethod("na_to_mean")

#' @export
na_to_mean.tbl_graph <- function(.data){
  weight <- NULL
  if(is_weighted(.data) & any(tie_weights(.data)>1)){
    .data %>% mutate_ties(weight = ifelse(is.na(weight), 
                             mean(weight, na.rm = TRUE), 
                             weight))
  } else {
    prob <- sum(tie_attribute(.data, "weight"), na.rm = TRUE)/
      sum(!is.na(tie_attribute(.data, "weight")))
    .data %>% mutate_ties(weight = vapply(seq_len(weight),
                              function(x) ifelse(is.na(x),
                                                 stats::rbinom(1,1,prob),
                                             x),
                            numeric(1)))
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
  if(any(.data>1, na.rm = TRUE)){
    .data[is.na(.data)] <- mean(.data, na.rm = TRUE)
    .data
  } else {
    .data[is.na(.data)] <- stats::rbinom(sum(is.na(.data)), 
                                    1, mean(.data, na.rm = TRUE))
    .data
  }
}

#' @export
na_to_mean.data.frame <- function(.data){
  .data[is.na(.data[,3]),3] <- mean(.data[,3], na.rm = TRUE)
  .data
}

