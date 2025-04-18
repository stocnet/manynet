#' Methods for equivalence clustering
#' 
#' @description
#'   These functions are used to cluster some census object:
#'   
#'   - `cluster_hierarchical()` returns a hierarchical clustering object
#'   created by `stats::hclust()`.
#'   - `cluster_concor()` returns a hierarchical clustering object
#'   created from a convergence of correlations procedure (CONCOR).
#' 
#'   These functions are not intended to be called directly,
#'   but are called within `node_equivalence()` and related functions.
#'   They are exported and listed here to provide more detailed documentation.
#' @name model_cluster
#' @inheritParams member_equivalence
NULL

#' @rdname model_cluster 
#' @export
cluster_hierarchical <- function(census, distance){
  correlations <- manynet::to_correlation(t(census))
  dissimilarity <- 1 - correlations
  distances <- stats::dist(dissimilarity, method = distance)
  hc <- stats::hclust(distances)
  hc$distances <- distances
  hc
}

#' @rdname model_cluster 
#' @export
cluster_cosine <- function(census, distance){
  cosines <- manynet::to_cosine(census)
  dissimilarity <- 1 - cosines
  distances <- stats::dist(dissimilarity, method = distance)
  hc <- stats::hclust(distances)
  hc$distances <- distances
  hc
}

# cluster_concor(ison_adolescents)
# cluster_concor(ison_southern_women)
# https://github.com/bwlewis/hclust_in_R/blob/master/hc.R

#' @rdname model_cluster 
#' @section CONCOR:
#'   First a matrix of Pearson correlation coefficients between each pair of nodes
#'   profiles in the given census is created. 
#'   Then, again, we find the correlations of this square, symmetric matrix,
#'   and continue to do this iteratively until each entry is either `1` or `-1`.
#'   These values are used to split the data into two partitions,
#'   with members either holding the values `1` or `-1`.
#'   This procedure from census to convergence is then repeated within each block,
#'   allowing further partitions to be found.
#'   Unlike UCINET, partitions are continued until there are single members in
#'   each partition.
#'   Then a distance matrix is constructed from records of in which partition phase
#'   nodes were separated, 
#'   and this is given to `stats::hclust()` so that dendrograms etc can be returned.
#' @importFrom stats complete.cases
#' @references 
#' ## On CONCOR clustering
#' Breiger, Ronald L., Scott A. Boorman, and Phipps Arabie. 1975.  
#'   "An Algorithm for Clustering Relational Data with Applications to 
#'   Social Network Analysis and Comparison with Multidimensional Scaling". 
#'   _Journal of Mathematical Psychology_, 12: 328-83.
#'   \doi{10.1016/0022-2496(75)90028-0}.
#' @export
cluster_concor <- function(.data, census){
  if(missing(.data)) {expect_nodes(); .data <- .G()}
  split_cor <- function(m0, cutoff = 1) {
    if (ncol(m0) < 2 | all(manynet::to_correlation(m0)==1)) list(m0)
    else {
      mi <- manynet::to_correlation(m0)
      while (any(abs(mi) <= cutoff)) {
        mi <- stats::cor(mi)
        cutoff <- cutoff - 0.0001
      }
      group <- mi[, 1] > 0
      if(all(group)){
       list(m0) 
      } else {
        list(m0[, group, drop = FALSE], 
           m0[, !group, drop = FALSE])
      }
    }
  }
  p_list <- list(t(census))
  if(is.null(colnames(p_list[[1]]))) 
    colnames(p_list[[1]]) <- paste0("V",1:ncol(p_list[[1]]))
  p_group <- list()
  if(is_twomode(.data)){
    p_list <- list(p_list[[1]][, !node_is_mode(.data), drop = FALSE],
                   p_list[[1]][, node_is_mode(.data), drop = FALSE])
    p_group[[1]] <- lapply(p_list, function(z) colnames(z))
    i <- 2
  } else i <- 1
  while(!all(vapply(p_list, function(x) ncol(x)==1, logical(1)))){
    p_list <- unlist(lapply(p_list,
                            function(y) split_cor(y)),
                     recursive = FALSE)
    p_group[[i]] <- lapply(p_list, function(z) colnames(z))
    if(i > 2 && length(p_group[[i]]) == length(p_group[[i-1]])) break
    i <- i+1
  }
  
  if(is_labelled(.data)){
    merges <- sapply(rev(1:(i-1)), 
                     function(p) lapply(p_group[[p]], 
                                        function(s){
                                          g <- match(s, node_names(.data))
                                          if(length(g)==1) c(g, 0, p) else 
                                            if(length(g)==2) c(g, p) else
                                              c(t(cbind(t(utils::combn(g, 2)), p)))
                                        } ))
  } else {
    merges <- sapply(rev(1:(i-1)), 
                     function(p) lapply(p_group[[p]], 
                                        function(s){
                                          g <- as.numeric(gsub("^V","",s))
                                          if(length(g)==1) c(g, 0, p) else 
                                            if(length(g)==2) c(g, p) else
                                              c(t(cbind(t(utils::combn(g, 2)), p)))
                                        } ))
  }
  merges <- c(merges, 
              list(c(t(cbind(t(utils::combn(seq_len(manynet::net_nodes(.data)), 2)), 0)))))
  merged <- matrix(unlist(merges), ncol = 3, byrow = TRUE)
  merged <- merged[!duplicated(merged[,1:2]),]
  merged[,3] <- abs(merged[,3] - max(merged[,3]))
  merged[merged == 0] <- NA
  merged <- merged[stats::complete.cases(merged),]
  merged <- as.data.frame(merged)
  names(merged) <- c("from","to","weight")
  
  distances <- manynet::as_matrix(manynet::as_igraph(merged))
  distances <- distances + t(distances)
  # distances <- distances[-which(rownames(distances)==0),-which(colnames(distances)==0)]
  if(manynet::is_labelled(.data))
    rownames(distances) <- colnames(distances) <- manynet::node_names(.data)
  hc <- hclust(d = as.dist(distances))
  hc$method <- "concor"
  hc$distances <- distances
  hc  
}
