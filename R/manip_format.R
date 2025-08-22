# Deformatting ####

#' Modifying network formats by removing information
#' 
#' @description
#'   These functions reformat manynet-consistent data.
#' 
#'   - `to_unnamed()` reformats labelled network data to unlabelled network data.
#'   - `to_undirected()` reformats directed network data to an undirected network,
#'   so that any pair of nodes with at least one directed edge will be
#'   connected by an undirected edge in the new network.
#'   This is equivalent to the "collapse" mode in `{igraph}`..
#'   - `to_unweighted()` reformats weighted network data to unweighted network 
#'   data, with all tie weights removed.
#'   - `to_unsigned()` reformats signed network data to unsigned network data
#'   keeping just the "positive" or "negative" ties.
#'   - `to_simplex()` reformats complex network data, containing loops, to simplex network data, without any loops.
#'   - `to_uniplex()` reformats multiplex network data to a single type of tie.
#' 
#'   If the format condition is not met,
#'   for example `to_undirected()` is used on a network that is already undirected,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   |              | data.frame| igraph| matrix| network| tbl_graph|
#'   |:-------------|----------:|------:|------:|-------:|---------:|
#'   |to_simplex    |          1|      1|      1|       1|         1|
#'   |to_undirected |          1|      1|      1|       1|         1|
#'   |to_uniplex    |          1|      1|      1|       1|         1|
#'   |to_unnamed    |          1|      1|      1|       1|         1|
#'   |to_unsigned   |          1|      1|      1|       1|         1|
#'   |to_unweighted |          1|      1|      1|       1|         1|
#' @inheritParams mark_is
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
#' @name manip_deformat
#' @family modifications
NULL

#' @rdname manip_deformat
#' @importFrom igraph delete_vertex_attr
#' @importFrom tidygraph as_tbl_graph
#' @importFrom network delete.vertex.attribute
#' @importFrom dplyr as_tibble
#' @export
to_unnamed <- function(.data) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(.data) {
  if ("name" %in% igraph::vertex_attr_names(.data)) {
    igraph::delete_vertex_attr(.data, "name")
  } else .data
}

#' @export
to_unnamed.tbl_graph <- function(.data) {
  if ("name" %in% igraph::vertex_attr_names(.data)) {
    as_tidygraph(igraph::delete_vertex_attr(.data, "name"))
  } else .data
}

#' @export
to_unnamed.network <- function(.data) {
  out <- network::delete.vertex.attribute(.data, "vertex.names")
  out
}

#' @export
to_unnamed.matrix <- function(.data) {
  out <- .data
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @export
to_unnamed.data.frame <- function(.data) {
  out <- .data
  names <- unique(unlist(c(out[,1],out[,2])))
  out[,1] <- match(unlist(.data[,1]), names)
  out[,2] <- match(unlist(.data[,2]), names)
  dplyr::as_tibble(out)
}

#' @rdname manip_deformat 
#' @export
to_undirected <- function(.data) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(.data) {
  igraph::as_undirected(.data)
}

#' @export
to_undirected.tbl_graph <- function(.data) {
  as_tidygraph(igraph::as_undirected(.data))
}

#' @export
to_undirected.network <- function(.data) {
  .data$gal$directed <- FALSE
  .data
}

#' @export
to_undirected.matrix <- function(.data) {
  if (is_twomode(.data)) {
    .data
  } else ((.data + t(.data)) > 0) * 1
}

#' @export
to_undirected.data.frame <- function(.data) {
  as_edgelist(to_undirected(as_igraph(.data)))
}

#' @rdname manip_deformat
#' @importFrom dplyr filter select
#' @export
to_unweighted <- function(.data, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(.data, threshold = 1) {
  if(is_weighted(.data)){
    edges <- weight <- NULL
    .data %>% activate(edges) %>% 
      dplyr::filter(weight >= threshold) %>% 
      dplyr::select(-c(weight))
  } else .data
}

#' @export
to_unweighted.igraph <- function(.data, threshold = 1) {
    as_igraph(to_unweighted(as_tidygraph(.data), threshold))
}

#' @export
to_unweighted.network <- function(.data, threshold = 1) {
  as_network(to_unweighted(as_tidygraph(.data), threshold))
}

#' @export
to_unweighted.matrix <- function(.data, threshold = 1) {
  (.data >= threshold)*1
}

#' @export
to_unweighted.data.frame <- function(.data, threshold = 1) {
  if(is_edgelist(.data)) .data[,1:2]
  else snet_abort("Not an edgelist")
}

#' @rdname manip_deformat
#' @param keep In the case of a signed network, whether to retain
#'   the "positive" or "negative" ties.
#' @importFrom igraph delete_edges E delete_edge_attr
#' @export
to_unsigned <- function(.data, 
                        keep = c("positive", "negative")) UseMethod("to_unsigned")

#' @export
to_unsigned.matrix <- function(.data, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- .data
  if(keep == "positive"){
    out[out < 0] <- 0
  } else if (keep == "negative"){
    out[out > 0] <- 0
    out <- abs(out)
  } else snet_abort("Indicate whether 'positive' or 'negative' ties should be kept.")
  out
}

#' @export
to_unsigned.data.frame <- function(.data, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- .data
  if(is_signed(.data)){
    if(keep == "positive"){
      out$sign[out$sign < 0] <- 0
    } else if (keep == "negative"){
      out$sign[out$sign > 0] <- 0
      out$sign <- out$sign(out)
    } else snet_abort("Indicate whether 'positive' or 'negative' ties should be kept.")
  }
  out
}

#' @export
to_unsigned.tbl_graph <- function(.data, 
                                  keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- to_unsigned(as_igraph(.data), keep = keep)
  as_tidygraph(out)
}

#' @export
to_unsigned.igraph <- function(.data, 
                               keep = c("positive", "negative")){
  if (is_signed(.data)) {
    keep <- match.arg(keep)
    if (keep == "positive") {
      out <- igraph::delete_edges(.data, 
                                  which(igraph::E(.data)$sign < 0))
    } else {
      out <- igraph::delete_edges(.data, 
                                  which(igraph::E(.data)$sign > 0))
    }
    out <- igraph::delete_edge_attr(out, "sign")
    out
  } else .data
}

#' @export
to_unsigned.network <- function(.data,
                                keep = c("positive", "negative")){
  as_network(to_unsigned(as_igraph(.data)))
}

#' @rdname manip_deformat
#' @importFrom igraph simplify
#' @export
to_simplex <- function(.data) UseMethod("to_simplex")

#' @export
to_simplex.tbl_graph <- function(.data) {
  as_tidygraph(to_simplex(as_igraph(.data)))
}

#' @export
to_simplex.igraph <- function(.data) {
  igraph::simplify(.data)
}

#' @export
to_simplex.matrix <- function(.data) {
  out <- .data
  diag(out) <- 0
  out
}

#' @export
to_simplex.data.frame <- function(.data) {
  out <- .data[.data$from != .data$to,]
  out
}

#' @export
to_simplex.network <- function(.data) {
  as_network(to_simplex(as_igraph(.data)))
}

#' @rdname manip_deformat
#' @param tie Character string naming a tie attribute to retain from a graph.
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#'   E edge_attr_names
#' @examples
#' as_tidygraph(create_filled(5)) %>%
#'   mutate_ties(type = sample(c("friend", "enemy"), 10, replace = TRUE)) %>%
#'   to_uniplex("friend")
#' @export
to_uniplex <- function(.data, tie) UseMethod("to_uniplex")

#' @export
to_uniplex.tbl_graph <- function(.data, tie){
  type <- NULL
  out <- dplyr::filter(.data = tidygraph::activate(.data, "edges"), 
                       type == tie) %>% dplyr::select(-type)
  if(is_signed(out) && all(tie_signs(out)==1)) out <- out %>% dplyr::select(-sign)
  if(is_weighted(out) && all(tie_weights(out)==1)) out <- out %>% dplyr::select(-weight)
  if(is_longitudinal(out) && length(unique(tie_attribute(out, "wave")))==1) out <- out %>% dplyr::select(-wave)
  tidygraph::activate(out, "nodes")
}

#' @export
to_uniplex.igraph <- function(.data, tie){
  as_igraph(to_uniplex(as_tidygraph(.data), tie))
}

#' @export
to_uniplex.network <- function(.data, tie){
  as_network(to_uniplex(as_igraph(.data), tie))
}

#' @export
to_uniplex.data.frame <- function(.data, tie){
  as_edgelist(to_uniplex(as_igraph(.data), tie))
}

#' @export
to_uniplex.matrix <- function(.data, tie){
  as_matrix(to_uniplex(as_igraph(.data), tie))
}

# Reformatting ####

#' Modifying network formats
#' 
#' @description
#'   These functions reformat manynet-consistent data.
#' 
#'   - `to_acyclic()` reformats network data to an acyclic graph.
#'   - `to_anti()` reformats network data into its complement, where only ties _not_ present in the original network
#'   are included in the new network.
#'   - `to_redirected()` reformats the direction of directed network data, flipping any existing direction.
#'   - `to_reciprocated()` reformats directed network data such that every directed tie is reciprocated.
#' 
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   |                | data.frame| igraph| matrix| network| tbl_graph|
#'   |:---------------|----------:|------:|------:|-------:|---------:|
#'   |to_acyclic      |          1|      1|      1|       1|         1|
#'   |to_anti         |          1|      1|      1|       1|         1|
#'   |to_reciprocated |          1|      1|      1|       1|         1|
#'   |to_redirected   |          1|      1|      1|       1|         1|
#' @name manip_reformat
#' @family modifications
#' @inheritParams mark_is
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
NULL

#' @rdname manip_reformat
#' @importFrom igraph as_directed feedback_arc_set
#' @export
to_acyclic <- function(.data) UseMethod("to_acyclic")

#' @export
to_acyclic.igraph <- function(.data) {
  if(is_directed(.data)){
    delete_ties(.data, igraph::feedback_arc_set(.data))
  } else igraph::as_directed(.data, mode = "acyclic")
}

#' @export
to_acyclic.tbl_graph <- function(.data) {
  as_tidygraph(to_acyclic(as_igraph(.data)))
}

#' @export
to_acyclic.matrix <- function(.data) {
  as_matrix(to_acyclic(as_igraph(.data)))
}

#' @export
to_acyclic.data.frame <- function(.data) {
  as_edgelist(to_acyclic(as_igraph(.data)))
}

#' @export
to_acyclic.network <- function(.data) {
  as_network(to_acyclic(as_igraph(.data)))
}

#' @rdname manip_reformat
#' @importFrom igraph complementer
#' @examples
#' to_anti(ison_southern_women)
#' @export
to_anti <- function(.data) UseMethod("to_anti")

#' @export
to_anti.matrix <- function(.data){
  matrix(1, nrow(.data), ncol(.data)) - .data
}

#' @export
to_anti.data.frame <- function(.data){
  as_edgelist.matrix(to_anti.matrix(as_matrix(.data)))
}

#' @export
to_anti.igraph <- function(.data){
  if(is_twomode(.data)){
    as_igraph(to_anti.matrix(as_matrix(.data)))
  } else {
    igraph::complementer(as_igraph(.data), 
                         loops = is_complex(.data))
  }
}

#' @export
to_anti.tbl_graph <- function(.data){
  if(is_twomode(.data)){
    as_tidygraph(to_anti.matrix(as_matrix(.data)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(.data), 
                                      loops = is_complex(.data)))
  }
}

#' @export
to_anti.network <- function(.data){
  as_network(to_anti(as_igraph(.data)))
}

#' @describeIn manip_reformat Returns an object that has any edge direction transposed,
#'   or flipped, so that senders become receivers and receivers become senders.
#'   This essentially has no effect on undirected networks or reciprocated ties.
#' @importFrom igraph reverse_edges
#' @importFrom tidygraph reroute
#' @export
to_redirected <- function(.data) UseMethod("to_redirected")

#' @export
to_redirected.tbl_graph <- function(.data) {
  as_tidygraph(to_redirected.igraph(.data))
}

#' @export
to_redirected.igraph <- function(.data) {
  igraph::reverse_edges(.data)
}

#' @export
to_redirected.data.frame <- function(.data) {
  out <- .data
  out$from <- .data$to
  out$to <- .data$from
  out
}

#' @export
to_redirected.matrix <- function(.data) {
  t(.data)
}

#' @export
to_redirected.network <- function(.data) {
  as_network(to_redirected(as_igraph(.data)))
}

#' @describeIn manip_reformat Returns an object where all ties are reciprocated.
#' @importFrom igraph as_directed
#' @export
to_reciprocated <- function(.data) UseMethod("to_reciprocated")

#' @export
to_reciprocated.igraph <- function(.data) {
  igraph::as_directed(.data, mode = "mutual")
}

#' @export
to_reciprocated.tbl_graph <- function(.data) {
  as_tidygraph(to_reciprocated(as_igraph(.data)))
}

#' @export
to_reciprocated.matrix <- function(.data) {
  .data + t(.data)
}

#' @export
to_reciprocated.network <- function(.data) {
  as_network(to_reciprocated(as_igraph(.data)))
}

#' @export
to_reciprocated.data.frame <- function(.data) {
  as_edgelist(to_reciprocated(as_igraph(.data)))
}

# Formatting ####

#' Modifying network formats
#' 
#' @description
#'   These functions add some format to manynet-consistent data.
#' 
#'   - `to_directed()` reformats undirected network data to a directed network.
#'   - `to_redirected()` reformats the direction of directed network data, flipping any existing direction.
#'   - `to_reciprocated()` reformats directed network data such that every directed tie is reciprocated.
#'   - `to_acyclic()` reformats network data to an acyclic graph.
#'   - `to_named()` reformats unlabelled network data to labelled network data 
#'   from a vector of names or random baby names.
#'   - `to_signed()` reformats unsigned network data to signed network data
#'   with signs from a mark vector or at random.
#' 
#'   If the format condition is not met,
#'   for example `to_undirected()` is used on a network that is already undirected,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   |                | data.frame| igraph| matrix| network| tbl_graph|
#'   |:---------------|----------:|------:|------:|-------:|---------:|
#'   |to_acyclic      |          1|      1|      1|       1|         1|
#'   |to_directed     |          1|      1|      1|       1|         1|
#'   |to_named        |          1|      1|      1|       1|         1|
#'   |to_reciprocated |          1|      1|      1|       1|         1|
#'   |to_redirected   |          1|      1|      1|       1|         1|
#'   |to_simplex      |          0|      1|      1|       0|         1|
#' @name manip_preformat
#' @family modifications
#' @inheritParams mark_is
#' @param names Character vector of the node names. NULL by default.
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
NULL

#' @rdname manip_preformat
#' @param names Character vector of the node names. NULL by default.
#' @importFrom dplyr mutate
#' @importFrom igraph vcount V
#' @export
to_named <- function(.data, names = NULL) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(.data, names = NULL) {
  if (!is.null(names)) {
    out <- .data %>% mutate(name = names)
  } else {
    n <- net_nodes(.data)
    out <- .data %>%
      mutate(name = .get_babynames(n))
  }
  out
}

#' @export
to_named.igraph <- function(.data, names = NULL) {
  if (!is.null(names)) {
    igraph::V(.data)$name  <- names
  } else {
    igraph::V(.data)$name  <- .get_babynames(net_nodes(.data))
  }
  .data
}

#' @export
to_named.data.frame <- function(.data, names = NULL) {
  if (!is.null(names)) {
    .data[,1]  <- names[as.numeric(.data[,1])]
    .data[,2]  <- names[as.numeric(.data[,2])]
  } else {
    .data[,1]  <- .get_babynames(net_nodes(.data))[as.numeric(.data[,1])]
    .data[,2]  <- .get_babynames(net_nodes(.data))[as.numeric(.data[,2])]
  }
  .data
}

#' @export
to_named.matrix <- function(.data, names = NULL) {
  if(is.null(names)) names <- .get_babynames(net_nodes(.data))
  if(is_twomode(.data)){
    rownames(.data)  <- names[seq_len(nrow(.data))]
    colnames(.data)  <- names[(nrow(.data)+1):length(names)]
  } else {
    rownames(.data)  <- names
    colnames(.data)  <- names
  }
  .data
}

#' @export
to_named.network <- function(.data, names = NULL) {
  as_network(to_named(as_igraph(.data), names))
}

.get_babynames <- function(n){
  indic <- seq(from=1, length.out=n) %% 26
  indic[indic == 0] <- 26
  snet_info("Assigning alphabetic baby names at random.")
  # table(stringr::str_extract(manynet:::baby_names, "^."))
  vapply(indic, 
         function(x){
           let <- LETTERS[x]
           sample(baby_names[startsWith(baby_names, let)], 1)
         }, FUN.VALUE = character(1))
}

#' @rdname manip_reformat 
#' @importFrom igraph as.directed
#' @export
to_directed <- function(.data) UseMethod("to_directed")

#' @export
to_directed.igraph <- function(.data) {
  if(!is_directed.igraph(.data)){
    snet_info("Directions are assigned to existing ties at random.")
    igraph::as_directed(.data, mode = "random")
  } else .data
}

#' @export
to_directed.tbl_graph <- function(.data) {
  as_tidygraph(to_directed(as_igraph(.data)))
}

#' @export
to_directed.matrix <- function(.data) {
  as_matrix(to_directed(as_igraph(.data)))
}

#' @export
to_directed.network <- function(.data) {
  as_network(to_directed(as_igraph(.data)))
}

#' @export
to_directed.data.frame <- function(.data) {
  as_edgelist(to_directed(as_igraph(.data)))
}

#' @rdname manip_preformat
#' @param mark A mark (logical vector) the length of the ties in the network.
#' @export
to_signed <- function(.data, mark = NULL) UseMethod("to_signed")

#' @export
to_signed.matrix <- function(.data, mark = NULL){
  if(is.null(mark)){
    out <- ifelse(stats::runif(length(.data))>=0.5, .data, -.data)  
    snet_info("Since no mark given, signs are generated by splitting",
              "a uniform distribution.")
  } else out <- ifelse(mark, .data[.data!=0], -.data[.data!=0])
  if(is_labelled(.data)){
    out <- matrix(out, nrow(.data), ncol(.data), 
                  dimnames = list(rownames(.data),colnames(.data)))
  } else out <- matrix(out, nrow(.data), ncol(.data))
  out
}

#' @export
to_signed.data.frame <- function(.data, mark = NULL){
  if(is.null(mark)) mark <- stats::runif(nrow(.data))>=0.5
  out <- data.frame(.data, sign = ifelse(mark, 1, -1))
  dplyr::tibble(out)
}

#' @export
to_signed.tbl_graph <- function(.data, mark = NULL){
  if(is.null(mark)){
    ties <- net_ties(.data)
    snet_info("Since no mark given, signs are generated by splitting",
              "a uniform distribution.")
    .data %>% mutate_ties(sign = ifelse(stats::runif(ties)>=0.5, 1, -1))
  } else .data %>% mutate_ties(sign = ifelse(mark, 1, -1))
}

#' @export
to_signed.igraph <- function(.data, mark = NULL){
  as_igraph(to_signed.tbl_graph(as_tidygraph(.data), mark = mark))
}

#' @export
to_signed.network <- function(.data, mark = NULL){
  as_network(to_signed.tbl_graph(as_tidygraph(.data), mark = mark))
}

#' @rdname manip_preformat
#' @param measure A numeric vector (measure) that will be added as the tie
#'   weights to the network.
#'   If this is NULL, then the tie weights will be drawn from a 
#'   Poisson distribution with \eqn{\lambda = 4}.
#' @export
to_weighted <- function(.data, measure = NULL) UseMethod("to_weighted")

#' @export
to_weighted.tbl_graph <- function(.data, measure = NULL){
  if(is.null(measure)){
    measure <- stats::rpois(net_ties(.data), lambda = 4)
    snet_info("Since no measure values given, weights are generated from",
              "a Poisson distribution with lambda = 4.")
  }
  .data %>% mutate_ties(weight = measure)
}

#' @export
to_weighted.igraph <- function(.data, measure = NULL){
  as_igraph(to_weighted.tbl_graph(as_tidygraph(.data), measure = measure))
}

#' @export
to_weighted.network <- function(.data, measure = NULL){
  as_network(to_weighted.tbl_graph(as_tidygraph(.data), measure = measure))
}

# Levelling ####

#' Modifying network levels
#' 
#' @description
#'   These functions reformat the levels in manynet-consistent network data.
#' 
#'   - `to_onemode()` reformats two-mode network data into one-mode network data by simply removing the nodeset 'type' information.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`.
#'   - `to_twomode()` reformats one-mode network data into two-mode network data, using a mark to distinguish the two sets of nodes.
#'   - `to_multilevel()` reformats two-mode network data into multimodal network data, which allows for more levels and ties within modes.
#' 
#'   If the format condition is not met,
#'   for example `to_onemode()` is used on a network that is already one-mode,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   |              | igraph| matrix| network| tbl_graph|
#'   |:-------------|------:|------:|-------:|---------:|
#'   |to_multilevel |      1|      1|       0|         1|
#'   |to_onemode    |      1|      1|       0|         1|
#'   |to_twomode    |      1|      0|       1|         1|
#' @name manip_levels
#' @family modifications
#' @inheritParams mark_is
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
NULL

#' @rdname manip_levels
#' @importFrom igraph delete_vertex_attr vertex_attr_names
#' @export
to_onemode <- function(.data) UseMethod("to_onemode")

#' @export
to_onemode.matrix <- function(.data) {
  if (is_twomode(.data)){
    .data <- rbind(cbind(matrix(0, nrow(.data), nrow(.data)), .data),
                    cbind(t(.data), matrix(0, ncol(.data), ncol(.data))))
    colnames(.data) <- rownames(.data)
  }
  .data
}

#' @export
to_onemode.tbl_graph <- function(.data) {
  as_tidygraph(to_onemode(as_igraph(.data)))
}

#' @export
to_onemode.igraph <- function(.data) {
  if ("type" %in% igraph::vertex_attr_names(.data)) 
    .data <- igraph::delete_vertex_attr(.data, "type")
  .data
}

#' @rdname manip_levels
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph V
#' @export
to_twomode <- function(.data, mark) UseMethod("to_twomode")

#' @export
to_twomode.igraph <- function(.data, mark){
  igraph::V(.data)$type <- mark
  to_undirected(.data)
}

#' @export
to_twomode.tbl_graph <- function(.data, mark){
  as_tidygraph(to_twomode.igraph(.data, mark))
}

#' @export
to_twomode.network <- function(.data, mark){
  as_network(to_twomode(as_igraph(.data), mark), twomode = TRUE)
}

#' @rdname manip_levels 
#' @importFrom igraph V delete_vertex_attr
#' @export
to_multilevel <- function(.data) UseMethod("to_multilevel")

#' @export
to_multilevel.tbl_graph <- function(.data) {
  as_tidygraph(to_multilevel(as_igraph(.data)))
}

#' @export
to_multilevel.igraph <- function(.data) {
  if(is_twomode(.data)){
    igraph::V(.data)$lvl <- ifelse(igraph::V(.data)$type, 2, 1)
    .data <- igraph::delete_vertex_attr(.data, "type")
  }
  .data
}

#' @export
to_multilevel.matrix <- function(.data) {
  top <- cbind(matrix(0, nrow(.data), nrow(.data)), .data)
  bottom <- cbind(t(.data), matrix(0, ncol(.data), ncol(.data)))
  out <- rbind(top, bottom)
  colnames(out) <- rownames(out)
  out
}

