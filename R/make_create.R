# Explicit ####

#' Making networks with explicit ties
#'
#' @description
#'   This function creates a network from a vector of explicitly named nodes 
#'   and ties between them.
#'   `create_explicit()` largely wraps `igraph::graph_from_literal()`,
#'   but will also accept character input and not just a formula,
#'   and will never simplify the result.
#'   
#'   Ties are indicated by `-`, and directed ties (arcs)
#'   require `+` at either or both ends. 
#'   Ties are separated by commas, and isolates can be added as
#'   an additional, unlinked node after the comma within the formula.
#'   Sets of nodes can be linked to other sets of nodes through use of
#'   a semi-colon.
#'   See the example for a demonstration.
#' @name make_explicit
#' @family makes
#' @param ... Arguments passed on to `{igraph}`.
#' @importFrom igraph make_graph
#' @examples
#'   create_explicit(A -+ B, B -+ C, A +-+ C, D, E:F:G-+A, E:F+-+G:H)
#' @export
create_explicit <- function(...){
  if(is.symbol(as.list(match.call())[-1][[1]])){
    mf <- stats::reformulate(...)
    mf[[1]] <- NULL
  } else mf <- as.list(match.call())[-1]
  f <- function(x) {
    if (is.call(x)) {
      return(list(as.character(x[[1]]), lapply(x[-1], f)))
    }
    else return(NULL)
  }
  ops <- unlist(lapply(mf, f))
  if (all(ops %in% c("-", ":"))) {
    directed <- FALSE
  }
  else if (all(ops %in% c("-", "+", ":"))) {
    directed <- TRUE
  }
  else {
    cli::cli_abort("Invalid operator in formula")
  }
  f <- function(x) {
    if (is.call(x)) {
      if (length(x) == 3) {
        return(list(f(x[[2]]), op = as.character(x[[1]]), 
                    f(x[[3]])))
      }
      else {
        return(list(op = as.character(x[[1]]), f(x[[2]])))
      }
    }
    else {
      return(c(sym = as.character(x)))
    }
  }
  ret <- lapply(mf, function(x) unlist(f(x)))
  v <- unique(unlist(lapply(ret, function(x) {
    x[names(x) == "sym"]
  })))
  ret <- lapply(ret, function(x) {
    res <- list()
    for (i in seq(along.with = x)) {
      if (x[i] == ":" && names(x)[i] == "op") {
      }
      else if (i > 1 && x[i - 1] == ":" && names(x)[i - 
                                                    1] == "op") {
        res[[length(res)]] <- c(res[[length(res)]], unname(x[i]))
      }
      else {
        res <- c(res, x[i])
      }
    }
    res
  })
  edges <- numeric()
  for (i in seq(along.with = ret)) {
    prev.sym <- character()
    lhead <- rhead <- character()
    for (j in seq(along.with = ret[[i]])) {
      act <- ret[[i]][[j]]
      if (names(ret[[i]])[j] == "op") {
        if (length(lhead) == 0) {
          lhead <- rhead <- act
        }
        else {
          rhead <- act
        }
      }
      else if (names(ret[[i]])[j] == "sym") {
        for (ps in prev.sym) {
          for (ps2 in act) {
            if (lhead == "+") {
              edges <- c(edges, unname(c(ps2, ps)))
            }
            if (!directed || rhead == "+") {
              edges <- c(edges, unname(c(ps, ps2)))
            }
          }
        }
        lhead <- rhead <- character()
        prev.sym <- act
      }
    }
  }
  ids <- seq(along.with = v)
  names(ids) <- v
  res <- igraph::make_graph(unname(ids[edges]), 
                            n = length(v), directed = directed)
  res <- igraph::set_vertex_attr(res, "name", value = v)
  as_tidygraph(res)
}

# Collections ####

#' Making ego networks through interviewing
#'
#' @description
#'   This function creates an ego network through interactive interview questions.
#'   Note that it only creates a simplex, directed network.
#' @param max_alters The maximum number of alters to collect.
#'   By default infinity, but many name generators will expect a maximum of
#'   e.g. 5 alters to be named.
#' @param roster A vector of node names to offer as potential alters for ego.
#' @param interpreter Logical. If TRUE, then it will ask for which attributes
#'   to collect and give prompts for each attribute for each node in the network.
#'   By default FALSE.
#' @param interrelater Logical. If TRUE, then it will ask for the contacts from
#'   each of the alters perspectives too.
#' @name make_ego
#' @family makes
#' @export
create_ego <- function(max_alters = Inf,
                       roster = NULL,
                       interpreter = FALSE,
                       interrelater = FALSE){
  cli::cli_text("What is ego's name?")
  ego <- readline()
  cli::cli_text("What is the relationship you are collecting? Name it in the singular, e.g. 'friendship'")
  ties <- readline()
  # cli::cli_text("Is this a weighted network?")
  # weighted <- q_yes()
  alters <- vector()
  if(!is.null(roster)){
    for (alt in roster){
      cli::cli_text("Is {ego} connected by {ties} to {alt}?")
      alters <- c(alters, q_yes())
    }
    alters <- roster[alters]
  } else {
    repeat{
      cli::cli_text("Please name a contact:")
      alters <- c(alters, readline())
      if(length(alters) == max_alters){
        cli::cli_alert_info("{.code max_alters} reached.")
        break
      }
      if (q_yes("Are these all the contacts?")) break
    }
  }
  out <- as_tidygraph(as.data.frame(cbind(ego, alters)))
  if(interpreter){
    attr <- vector()
    repeat{
      cli::cli_text("Please name an attribute you are collecting, or press [Enter] to continue.")
      attr <- c(attr, readline())
      if (attr[length(attr)]==""){
        attr <- attr[-length(attr)]
        break
      } 
    }
    if(length(attr)>0){
      for(att in attr){
        values <- vector()
        for (alt in c(ego, alters)){
          cli::cli_text("What value does {alt} have for {att}:")
          values <- c(values, readline())
        }
        out <- add_node_attribute(out, att, values)
      }
    }
  }
  if(interrelater){
    for(alt in alters){
      others <- setdiff(c(ego,alters), alt)
      extra <- vector()
      for(oth in others){
        cli::cli_text("Is {alt} connected by {ties} to {oth}?")
        extra <- c(extra, q_yes())
      }
      # cat(c(rbind(alt, others[extra])))
      out <- add_ties(out, c(rbind(alt, others[extra])))
    }
  }
  out <- add_info(out, ties = ties, 
                  collection = "Interview",
                  year = format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))
  out
}

q_yes <- function(msg = NULL){
  if(!is.null(msg)) cli::cli_text(msg)
  out <- readline()
  if(is.logical(out)) return(out)
  if(out=="") return(FALSE)
  choices <- c("yes","no","true","false")
  out <- c(TRUE,FALSE,TRUE,FALSE)[pmatch(tolower(out), tolower(choices))]
  out
}

# Defined ####

#' Making networks with defined structures
#'
#' @description
#'   These functions create networks with particular structural properties.
#'   
#'   - `create_empty()` creates an empty network without any ties.
#'   - `create_filled()` creates a filled network with every possible tie realised.
#'   - `create_ring()` creates a ring or chord network where each nodes'
#'   neighbours form a clique.
#'   - `create_star()` creates a network with a maximally central node.
#'   - `create_tree()` creates a network with successive branches.
#'   - `create_lattice()` creates a network that forms a regular tiling.
#'   - `create_components()` creates a network that clusters nodes into separate components.
#'   - `create_core()` creates a network in which a certain proportion of 'core' nodes
#'   are densely tied to each other, and the rest peripheral, tied only to the core.
#'   - `create_degree()` creates a network with a given (out/in)degree sequence,
#'   which can also be used to create k-regular networks.
#'
#'   These functions can create either one-mode or two-mode networks.
#'   To create a one-mode network, pass the main argument `n` a single integer,
#'   indicating the number of nodes in the network.
#'   To create a two-mode network, pass `n` a vector of \emph{two} integers,
#'   where the first integer indicates the number of nodes in the first mode,
#'   and the second integer indicates the number of nodes in the second mode.
#'   As an alternative, an existing network can be provided to `n`
#'   and the number of modes, nodes, and directedness will be inferred.
#' @name make_create
#' @family makes
#' @seealso [as]
#' @param n Given:
#'   \itemize{
#'   \item A single integer, e.g. `n = 10`,
#'   a one-mode network will be created.
#'   \item A vector of two integers, e.g. `n = c(5,10)`,
#'   a two-mode network will be created.
#'   \item A manynet-compatible object,
#'   a network of the same dimensions will be created.
#'   }
#' @param directed Logical whether the graph should be directed.
#'   By default `directed = FALSE`.
#'   If the opposite direction is desired, 
#'   use `to_redirected()` on the output of these functions.
#' @param width Integer specifying the width of the ring,
#'   breadth of the branches, or maximum extent of the neighbourbood.
#' @param membership A vector of partition membership as integers.
#'   If left as `NULL` (the default), nodes in each mode will be
#'   assigned to two, equally sized partitions.
#' @return By default a `tbl_graph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, or `as_network()`.
#'   
#'   By default, all networks are created as undirected.
#'   This can be overruled with the argument `directed = TRUE`.
#'   This will return a directed network in which the arcs are
#'   out-facing or equivalent.
#'   This direction can be swapped using `to_redirected()`.
#'   In two-mode networks, the directed argument is ignored.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_biadjacency_matrix
NULL

#' @rdname make_create 
#' @examples
#' create_empty(10)
#' @export
create_empty <- function(n, directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    out <- as_igraph(out, twomode = TRUE)
  }
  if (!directed) out <- to_undirected(out)
  as_tidygraph(out)
}

#' @rdname make_create 
#' @examples
#' create_filled(10)
#' @export
create_filled <- function(n, directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(1, n, n)
    diag(out) <- 0
    out <- igraph::graph_from_adjacency_matrix(out, ifelse(directed, "directed",
                                                           "undirected"))
  } else if (length(n) == 2) {
    out <- matrix(1, n[1], n[2])
    out <- as_igraph(out, twomode = TRUE)
  }
  as_tidygraph(out)
}

#' @rdname make_create 
#' @param ... Additional arguments passed on to `igraph::make_ring()`.
#' @examples
#' create_ring(8, width = 2)
#' @export
create_ring <- function(n, directed = FALSE, width = 1, ...) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    if (width == 1) {
     out <- igraph::make_ring(n, directed, ...)
    } else {
      out <- w <- as_matrix(igraph::make_ring(n, directed, ...))
      for (i in 1:(width - 1)) {
        w <- roll_over(w)
        out <- out + w
      }
      diag(out) <- 0
      out[out > 1] <- 1
      out <- igraph::graph_from_adjacency_matrix(out, ifelse(directed,
                                                             "directed",
                                                             "undirected"))
    }
  } else if (length(n) == 2) {
    mat <- matrix(0, n[1], n[2])
    diag(mat) <- 1
    while (any(rowSums(mat) == 0)) {
      top <- mat[rowSums(mat) == 1, ]
      bot <- mat[rowSums(mat) == 0, ]
      diag(bot) <- 1
      mat <- rbind(top, bot)
    }
    while (any(colSums(mat) == 0)) {
      left <- mat[, colSums(mat) == 1]
      right <- mat[, colSums(mat) == 0]
      diag(right) <- 1
      mat <- cbind(left, right)
    }
    for (i in 1:(width)) {
      w <- roll_over(mat)
      mat <- mat + w
    }
    mat[mat > 1] <- 1
    out <- as_igraph(mat, twomode = TRUE)
  }
  as_tidygraph(out)
}

#' @rdname make_create 
#' @importFrom igraph graph_from_adjacency_matrix graph_from_biadjacency_matrix
#'   make_star
#' @examples
#' create_star(12)
#' @export
create_star <- function(n,
                        directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- igraph::make_star(n, mode = ifelse(directed, "out", "undirected"))
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    if (directed) {
      out[1, ] <- 1
    } else {
      out[, 1] <- 1
    }
    out <- as_igraph(out, twomode = TRUE)
  }
  as_tidygraph(out)
}

#' @rdname make_create 
#' @importFrom igraph make_tree
#' @examples
#' create_tree(c(7,8))
#' @export
create_tree <- function(n,
                        directed = FALSE,
                        width = 2) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 2) {
    if(which.min(n) == 2){
      n1 <- n[1]
      n2 <- n[2]
    } else {
      n1 <- n[2]
      n2 <- n[1]
    }
    out <- matrix(0, n1, n2)
    avail1 <- seq.int(n1)
    avail2 <- seq.int(n2)
    on1 <- 1
    avail1 <- setdiff(avail1, on1)
    while (length(avail1) > 0 & length(avail2) > 0) {
      on2 <- vector()
      for (i in on1) {
        new <- avail2[seq.int(width)]
        out[i, new] <- 1
        on2 <- c(on2, new)
        avail2 <- setdiff(avail2, new)
      }
      on1 <- vector()
      for (j in on2) {
        new <- avail1[seq.int(width)]
        out[new, j] <- 1
        on1 <- c(on1, new)
        avail1 <- setdiff(avail1, new)
      }
    }
    if(which.min(n) == 1) out <- t(out)
    as_tidygraph(out, twomode = TRUE)
  } else {
    as_tidygraph(igraph::make_tree(sum(n), children = width,
                                   mode = ifelse(directed, "out",
                                                 "undirected")))
  }
}

#' @rdname make_create 
#' @section Lattice graphs:
#'   `create_lattice()` creates both two-dimensional grid and triangular
#'   lattices with as even dimensions as possible.
#'   When the `width` parameter is set to 4, nodes cannot have (in or out)
#'   degrees larger than 4.
#'   This creates regular square grid lattices where possible.
#'   Such a network is bipartite, that is partitionable into two types that are
#'   not adjacent to any of their own type.
#'   If the number of nodes is a prime number, it will only return a chain
#'   (a single dimensional lattice).
#'
#'   A `width` parameter of 8 creates a network where the maximum degree of any
#'   nodes is 8.
#'   This can create a triangular mesh lattice or a Queen's move lattice,
#'   depending on the dimensions.
#'   A `width` parameter of 12 creates a network where the maximum degree of
#'   any nodes is 12.
#'   Prime numbers of nodes will return a chain.
#' @importFrom igraph make_lattice
#' @examples
#' create_lattice(12, width = 4)
#' @export
create_lattice <- function(n,
                           directed = FALSE,
                           width = 8) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    divs <- divisors(n)
    if ((length(divs) %% 2) == 0) {
      dims <- c(divs[length(divs) / 2], divs[length(divs) / 2 + 1])
    } else dims <- c(stats::median(divs), stats::median(divs))
    if (width == 8) {
      nei1.5 <- as_matrix(igraph::make_lattice(dims, nei = 2, 
                                               directed = directed))
      for (i in 1:(prod(dims)-2)) {
        nei1.5[i,i+2] <- 0
        if(i+dims[1]*2<=prod(dims))
          nei1.5[i,i+dims[1]*2] <- 0
      }
      if (!directed)
        nei1.5[lower.tri(nei1.5)] <- t(nei1.5)[lower.tri(nei1.5)]
      as_tidygraph(nei1.5)
    } else if (width == 12) {
      as_tidygraph(igraph::make_lattice(dims, nei = 2, directed = directed))
    } else if (width == 4) {
      as_tidygraph(igraph::make_lattice(dims, nei = 1, directed = directed))
    } else cli::cli_abort("`max_neighbourhood` expected to be 4, 8, or 12")
  } else {
    divs1 <- divisors(n[1])
    divs2 <- divisors(n[2])
    # divs1 <- divs1[-c(1, length(divs1))]
    # divs2 <- divs2[-c(1, length(divs2))]
    divs1 <- intersect(divs1, divs2)
    divs2 <- intersect(divs2, divs1)
    # divs1 <- intersect(divs1, c(divs2+1, divs2-1))
    # divs2 <- intersect(divs2, c(divs1+1, divs1-1))
    mat <- matrix(0, n[1], n[2])
    diag(mat) <- 1
    w <- roll_over(mat)
    mat <- mat + w
    mat[lower.tri(mat)] <- 0
    out <- mat[rowSums(mat) ==2,]
    out <- do.call(rbind, replicate(nrow(mat)/nrow(out), out, simplify=FALSE))
    as_tidygraph(out)
  }
}

# #' @describeIn create Creates a honeycomb-style, isometric, or triangular
# #'   grid/mesh lattice graph of the given dimensions with ties to nodes up
# #'   to a maximum width.
# #' @importFrom igraph make_lattice
# #' @examples
# #' reate_mesh(5)
# #' @export
# create_mesh <- function(n,
#                         directed = FALSE, 
#                         width = 8) {
  # offset_divisors <- function(x){
  #   y <- seq_len(x)
  #   y[ x%%y == 0 ]
  # }
  # 
  # if(length(n)== 1){
  #   divs <- offset_divisors(n)
    # if((length(divs) %% 2) == 0){
    #   dims <- c(divs[length(divs)/2], divs[length(divs)/2+1])
    # } else dims <- c(stats::median(divs), stats::median(divs))
    # if(width == 8){
    #   nei1.5 <- as_matrix(igraph::make_lattice(dims, nei = 2, 
    #                                            directed = directed))
    #   for(i in 1:(prod(dims)-2)){
    #     nei1.5[i,i+2] <- 0
    #     if(i+dims[1]*2<=prod(dims))
    #       nei1.5[i,i+dims[1]*2] <- 0
    #   }
    #   if(!directed)
    #     nei1.5[lower.tri(nei1.5)] <- t(nei1.5)[lower.tri(nei1.5)]
    #   as_igraph(nei1.5)
    # } else if (width == 12){
    #   igraph::make_lattice(dims, nei = 2, directed = directed)
    # } else if (width == 4){
    #   igraph::make_lattice(dims, nei = 1, directed = directed)
    # } else cli::cli_abort("`max_neighbourhood` expected to be 4, 8, or 12")
#   }
# }

#' @rdname make_create 
#' @examples
#' create_components(10, membership = c(1,1,1,2,2,2,3,3,3,3))
#' @export
create_components <- function(n, directed = FALSE, membership = NULL) {
  directed <- infer_directed(n, directed)
  membership <- infer_membership(n, membership)
  n <- infer_n(n)
  if (length(n) == 1) {
    out <- matrix(0, n, n)
    for (x in unique(membership)) out[membership == x, membership == x] <- 1
    diag(out) <- 0
    if(directed) out[lower.tri(out)] <- 0
    out <- as_igraph(out)
  } else if (length(n) == 2) {
    out <- matrix(0, n[1], n[2])
    for (x in unique(membership)) out[membership[1:n[1]] == x,
                                     membership[(n[1]+1):length(membership)] ==
                                       x] <- 1
    out <- as_igraph(out, twomode = TRUE)
  }
  as_tidygraph(out)
}

#' @rdname make_create 
#' @param outdegree Numeric scalar or vector indicating the 
#'   desired outdegree distribution.
#'   By default NULL and is required.
#'   If `n` is an existing network object and the outdegree is not specified, 
#'   then the outdegree distribution will be inferred from that of the network.
#'   Note that a scalar (single number) will result in a k-regular graph.
#' @param indegree Numeric vector indicating the desired indegree distribution.
#'   By default NULL but not required unless a directed network is desired.
#'   If `n` is an existing directed network object and the indegree is not specified, 
#'   then the indegree distribution will be inferred from that of the network.
#' @importFrom igraph realize_degseq realize_bipartite_degseq
#' @examples
#' create_degree(10, outdegree = rep(1:5, 2))
#' @export
create_degree <- function(n, outdegree = NULL, indegree = NULL) {
  directed <- infer_directed(n, !is.null(indegree))
  outdegree <- infer_outdegree(n, outdegree)
  indegree <- infer_indegree(n, indegree)
  n <- infer_n(n)
  if (length(n) == 1) {
    if(!directed){
      if(length(outdegree)==1) outdegree <- rep(outdegree, n)
      stopifnot(n == length(outdegree))
      out <- igraph::realize_degseq(outdegree)
    } else {
      if(length(outdegree)==1) outdegree <- rep(outdegree, n)
      if(length(indegree)==1) indegree <- rep(indegree, n)
      stopifnot(n == length(outdegree), n == length(indegree))
      out <- igraph::realize_degseq(outdegree, indegree)
    }
  } else if (length(n) == 2) {
    if(length(outdegree)==1) outdegree <- rep(outdegree, n[1])
    if(length(indegree)==1) indegree <- rep(indegree, n[2])
    stopifnot(n[1] == length(outdegree), n[2] == length(indegree))
    out <- igraph::realize_bipartite_degseq(outdegree, indegree)
  }
  as_tidygraph(out)
}

#' @rdname make_create
#' @param mark A logical vector the length of the nodes in the network.
#'   This can be created by, among other things, any `node_is_*()` function.
#' @examples
#' create_core(6)
#' @export
create_core <- function(n, directed = FALSE, mark = NULL) {
  directed <- infer_directed(n, directed)
  mark <- infer_membership(n, mark)
  if(!is.numeric(mark)) mark <- as.numeric(as.factor(mark))
  n <- infer_n(n)
  if (length(n) > 1) {
    mat <- matrix(0, n[1], n[2])
    mat[mark[1:n[1]] == 1,] <- 1
    mat[, mark[(n[1] + 1):length(mark)] == 1] <- 1
    as_tidygraph(mat, twomode = TRUE)
  } else {
    mat <- matrix(0, n, n)
    mat[mark == 1,] <- 1
    mat[, mark == 1] <- 1
    diag(mat) <- 0
    if(directed) mat[lower.tri(mat)] <- 0
    as_tidygraph(mat)
  }
}

# #' @rdname create
# #' @details Creates a nested two-mode network.
# #' Will construct an affiliation matrix,
# #' with decreasing fill across n2.
# #' @importFrom tidygraph as_tbl_graph
# #' @importFrom igraph graph_from_biadjacency_matrix
# #' @examples
# #' create_nest(10, 12)
# #' @export
# create_nest <- function(n1, n2,
#                         as = c("tidygraph", "igraph", "matrix")) {
#   as <- match.arg(as)
#   out <- matrix(0, n1, n2)
#   out[(row(out) - col(out)) >= 0] <- 1
#   if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
#   if(as == "igraph") out <- igraph::graph_from_biadjacency_matrix(out)
#   out
# }
# 
# # mat.dist <- matrix(0,5,3)
# # mat.dist[1:2,1] <- 1
# # mat.dist[,2] <- 1
# # mat.dist[4:5,3] <- 1
# #
# # mat.hier <- matrix(0,4,4)
# # mat.hier[1:4,1] <- 1
# # mat.hier[1:2,2] <- 1
# # mat.hier[1:2,3] <- 1
# # mat.hier[3:4,4] <- 1

# Helper functions ------------------

infer_dims <- function(object) {
  if(is_twomode(object) &
     any(grepl("type", igraph::vertex_attr_names(as_igraph(object))))) {
    c(sum(!igraph::V(as_igraph(object))$type),
      sum(igraph::V(as_igraph(object))$type))
  } else {
    igraph::vcount(as_igraph(object))
  }
}

infer_n <- function(n) {
  if (is_manynet(n)) n <- infer_dims(n)
  if (length(n) > 2) cli::cli_abort(paste("`n` should be a single integer for a one-mode network or",
                             "a vector of two integers for a two-mode network."))
  n
}

infer_directed <- function(n, directed) {
  if(is_manynet(n)) directed <- is_directed(n)
  directed
}

infer_outdegree <- function(n, outdegree) {
  if (is.null(outdegree) && is_manynet(n)){
    outdegree <- node_deg(n, direction = "out")
    if(is_twomode(n)) outdegree <- outdegree[1:net_dims(n)[1]]
  } 
  outdegree
}

infer_indegree <- function(n, indegree) {
  if (is.null(indegree) && is_manynet(n)){
    indegree <- node_deg(n, direction = "in")
    if(is_twomode(n)) indegree <- indegree[(net_dims(n)[1]+1):sum(net_dims(n))]
  } 
  indegree
}

infer_membership <- function(n, membership) {
  if (is.null(membership)) {
    if(is_manynet(n)) n <- infer_n(n)
    if (length(n) > 1) {
      membership <- c(sort(abs(seq_len(n[1]) %% 2 -2)), 
                      sort(abs(seq_len(n[2]) %% 2 -2)))
    } else membership <- sort(abs(seq_len(n) %% 2 -2))
  }
  membership
}

divisors <- function(x) {
  y <- seq_len(x)
  y[ x%%y == 0 ]
}

roll_over <- function(w) {
  cbind(w[, ncol(w)], w[, 1:(ncol(w) - 1)])
}

