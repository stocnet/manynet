# Explicit ####

#' Making networks with explicit ties
#' @name make_explicit
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
      list(as.character(x[[1]]), lapply(x[-1], f))
    }
    else NULL
  }
  ops <- unlist(lapply(mf, f))
  if (all(ops %in% c("-", ":"))) {
    directed <- FALSE
  }
  else if (all(ops %in% c("-", "+", ":"))) {
    directed <- TRUE
  }
  else {
    snet_abort("Invalid operator in formula")
  }
  f <- function(x) {
    if (is.call(x)) {
      if (length(x) == 3) {
        list(f(x[[2]]), op = as.character(x[[1]]), 
             f(x[[3]]))
      }
      else {
        list(op = as.character(x[[1]]), f(x[[2]]))
      }
    }
    else {
      c(sym = as.character(x))
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
#'   - `create_cycle()` creates a network in which all the nodes form
#'   a single closed chain.
#'   - `create_wheel()` creates a network in which a single dominant node
#'   is tied to all the nodes in a cycle.
#'   - `create_windmill()` creates a network in which several cliques,
#'   or blades, share a single hub node.
#'   Unless a blade `width` is given, the number of blades is balanced
#'   against their size.
#'
#'   Some of these structures are constrained in two-mode networks.
#'   Since ties in two-mode networks can only run between the modes,
#'   a two-mode cycle must alternate between them, and so can only be as long
#'   as twice the number of nodes in the smaller mode.
#'   Similarly, the rim of a two-mode wheel alternates between the modes,
#'   and its hub, drawn from the first mode, can only be tied to the
#'   second mode's rim nodes.
#'   The hub of a two-mode windmill is also drawn from the first mode,
#'   and is tied to every blade node in the second mode,
#'   while each other first-mode node is tied to its own blade.
#'   Where `n` is larger than such a structure can accommodate,
#'   the largest such structure is created and the surplus nodes are
#'   added as isolates, with a message.
#'   This is also the case for windmills whose blades cannot be equally sized.
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
#'   breadth of the branches, number of nodes in each blade of a windmill,
#'   or maximum extent of the neighbourbood.
#' @param membership A vector of partition membership as integers.
#'   If left as `NULL` (the default), nodes in each mode will be
#'   assigned to two, equally sized partitions.
#' @return By default a `tbl_graph` object is returned,
#'   but this can be coerced into other types of objects
#'   using `as_edgelist()`, `as_matrix()`,
#'   `as_tidygraph()`, or `as_network()`.
#'   `create_windmill()` returns a `stocnet` object.
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
  as_tidygraph(out) |> 
    add_info(name = "Empty network")
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
  as_tidygraph(out) |> 
    add_info(name = "Filled network")
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
  as_tidygraph(out) |> 
    add_info(name = "Ring network")
}

#' @rdname make_create 
#' @importFrom igraph graph_from_adjacency_matrix graph_from_biadjacency_matrix make_star
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
  as_tidygraph(out) |> 
    add_info(name = "Star network")
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
      as_tidygraph(nei1.5) |> 
        add_info(name = "Lattice network")
    } else if (width == 12) {
      as_tidygraph(igraph::make_lattice(dims, nei = 2, directed = directed)) |> 
        add_info(name = "Lattice network")
    } else if (width == 4) {
      as_tidygraph(igraph::make_lattice(dims, nei = 1, directed = directed)) |> 
        add_info(name = "Lattice network")
    } else snet_abort("`max_neighbourhood` expected to be 4, 8, or 12")
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
    as_tidygraph(out) |> 
      add_info(name = "Lattice network")
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
    # } else snet_abort("`max_neighbourhood` expected to be 4, 8, or 12")
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
#'   Note that a scalar (single number) will result in a k-regular graph.
#'   By default NULL.
#'   If `n` is an existing network object and the outdegree is not specified,
#'   then the outdegree distribution will be inferred from that of the network.
#'   If only the indegree is specified, then in one-mode networks the outdegree
#'   will mirror it, and in two-mode networks the same number of ties will be
#'   spread as evenly as possible across the nodes in the first mode.
#'   If neither is specified, the sparsest connected structure is created:
#'   a cycle in one-mode networks, and in two-mode networks one in which the
#'   larger mode is 1-regular.
#' @param indegree Numeric vector indicating the desired indegree distribution.
#'   By default NULL but not required unless a directed network is desired.
#'   If `n` is an existing directed network object and the indegree is not specified,
#'   then the indegree distribution will be inferred from that of the network.
#'   Otherwise it is filled in from the outdegree as described above.
#' @importFrom igraph realize_degseq realize_bipartite_degseq
#' @examples
#' create_degree(10, outdegree = rep(1:5, 2))
#' create_degree(10)
#' create_degree(c(6,4))
#' @export
create_degree <- function(n, outdegree = NULL, indegree = NULL) {
  directed <- infer_directed(n, !is.null(indegree))
  outdegree <- infer_outdegree(n, outdegree)
  indegree <- infer_indegree(n, indegree)
  n <- infer_n(n)
  degs <- default_degree(n, outdegree, indegree, directed)
  outdegree <- degs$outdegree
  indegree <- degs$indegree
  if (length(n) == 1) {
    outdegree <- recycle_degree(outdegree, n, "outdegree")
    if(!directed){
      out <- igraph::realize_degseq(outdegree)
    } else {
      indegree <- recycle_degree(indegree, n, "indegree")
      out <- igraph::realize_degseq(outdegree, indegree)
    }
  } else if (length(n) == 2) {
    outdegree <- recycle_degree(outdegree, n[1], "outdegree")
    indegree <- recycle_degree(indegree, n[2], "indegree")
    out <- igraph::realize_bipartite_degseq(outdegree, indegree)
    # igraph assigns the first degree sequence to type TRUE,
    # but the first mode is expected to be type FALSE here
    out <- igraph::set_vertex_attr(out, "type", value = !igraph::V(out)$type)
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

#' @rdname make_create
#' @examples
#'   create_windmill(7)
#'   create_windmill(7, width = 2)
#'   create_windmill(c(4,6))
#' @export
create_windmill <- function(n, directed = FALSE, width = NULL) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  # A blade holds a whole number of nodes, and at least one.
  if (!is.null(width) &&
      (!is.numeric(width) || length(width) != 1 || is.na(width) ||
       width < 1 || width != round(width)))
    snet_abort("`width` must be a single whole number of at least 1.")
  if (length(n) == 1) {
    if (n < 3) {
      snet_abort("At least 3 nodes required to form a windmill graph.")
    }
    # Each blade is a clique of `w` nodes that, together with the hub, form
    # a complete subgraph; `k` blades share the hub.
    m <- n - 1
    if (!is.null(width)) {
      w <- width
      k <- m %/% w
      if (k < 1) snet_abort("`width` can be at most {m} for a windmill of {n} nodes.")
    } else {
      pairs <- expand.grid(k = 2:max(2, m %/% 2), w = 2:max(2, m %/% 2))
      pairs <- pairs[pairs$k * pairs$w <= m, ]
      if (nrow(pairs) == 0) {
        k <- 1
        w <- m
      } else {
        # Use as many nodes as possible, then balance blade count against
        # the size of the clique each blade forms with the hub.
        pairs <- pairs[order(-pairs$k * pairs$w,
                             abs(pairs$k - (pairs$w + 1))), ]
        k <- pairs$k[1]
        w <- pairs$w[1]
      }
    }
    out <- matrix(0, n, n)
    out[1, 2:(1 + k*w)] <- out[2:(1 + k*w), 1] <- 1
    for (i in seq_len(k)) {
      blade <- (2 + (i - 1)*w):(1 + i*w)
      out[blade, blade] <- 1
    }
    diag(out) <- 0
    if (directed) out[lower.tri(out)] <- 0
    extra <- m - k*w
    if (extra > 0) {
      snet_info("Windmills require the nodes other than the hub to divide",
                "evenly into blades, so a windmill of {1 + k*w} nodes has been created",
                "and the remaining {extra} node{?s} added as isolate{?s}.")
    }
    out <- igraph::graph_from_adjacency_matrix(out, ifelse(directed, "directed",
                                                           "undirected"))
  } else if (length(n) == 2) {
    a <- n[1]
    b <- n[2]
    if (a < 2 || b < 1) {
      snet_abort("Two-mode windmills require at least two nodes in the first mode",
                 "and one node in the second mode.")
    }
    # The hub, drawn from the first mode, is tied to every blade node in the
    # second mode, and each other first-mode node to its own blade of `w`.
    if (!is.null(width)) {
      w <- width
      k <- min(a - 1, b %/% w)
      if (k < 1) snet_abort("`width` can be at most {b} for a second mode of {b} nodes.")
    } else {
      ks <- seq_len(min(a - 1, b))
      used <- ks + ks * (b %/% ks)
      k <- max(ks[used == max(used)])
      w <- b %/% k
    }
    extra <- (a - 1 - k) + (b - k*w)
    if (extra > 0) {
      snet_info("Two-mode windmills require the second mode to divide evenly",
                "across the first mode's blades, so a windmill of {1 + k + k*w} nodes",
                "has been created and the remaining {extra} node{?s} added as isolate{?s}.")
    }
    blades <- rep(seq_len(k), each = w)
    edges <- rbind(1, a + seq_along(blades), blades + 1, a + seq_along(blades))
    out <- igraph::make_empty_graph(n = a + b, directed = FALSE) |>
      igraph::add_edges(as.vector(edges)) |>
      igraph::set_vertex_attr("type", value = rep(c(FALSE, TRUE), c(a, b)))
  } else snet_abort("Argument 'n' must be a scalar or vector of length 2.")
  as_stocnet(out) |>
    add_info(name = "Windmill network")
}

#' @rdname make_create
#' @examples
#'   create_cycle(6)
#'   create_cycle(c(4,6))
#' @export
create_cycle <- function(n, directed = FALSE){
  n <- infer_n(n)
  # Helper: Create edge list for unimodal cycle
  unimodal_cycle <- function(n_nodes) {
    # Constructed directed explicitly rather than via an edgelist: an edgelist
    # carries no directedness flag, so coercing one infers direction from
    # reciprocity, and a directed cycle has none.
    as_tidygraph(igraph::make_ring(n_nodes, directed = TRUE))
  }
  
  # Helper: Create edge list for bimodal cycle
  bimodal_cycle <- function(n_modes) {
    a <- n_modes[1]
    b <- n_modes[2]
    # A two-mode cycle alternates between the modes, so it can only be as long
    # as twice the smaller mode; any surplus nodes are added as isolates.
    m <- min(a, b)
    if(m < 2)
      snet_abort("Two-mode cycles require at least two nodes in each mode.")
    if(a != b){
      extra <- a + b - 2*m
      snet_info("Two-mode cycles require an equal number of nodes in each mode,",
                "so a cycle of {2*m} nodes has been created",
                "and the remaining {extra} node{?s} added as isolate{?s}.")
    }
    edges <- vapply(seq_len(m), function(i)
      c(i, a + i, a + i, if(i < m) i + 1 else 1), numeric(4))
    igraph::make_empty_graph(n = a + b, directed = TRUE) |>
      igraph::add_edges(as.vector(edges)) |>
      igraph::set_vertex_attr("type", value = rep(c(FALSE, TRUE), c(a, b))) |>
      as_tidygraph()
  }
  
  # Main logic
  if (length(n) == 1) {
    # Unimodal cycle
    net <- unimodal_cycle(n)
  } else if (length(n) == 2) {
    # Bimodal cycle
    net <- bimodal_cycle(n)
  } else {
    snet_abort("Argument 'n' must be a scalar or a vector of length 2.")
  }
  if(!directed || length(n) == 2) net <- to_undirected(net)
  net
}

#' @rdname make_create
#' @examples
#'   create_wheel(6)
#'   create_wheel(c(4,6))
#' @export
create_wheel <- function(n, directed = FALSE) {
  directed <- infer_directed(n, directed)
  n <- infer_n(n)
  if (length(n) == 1) {
    if (n < 4) {
      snet_abort("At least 4 nodes required to form a wheel graph.")
    }
    center_node <- 1
    rim_nodes <- 2:n
    # Create the cycle (rim)
    rim_cycle <- cbind(rim_nodes, c(rim_nodes[-1], rim_nodes[1]))
    # Connect center to each rim node
    center_edges <- cbind(center_node, rim_nodes)
    edges <- rbind(rim_cycle, center_edges)
    out <- igraph::graph_from_edgelist(edges, directed = directed)
  } else if (length(n) == 2) {
    a <- n[1]
    b <- n[2]
    # Since rim nodes cannot be adjacent to both their neighbouring rim nodes
    # and the hub, the rim alternates between the modes and the hub, drawn from
    # the first mode, is tied to the second mode's rim nodes only.
    # The rim can thus only be as long as twice the smaller of the first mode
    # (excluding the hub) and the second mode; surplus nodes are isolates.
    m <- min(a - 1, b)
    if (m < 2)
      snet_abort("Two-mode wheels require at least three nodes in the first mode",
                 "and two nodes in the second mode.")
    if (a - 1 != b) {
      extra <- a + b - (2*m + 1)
      snet_info("Two-mode wheels require one more node in the first mode",
                "than in the second, so a wheel of {2*m+1} nodes has been created",
                "and the remaining {extra} node{?s} added as isolate{?s}.")
    }
    rim <- vapply(seq_len(m), function(i)
      c(i + 1, a + i, a + i, if(i < m) i + 2 else 2), numeric(4))
    spokes <- rbind(1, a + seq_len(m))
    out <- igraph::make_empty_graph(n = a + b, directed = FALSE) |>
      igraph::add_edges(c(as.vector(rim), as.vector(spokes))) |>
      igraph::set_vertex_attr("type", value = rep(c(FALSE, TRUE), c(a, b)))
  } else snet_abort("Argument 'n' must be a scalar or vector of length 2.")
  as_tidygraph(out) |>
    add_info(name = "Wheel network")
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

infer_n <- function(n, data = NULL) {
  if (is.null(n)) {
    if (is.null(data))
      snet_abort(paste("Please provide either a network (to `.data`)",
                       "or a number of nodes (to `n`)."))
    n <- infer_dims(data)
  } else if (is_manynet(n)) n <- infer_dims(n)
  if (length(n) > 2) snet_abort(paste("`n` should be a single integer for a one-mode network or",
                             "a vector of two integers for a two-mode network."))
  n
}

infer_directed <- function(n, directed) {
  if(is_manynet(n)) directed <- is_directed(n)
  directed
}

infer_signed <- function(n, signed) {
  if(is_manynet(n)) signed <- is_signed(n)
  signed
}

infer_outdegree <- function(n, outdegree) {
  if (is.null(outdegree) && is_manynet(n)){
    outdegree <- .node_deg(n, direction = "out")
    if(is_twomode(n)) outdegree <- outdegree[1:mode_nodes(n)[1]]
  }
  outdegree
}

infer_indegree <- function(n, indegree) {
  if (is.null(indegree) && is_manynet(n)){
    indegree <- .node_deg(n, direction = "in")
    if(is_twomode(n)) indegree <- indegree[(mode_nodes(n)[1]+1):sum(mode_nodes(n))]
  }
  indegree
}

recycle_degree <- function(degree, k, name) {
  if (length(degree) == 1) degree <- rep(degree, k)
  if (length(degree) != k)
    snet_abort("`{name}` should be a single number or a vector of length {k},",
               "but a vector of length {length(degree)} was given.")
  degree
}

# Spreads `ties` ties as evenly as possible across `k` nodes,
# e.g. 6 ties across 4 nodes gives c(2,2,1,1).
spread_degree <- function(ties, k) {
  rep(ties %/% k, k) + c(rep(1, ties %% k), rep(0, k - ties %% k))
}

# Where one or both degree sequences are missing, defaults to the sparsest
# connected structure available: a cycle for one-mode networks, and for
# two-mode networks one in which the larger mode is 1-regular and the
# smaller mode's ties are spread as evenly as possible.
default_degree <- function(n, outdegree, indegree, directed) {
  if (!is.null(outdegree) && !is.null(indegree))
    return(list(outdegree = outdegree, indegree = indegree))
  neither <- is.null(outdegree) && is.null(indegree)
  if (length(n) == 1) {
    if (neither) {
      deg <- if (n < 2) 0 else if (directed || n < 3) 1 else 2
      snet_info("No degree sequence given,",
                "so creating a {deg}-regular network.")
      outdegree <- indegree <- deg
    } else if (is.null(outdegree)) outdegree <- indegree else
      indegree <- outdegree
  } else if (length(n) == 2) {
    ties <- if (neither) max(n) else
      if (is.null(indegree)) sum(rep(outdegree, length.out = n[1])) else
        sum(rep(indegree, length.out = n[2]))
    if (neither)
      snet_info("No degree sequence given, so spreading {ties} tie{?s}",
                "as evenly as possible across the nodes in each mode.")
    if (is.null(outdegree)) outdegree <- spread_degree(ties, n[1])
    if (is.null(indegree)) indegree <- spread_degree(ties, n[2])
  }
  list(outdegree = outdegree, indegree = indegree)
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

